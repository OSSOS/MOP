#!/usr/bin/env perl

### ingest an MPC file given on the command line.  The name of the file is
### used as the name of the OBJECT the 'ID' tag in the file is used as the 
### source name
###


use Bucket::MOP;

use Getopt::Long;
use Pod::Usage;
use Astro::SLA;

use warnings;
use strict;
use Carp;

my $help =0;
my $debug=0;
my $verbose=0;

GetOptions ('h|help|?' => \$help,
	    'v|verbose' => \$verbose,
            'd|debug' => \$debug,
            );

pod2usage() if ( $help);

### get handles for the source and measure tables, create if needed.
####
my $measure = Bucket::MOP->new("measure");
$measure->createTable();
my $source = Bucket::MOP->new("source");
$source->createTable();

### MPC 80 column format specifier
my $MPC_T = "A1A11A2A1A4x1A2x1A8x1A11x1A11x10A4x1A1x6A3";
foreach my $file ( @ARGV ) {
    chomp($file);
    next if ( ! -f $file ) ;
    my %row = ();
    open (MPC,"<$file");
    #print "\n";
    my $reset_file_id=1;
    while (<MPC>) {
        print;
	chomp;
	$row{"file_id"}='unknown' if $reset_file_id;
 	if ( /^\#L\s(\S*)\s/ ) {
	    ## start of a Legacy record so the next 'word' is the file name
	    my $filename=$1;
	    warn "$filename is not a valid CFHT/ls filename\n" 
		unless $filename =~ m/[1-9][0-9]{5}[op][0-3][0-9]/;
	    $row{"file_id"}=$filename;
	    $reset_file_id=0;
	    next;
        }
        ### only use the filename ONCE.. if we got to here then we
	### need to reset the filename after this 'loop'
	$reset_file_id=1;
	next if ( /^\S/ ) ;
	next if ( /^\s*$/ ) ;
	$row{"mpc"} = $_;
	### unpack the MPC line (assumes 80 col format MPC line)
	my @value = (unpack $MPC_T, $_);

	### delete the previously measured lines ID number
	delete 	$row{"measure"};
	## Get the provisional designation for this astrometry line
	$row{"provisional"} = $value[1];
	$row{"provisional"} =~ s/^\s*//;
	$row{"provisional"} =~ s/\s*$//;
	print STDERR "Inserting ",$row{"provisional"},"\r" if ( $verbose) ;
	$row{"observatory"} = $value[11];
	$row{"mag"} = $value[9];
	print $row{"provisional"}."\n";
	$row{"filter"} = $value[10];
	## need to convert the RA/DEC to radians.
	## use the slaDafin routines for this
	my $ptr=1;
	my $status=0;
	my $angle=0;
	#$value[7] =~ s/ /:/g;
	#$value[8] =~ s/ /:/g;
	#print "$value[7], $value[8]\n";
	slaDafin($value[7],$ptr,$angle,$status);
	carp "Bad conversion of $value[7] to RA value \n" if ( $status);
	next if ( $status) ;
	$row{"ra_rad"} = $angle*15.0;

	### conver the DEC to radians
	$status=0;
 	$ptr=1;
	$angle=0;
	slaDafin($value[8],$ptr,$angle,$status);
	carp "Bad conversion of $value[8] to DEC value \n" if ( $status);
	next if ( $status) ;
	$row{"dec_rad"}=$angle;
	#print $row{"ra_rad"}."  ".$row{"dec_rad"}."\n";
	### get the year/mon/iday.fday  and compute mjdate
	my $year=$value[4];
	my $mon=$value[5];
	my $day=$value[6];
	my $iday = int($day);
	my $fday = $day - $iday;
	my $mjdate=0;
	$status=0;
	slaCldj($year,$mon,$iday,$mjdate,$status);
	carp "Failed to convert $year $mon $day to MJD\n" if ($status);
	next if ( $status) ;
	$mjdate += $fday;
	$row{"mjdate"} = $mjdate;

	### select a line from the measure table with the same provisional
	### designation,mjdate and observatory... if that exists then we
	### need to overwrite the entry.
	my $condition = " (observatory=$row{observatory} AND mjdate=$row{mjdate} AND provisional LIKE \'$row{provisional}\' ) || ( observatory=$row{observatory} AND mjdate=$row{mjdate} AND ra_rad=$row{ra_rad} AND dec_rad=$row{dec_rad})";
	$measure->deleteRow($condition);
	$measure->insertRow(\%row);
	### now get the value of the insert ID
	my $result = $measure->selectRows({WHERE => "observatory=$row{observatory} AND mjdate=$row{mjdate} AND provisional LIKE \'$row{provisional}\'"});
	carp "Insert failed?  \n" if ( !$result ) ;
	next if ( $result) ;
	my $newRow = $measure->fetchRow();
	### set the id (measure) of the current row to match the value
	### returned when the row was inserted into the measure table 
	$row{'measure'}=$measure->{'measure'};
	### now insert that into the source table to link these measures
	### to the source designation.
	### Check that this measure doesn't already have an entry in the object table.
	$result = $source->selectRows({WHERE => "measure=$row{measure}"});
	carp "Astrom line already associated with the measure\n" if ( $result);
	$source->insertRow(\%row);
    }
}
