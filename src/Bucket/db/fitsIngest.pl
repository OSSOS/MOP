#!/usr/cadc/misc/bin/perl

### use gethead to read the key words from a fits file and then 
### store those key words in the exposure table

use Bucket::MOP;

#use Term::ReadKey;
use Getopt::Long;
use Pod::Usage;
use warnings;
use strict;
use Carp;
use File::Basename;

my $help = 0;
my $debug = 0;

GetOptions ('h|help|?' => \$help,
            'd|debug' => \$debug,
            );

pod2usage() if ( $help);

##
## set up a has array to get the desired info from the frame.
## hash has the structure "dbcol" => "fits keyword"
##
## currently the correlation between the image header and the exposure
## table is 1:1

my %dataDictionary = ( "expnum" => "expnum",
		       "chipid" => "extname",
		       "runid" => "runid",
		       "object" => "object",
		       "ra_deg" => "ra_deg",
		       "dec_deg" => "dec_deg",
		       "exptime" => "exptime",
		       "mjdate" => "mjdate",
		       "date_obs" => "date-obs",
		       "utc_obs" => "utc-obs",
		       "filter" => "filter"
		       #"naxis1" => "naxis1",
		       #"naxis2" => "naxis2",
		       #"crval1" => "crval1",
		       #"crval2" => "crval2",
		       #"crpix1" => "crpix1",
		       #"crpix2" => "crpix2",
		       #"cd1_1" => "cd1_1",
		       #"cd1_2" => "cd1_2",
		       #"cd2_2" => "cd2_2",
		       #"cd2_1" => "cd2_1",
		       #"cdelta1" => "cdelt1",
		       #"crota" => "crota",
		       #"cdelta2" => "cdelt2"
 );

### call createTable on each table... no effect if table already exists
### the received table stores the location of the file
my $received = Bucket::MOP->new("received","cfhls","shift+add");
$received->createTable();
### the exposure table stores information about the exposure 
my $exposure = Bucket::MOP->new("exposure","cfhls","shift+add");
$exposure->createTable();
my $wcs = Bucket::MOP->new("wcs","cfhls","shift+add");
$wcs->createTable();



foreach my $file ( @ARGV ) {
    next if ( ! -f $file ) ;
    my %row = ();
    $row{"file_id"}=basename($file);
    $row{"file_id"} =~ s/.fits$//;
    $row{"active"} = $file;
    ##my $archive = `ssh archive find /ss/cfhls/B1on/B1on.proc -name $row{file_id} -print `;
    my $archive = 'not available';
    chomp $archive;
    $row{"archive"} = $archive; 
    print "ingesting $row{file_id}                   \n";
    my $val="___";
    foreach my $key ( keys %dataDictionary ) {
	$val = `gethead -u $file $dataDictionary{$key}`;
	chomp $val;
	if ($val =~ m/___/){
            print "\r FAILED on keyword: $key for $file \n";
	    last ;
	}
	$row{$key}=$val;
    }
    next if ($val =~ m/___/) ;
    my $cmd = "wcshead ".$file;
    ($row{junk}, 
     $row{naxis1}, $row{naxis2}, 
     $row{ctype1}, $row{ctype2}, 
     $row{crval1}, $row{crval2}, 
     $row{radecsys}, 
     $row{crpix1}, $row{crpix2}, 
     $row{cdelta1}, $row{cdelta2}, 
     $row{crota}) = split ' ',`$cmd`;

    ### store the information in the appropriate table
    $exposure->deleteRow("file_id LIKE '$row{file_id}'");
    $wcs->deleteRow("file_id LIKE '$row{file_id}'");
    $received->deleteRow("file_id LIKE '$row{file_id}'");
    my $type = $exposure->selectRows("file_id LIKE '".$row{"file_id"}."'") ? "updateRow" : "insertRow";
    $type="insertRow";
    $exposure->$type(\%row);
    $received->$type(\%row);
    $wcs->$type(\%row);

} 

print "\n Done \n";




