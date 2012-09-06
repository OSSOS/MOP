#!/usr/bin/env perl
##
## Compute the mag difference between three sets of photometry. 
## using the shifts file to allign the lists of stars.
##
## USAGE: checkwcs.pl file1.xy file2.xy

use Getopt::Long;
use Pod::Usage; 

my $help = 0;
my $file1 = 0;
my $file2 = 0;
my $verbose=0;
my $outfile='';

$result = GetOptions('h|help|?' => \$help,
		     'v|verbose' => \$verbose,
		     'file1=s' => \$file1,
		     'output=s' => \$outfile,
		     'file2=s' => \$file2 ) || pod2usage();

pod2usage() if $help;
pod2usage() if !$file1 || !$file2;

my $files;
$files[0]=$file1;
$files[1]=$file2;

##
## Let the user know what's going on.
##
open(OUTFILE, ">$outfile");
printf OUTFILE "# %6s %8s %8s %8s %9s %9s %8s %8s\n","X_0","Y_0","X_USNO","Y_USNO","RA_USNO","DEC_USNO","delta-X","delta-Y";

open(FILE1,"<$file1");
my $count=0;
my $stddev=0;
while ( <FILE1> ) {
    next if ( /^#/) ;
    @v = split(' ');
    open(FILE2,"<$file2");
    my $minrad=5;
    my $rad=0;
    my $match=0;
    while (<FILE2>) { 
	next if ( /^#/ ) ;
	@t=split(' ');
	$dx=$v[0]-$t[0];
	$dy=$v[1]-$t[1];
	$rad = sqrt($dx**2 + $dy**2);
	if ( $rad < $minrad ) { 
	    $match=1;
	    @tt=@t;
	    $tt[4]=$dx*5.111155979E-05*3600.0;
	    $tt[5]=$dy*5.111155979E-05*3600.0;
	    $minrad=$rad;
	}
    }
    close(FILE2);
    if ($match) {
	$count++;
	$stddev+=$minrad;
	printf OUTFILE "%8.3f %8.3f %8.3f %8.3f %9.5f %9.5f %8.3f %8.3f\n", $v[0],$v[1],$tt[0],$tt[1],$tt[2],$tt[3],$tt[4],$tt[5];
    }
}
$rms=-1;
if ( $count > 0 ) {
     $rms=0.183*$stddev/$count;
} 
printf OUTFILE "#RMS: %8.3f arcsec based on %8.3f USNO sources\n", $rms,$count;

