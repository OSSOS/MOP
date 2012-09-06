#!/usr/bin/env perl
##
## Compute the mag difference between the identified USNO stars and 
## the output of PHOT.
##
## USAGE: deltamag.pl 
##	  	      --usno file1.usno
##		      --phot file2.usno.mag
##
use Getopt::Long;
use Pod::Usage; 

## Get the commandline options.  --program  --input are
## required. pod2usage will parse the material after
## __END__ into a usage command if the options aren't
## Specified

my $help = 0;
my $usno = "";
my $phot = "";

$result = GetOptions('h|help|?' => \$help,
		     'usno=s' => \$usno,
		     'phot=s' => \$phot
		     ) || pod2usage();

pod2usage() if $help;
pod2usage() if !$usno;
pod2usage() if !$phot;

##
## Let the user know what's going on.
##

#print STDERR "Computing magnitude difference between $file1 and $file2\n";

##
## Get the transformation for the first file
##


open(MASTER,"< $phot") or 
		die "Can't open star phot file $phot. $!\n";

my $dmag=0;
my $n=0;
my $merr;
my $dum;

FILE1: while ( <MASTER> ) {
   ($x1, $y1, $mag1, $merr ) = split(' '); 
   open (SLAVE,"< $usno") or 
		die "Can't open star photometry file $usno. $!\n";
   FILE2: while (<SLAVE> ) {
      ($x2, $y2, $mag2, $dum ) = split(' '); 
      if ( sqrt( ($x1-$x2)**2 + ($y1 - $y2)**2) < 3 ) {
         $dmag += $mag1 - $mag2 ;
	 $derr += $merr**2;
	 $n++;
	 last FILE2;
      }
   }
   close(SLAVE);
   #print "$x1,$y1 i<==> $x2,$y2 $mag1 $mag2 --> $dmag\n";
}
if ( $n > 0 ) { 
   $dmag = $dmag/$n;
   $derr = sqrt($derr)/$n;
} else {
   $dmag = 9.999;
}
print "$dmag $derr \n";

#printf("%8.3f +/- %8.4f (based on %d stars)\n",$dmag,$derr,$n);

