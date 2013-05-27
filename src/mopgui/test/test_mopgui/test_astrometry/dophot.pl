#!/usr/bin/env perl

## perform apperature photometry on a single object or list of x/y objects

use Getopt::Long;
use Control;
use Pod::Usage;
use strict;
use File::Path;
use File::Basename;
use File::Spec;
use File::Temp qw/ tempfile tempdir /;

### intialize some variables for usage call
my $file=();
my $maxcount;
my $zeropoint;
my $exptime;
my $x;
my $y;
my $help;
my $insky;
my $outsky;
my $ap;
my $debug;

GetOptions('h|help|?' => \$help,
	   'f|file=s' => \$file,
	   'm|maxcount=f' => \$maxcount,
	   'z|zeropoint=f' => \$zeropoint,
	   'e|exptime=f' => \$exptime,
	   'a|ap=f' => \$ap,
	   'i|insky=f' => \$insky,
	   'o|outsky=f' => \$outsky,
	   'x|xcen=f' => \$x,
	   'debug' => \$debug,
	   'y|ycen=f' => \$y ) || pod2usage(1);

pod2usage(1) if ($help);

## some very basic argu checking
$file = -e $file ? $file : "$file.fits";
-e $file || die "cann't read $file\n";
print STDERR "range check\n" if ($debug);
print STDERR "exptime zeropoint x y ap insky  outsky\n" if ($debug) ;
print STDERR "$exptime $zeropoint $x $y $ap $insky $outsky\n" if ( $debug);

($exptime > 0 && 
 $exptime !=0 && 
 $exptime < 1e5 && 
 $zeropoint >= 0 && 
 $zeropoint < 100 && 
 $x > 0 && 
 $x < 1e5 && 
 $y > 0 && 
 $y < 1e5  && 
 $ap < $insky && 
 $ap > 0 && 
 $insky < $outsky && 
 $outsky < 1e5 ) || die "Parameter range bad?\n";

my $curdir = File::Spec->rel2abs(File::Spec->curdir());
print STDERR "working in $curdir\n" if ($debug);

## create a temp file with the x/y coordinates
my ($fhc, $coofile) = tempfile(SUFFIX=>'.coo');
print STDERR "coordfile $coofile \n" if ($debug);
print $fhc "$x $y \n";
close($fhc);

my ($fhm, $magfile) = tempfile(SUFFIX=>'.mag');
close($fhm);
unlink ($magfile);
open (IRAF,"| (cd ~/iraf; cl -old) ");
print IRAF " pwd\n";
print IRAF " cd $curdir  \n";
print STDERR "$curdir\n" if ( $debug ) ;
print IRAF " ls\n" if ( $debug ) ;
print IRAF "digiphot \n daophot \n";
$outsky = $outsky - $insky;
print IRAF "phot(\"$file\",\"$coofile\",\"$magfile\",verify-,verbose-,photpars.apertures=$ap,photpars.zmag=$zeropoint,datapars.epadu=1.65,datapars.datamin=INDEF,datapars.datamax=$maxcount,datapars.exposur='',datapars.itime=$exptime,centerpars.calgorithm=\"centroid\",fitskypars.annulus=$insky,fitskypars.dannulus=$outsky)\n";
print IRAF "txdump(\"$magfile\",\"XCENTER,YCENTER,MAG,MERR\",\"yes\",header-) | scan(line) \n";
print IRAF "print(\"DOPHOT \",line)\n";
print IRAF "logout\n";

close(IRAF);

unlink $coofile if ( ! $debug ) ;
unlink $magfile if ( ! $debug ) ;

exit;


__END__

=head1 NAME

B<dophot.pl> - do apperature photometry on a single object in a fits image

=head1 SYNOPSIS

B<dophot.pl> --file image.fits --xcen 345.2 --ycen 2343.1 \
    --maxcount  20000 --exptime 600 --ap 3.4 --insky 10 --outsky 20

=head1 ARGUMENTS

=over 8

=item B<--file>

The image to measure the brightness of the object in.



