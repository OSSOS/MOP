#!/usr/bin/env perl

## perform psf fitting on a list of objects 

use Getopt::Long;
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
my $coofile;
my $psffile;
my $help;
my $insky;
my $outsky;
my $ap;
my $debug;
my $rdnoise=5;
my $gain=1.3;

GetOptions('h|help|?' => \$help,
	   'f|file=s' => \$file,
	   'm|maxcount=f' => \$maxcount,
	   'z|zeropoint=f' => \$zeropoint,
	   'e|exptime=f' => \$exptime,
	   'a|ap=f' => \$ap,
	   'i|insky=f' => \$insky,
	   'o|outsky=f' => \$outsky,
	   'debug' => \$debug,
	   'p|psf=s' => \$psffile,
	   'r|rdnoise=f' => \$rdnoise,
	   'g|gain=f' => \$gain,
	   'c|coo=s' => \$coofile ) || pod2usage(1);

pod2usage(1) if ($help);

## some very basic argu checking
$file = -e $file ? $file : "$file.fits";
-e $file || die "cann't read $file\n";
-e $coofile || die "Cann't read $coofile\n";
-e $psffile || die "Cann't read $psffile\n";

print STDERR "range check\n" if ($debug);
print STDERR "exptime zeropoint x y ap insky  outsky\n" if ($debug) ;
print STDERR "$exptime $zeropoint $x $y $ap $insky $outsky\n" if ( $debug);

($exptime > 0 && 
 $exptime !=0 && 
 $exptime < 1e5 && 
 $zeropoint > 0 && 
 $zeropoint < 100 && 
 $ap < $insky && 
 $ap > 0 && 
 $insky < $outsky && 
 $outsky < 1e5 ) || die "Parameter range bad?\n";

my $curdir = File::Spec->rel2abs(File::Spec->curdir());
print STDERR "working in $curdir\n" if ($debug);

### Here I create the file and then delete it to be sure the 
### temp name is valid
my ($fhm, $magfile) = tempfile(SUFFIX=>'.mag');
close($fhm);
unlink ($magfile);
my ($fhm, $alsfile) = tempfile(SUFFIX=>'.als');
close($fhm);
unlink ($alsfile);

open (IRAF,"| (cd ~/iraf; cl) ");


print IRAF "cd $curdir\n";
print IRAF " ls\n" if ( $debug ) ;
print IRAF "digiphot \n daophot \n";
$outsky = $outsky - $insky;
print IRAF "phot(\"$file\",\"$coofile\",\"$magfile\",verify-,verbose-,photpars.apertures=$ap,photpars.zmag=$zeropoint,datapars.datamin=INDEF,datapars.datamax=$maxcount,datapars.itime=$exptime,centerpars.calgorithm=\"centroid\",fitskypars.annulus=$insky,fitskypars.dannulus=$outsky)\n";
print IRAF "allstar(\"$file\",\"$magfile\",\"$psffile\",\"$alsfile\",default,default,verify-,verbose-,datapars.gain=$gain,datapars.readnoise=$rdnoise,datapars.datamin=INDEF,datapars.datamax=$maxcount,datapars.itime=$exptime)";
print IRAF "txdump(\"$magfile\",\"XCENTER,YCENTER,MAG,MERR\",\"yes\",header-) | scan(line) \n";
print IRAF "print(\"DOPHOT \",line)\n";
print IRAF "logout\n";

close(IRAF);

exit;


__END__

=head1 NAME

B<dophot.pl> - do apperature photometry on a single object in a fits image

=head1 SYNOPSIS

B<dophot.pl> --file image.fits --coo file.coo  \
    --maxcount  20000 --exptime 600 --ap 3.4 --insky 10 \
    --outsky 20 --zeropoint 25.6

=head1 ARGUMENTS

=over 8

=item B<--file>

The image to measure the brightness of the object in.



