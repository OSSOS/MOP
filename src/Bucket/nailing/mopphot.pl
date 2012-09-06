#!/opt/perl/bin/perl -w

### do photometry of an object based on the x/y location.  Assumes we're inside a MOP area and gathers the needed info from mopheader

use Getopt::Long;

use  strict;

my $debug=0;
my $help;
my ($xcen, $ycen, $frame , $mopheader);

GetOptions('d|debug' => \$debug,
           'f|frame:s' => \$frame,
	   'x|xcen=f' => \$xcen,
	   'y|ycen=f' => \$ycen,
           'h|help' => \$help);

$frame =~ s/\.fits$//;
-e "$frame.fits" || die "Cann't access $frame\n";
## get the needed header info from mopheader
my $mophead = $frame;

$mopheader = "$mophead.mopheader";
-e $mopheader || die "MOPheader doesn't exist ($mopheader)\n";
my $ap;
($ap = `gethead $mopheader APIN` ) || ( $ap = 4 ) ;
chomp ($ap);
my $insky;
( $insky = `gethead $mopheader APOUT`) || ( $insky = 15 ) ;
chomp ($insky);
$insky = $insky+3*$ap;
my $outsky = $insky+3*$ap;
my $maxcount;
($maxcount = `gethead $mopheader MAXCOUNT`) || ( $maxcount = 20000 ) ;
chomp $maxcount;
my $zp;
($zp = `gethead $mopheader MOP_ZP` ) || ( $zp = 25.6 );
chomp($zp);
my $exptime = `gethead $mopheader exptime`;
chomp($exptime);
my $cmd = "dophot.pl --file $frame --ap $ap --insky $insky --outsky $outsky";
$cmd .= " --maxcount $maxcount --zeropoint $zp --exptime $exptime --xcen $xcen --ycen $ycen";
print "$cmd\n";
my @dophot = split (' ',`$cmd A| grep DOPHOT`);
my $end = $#dophot;
printf "%5.2f %5.2f %5.2f %5.2f\n",$dophot[$end-3],$dophot[$end-2],$dophot[$end-1],$dophot[$end];
