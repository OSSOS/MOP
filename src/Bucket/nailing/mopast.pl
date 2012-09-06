#!/usr/bin/perl -w


use strict;
## Determine RA/DEC location of X/Y using a local solution.
## and eject an MPC format line for this observation


use Getopt::Long;
use Pod::Usage;


my ($image, $xcen, $ycen, $help,$verbose, $name, $root, $quiet, $debug);

my $result = GetOptions('h|help|?' => \$help
			,'f|frame=s' => \$root
			,'v|verbose+' => \$verbose
			,'d|debug' => \$debug
			,'q|quiet' => \$quiet
			,'x|xcen=f' => \$xcen
			,'y|ycen=f' => \$ycen
		        ,'n|name=s' => \$name
			) || pod2usage();

my $obj_name = $name;
my $output = "";
## redirect to null if quiet is on, and turn off debug and verbose
if ( $quiet ) {
    $verbose = 0;
    $debug = 0;
    $output = " 2>>/dev/null ";
}

my $dir = $root;
$dir =~ m\(.*)/\;
$dir = $1;

## strip off the .fits (if its there) 
$root =~ s/.fits$//;

## image has the .fits
$image= "$root.fits";

## need to have run getcenter first (for refined WCS)
if (`gethead  $image WRA | wc -l ` == 0 ) {
    warn "Run getcenter on $image";
exit;
}
my $getcen = `gethead $image GETCEN`;
if ( $getcen =~ m/^$/ ) {
    warn "get_cenphot.pl was not run on $image \n ";
exit;
}

if ( $getcen != 1 ) {
    warn "# get_cenphot.pl failed on $image.\n"; 
exit;
}





##read the mag file and choose the 10 closest USNO stars to this x/y
my $cmd;
if ( 0 == 1 ) {
my @line;
open(MAG,"<$root.immatch");
open(NEAR,">$root.near");

while (my $line=<MAG>) {
    if ( $line =~ m/^#/ ) {
	 next;
	 print NEAR $line ;
     }
$line[$#line+1]{content} = $line;
my ($x, $y, @other) =  split ' ',$line;
$line[$#line]{delta} = sqrt(($x - $xcen)**2 + ($y-$ycen)**2);
}
close(MAG);

my $count = 0;
my @sorted = sort { $a->{delta} <=> $b->{delta} } @line;
foreach my $line ( @sorted ) {
    $count++;
    print NEAR $line->{content};
    last if ($count == 20 );
}
close(NEAR);
my $res = `center.csh $image $root.near $root.cen`;
warn "$res" if ($debug);
## The imwcs program needs the usno catalog to be called usno_stars
## I'm not sure why
unlink ("usno_stars");
$res = `ln -s $root.usno usno_stars`;
warn $res if ($verbose);
my $cmd = "imwcs -h 200 -n 2 -c usno_stars -d $root.$obj_name.usno -o -w $image".$output;
warn "$cmd\n" if ($debug);
}
warn "Localization of WCS turned off.... will fix it later :-)(JJK)\n" if ($debug || $verbose);
#$result = `$cmd`;
#warn "$result\n" if ($verbose);


## get the aperture corrections
my ($apin, $apout, $apcor, $aperr) = ( 5, 18, 0.20 , 9.99) ;
if ( ! -e $root.".apcor" ) {
    warn "Aperture correction doesn't exist? $root.apcor \n using hardwired values\n" if ( !$quiet ) ;
} else { 
    open (APCOR,"< $root.apcor");
    ($apin,$apout,$apcor,$aperr) = split(' ',<APCOR>);
    close(APCOR);
}      
## get the zeropoint for the images
my $zpfile = sprintf("%s/zeropoint.used",$dir);
my $zp=26.0;
warn "trying to use usinging $zpfile" if ( $debug);
if ( -e $zpfile ) { 
    open (ZP,"<$dir/zeropoint.used");
$zp = <ZP>;
close(ZP);
chomp $zp;
}else{
    warn "No ZP file for this frame ($root).  using 26.0";
}
my $insky = $apout+3*$apin;
my $outsky = $insky+10;

## get the date and exposure time from .mopheader
my $head =  $root.".mopheader";
if ( ! -e $head ) {
	`stepZjmp -f $root`;
}
my ($exptime,$MJD) = split(' ',`gethead $root.mopheader EXPTIME MJD-OBSC`);
my ($year,$mon,$day) = split('-',`gethead $root.fits DATE-OBS `);
my ($filter) = `gethead $root.fits FILTER`;
chomp $filter;
chomp ($MJD);
$day = $day + $MJD - int($MJD) ;
my $date = sprintf("%04d %02d %08.5f",$year,$mon,$day);
warn "$root $date \n" if ($debug);

## determine the magnitude of the object
$cmd = "dophot.pl --file $root";
$cmd .= " --ap $apin";
$cmd .= " --insky $insky";
$cmd .= " --outsky $outsky";
$cmd .= " --maxcount 30000";
$cmd .= " --zeropoint $zp";
$cmd .= " --exptime $exptime";
$cmd .= " --xcen $xcen --ycen $ycen";
warn "$cmd\n" if ($debug);
my @dophot = split (' ',`$cmd | grep DOPHOT`);
my($err,$mag,$y_o,$x_o) = (pop @dophot, pop @dophot, pop @dophot, pop @dophot );
warn "Large x/y shift ($xcen,$ycen) -> ($x_o,$y_o)\n" if ( sqrt( ($xcen-$x_o)**2 +($ycen-$y_o)**2 ) > 3 ) ;
my  ($x0,$y0,$check,$ra,$dec) = split(' ',`xy2sky -j -n 2 $root.fits $x_o $y_o `);
warn "$x0  $y0 $check $ra $dec\n" if ($debug);

$mag = $mag - $apcor;

$ra =~ s/:/ /g;
$dec =~ s/:/ /g;
### BRETT's DB constrained MPC format line
my $mpc .= sprintf "  %11.11s  C%16.16s %11.11s %11.11s         %5.1f %1.1s      568\n",
	$obj_name,$date,$ra,$dec,$mag,$filter;

print $mpc;


exit;

__END__

=head1 SYNOPSIS

B<getcenter.pl> -f file.fits

=head1 OPTIONS

=over 8

=item B<-f> 

Name of the fits file to rebuild the astrometric WCS for.  Checks for WRA keyword and skips file if 
that keyword exists.

=back

=head1 USAGE

The B<getcenter.pl> script uses I<sex> to find stars I<IRAF> to centroid them I<get_usno.pl> to find
the USNO stars in the field I<imwcs> to determine the solution and I<immatch> to check if the solution 
is anygood.  The last line of output is the fraction of USNO catalog stars that were found in the field.

=head1 NOTE

There is currently no 'drop out' if the solution is crap.  Should there be? 
