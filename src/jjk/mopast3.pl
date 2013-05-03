#!/usr/bin/env perl

## Changelog: 
## 200?-2012: JJ.Kavelaars wrote most of this.
## 2012-06-19: M.Alexandersen fixed some isues with manual centroid & mpc_codes

use strict;
## Determine RA/DEC location of X/Y using a local solution.
## and eject an MPC format line for this observation
use Astro::SLA;
use Getopt::Long;
use Pod::Usage;
#use Bucket::MOP;
use Bucket::Control;
use File::Basename;
use File::Temp qw/ tempfile /;;


my ($image, $xcen, $ycen, $help,$verbose, $name, $root, $quiet, $debug, $nocentroid, $nophot, $mpc_code);
my $result = GetOptions( 'h|help|?' => \$help
			,'f|frame=s' => \$root
			,'v|verbose+' => \$verbose
			,'d|debug' => \$debug
			,'q|quiet' => \$quiet
			,'x|xcen=f' => \$xcen
			,'y|ycen=f' => \$ycen
		        ,'n|name=s' => \$name
			,'m|mpc_code=s' => \$mpc_code
			) || pod2usage();

    
### seperate the file from it's location
my $cdir = `pwd` || die "Cann't get the current working directory\n";
my $wdir = dirname($root);
$root = basename($root);
$root =~ s/.fits$//;


### move to the image directory (required by hansastone)
chdir($wdir) || die "Cann't get to the working directory ($wdir) \n";


my $obj_name = $name;
my $output = "";
## redirect to null if quiet is on, and turn off debug and verbose
if ( $quiet ) {
    $verbose = 0;
    $debug = 0;
    $output = " 2>>/dev/null ";
}

### TURN OFF QUIET... TOO DANGEROUS
$quiet=0;

## image has the .fits
$image= "$root.fits";
## get the aperture corrections
my ($apin, $apout, $apcor, $aperr) = ( 5, 18, -99.99 , -99.99) ;
if ( ! -e $root.".apcor" ) {
    warn "Aperture correction doesn't exist? $root.apcor, blanking\n" if ( !$quiet ) ;
} else { 
    open (APCOR,"< $root.apcor");
    ($apin,$apout,$apcor,$aperr) = split(' ',<APCOR>);
    close(APCOR);
}      

## get the zeropoint for the images
## JJK took out the $wdir as I think it's not needed.
## my $zpfile = sprintf("%s/%s.zeropoint.used",$wdir,$root);
my $zpNominal=30.0;
## Use the header keyword value
my $zp="";
$zp=`gethead PHOTZP $root.fits`;
chomp $zp;
warn "Trying to use PHOTZP value" if ( $debug);
if ( $zp=="" ){
   $zp=$zpNominal;
   warn "No PHOTZP for this frame ($root).  using default, $zp" if ( !$quiet ) ;
}

my $insky = $apout+3;
my $outsky = $insky+3;

## get the date and exposure time from .mopheader
my $head =  $root.".mopheader";
if ( ! -e $head ) {
    warn "Can't read $root.mopheader in $wdir... trying to run stepZjmp\n" ;
    my $cmd = "stepZjmp -f $root";
    warn $cmd;
    my $result=`$cmd`;
}
my ($exptime,$MJD) = split(' ',`gethead $root.mopheader EXPTIME MJD-OBSC`);
$exptime=1.0;
my ($year,$mon,$day) = split('-',`gethead $root.fits DATE-OBS `);
my ($filter) = `gethead $root.fits FILTER`;
chomp $filter;
chomp ($MJD);
$day = $day + $MJD - int($MJD) ;
my $date = sprintf("%04d %02d %08.5f",$year,$mon,$day);
warn "$root $date \n" if ( $debug);

## determine the magnitude of the object

my $cmd = "--file $root";
$cmd .= " --ap $apin";
$cmd .= " --insky $insky";
$cmd .= " --outsky $outsky";
$cmd .= " --maxcount 30000";
$cmd .= " --zeropoint $zp";
$cmd .= " --exptime $exptime";
$cmd .= " --xcen $xcen --ycen $ycen";
warn "$cmd" if ( $debug);

my ($x_o, $y_o, $mag, $err) = ($xcen,$ycen,"  ","  ");


## if ( $mpc_code !~ /I/ ) {            # No, always do astrometry! MA
my $do_res = MOP_execute("dophot.pl", $cmd);
if ( $Bucket::Control::err ) {
    warn "Photometry failed \n";
} else {
    foreach my $line ( split "\n", $do_res ) {
      if ( $line =~ /DOPHOT/ ) {
        ### this is the line for dophot that has the photometry
        
        $line =~ /DOPHOT\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s*/;
        ($x_o, $y_o, $mag, $err) = ($1,$2,$3,$4);
        warn ":: $mag :: $apcor\n" if ( $debug ) ;
        if ( $mag =~ /\d+\.\d*/ && $apcor > 0  ) {
          $mag = sprintf("%5.1f",$mag - $apcor);
        } else {
          $filter=" ";
          $mag = "    ";
        }
        ### allow the use of manual center if pixel shift is larger then 3 pixels
        if ( $mpc_code =~ /H/ ) {
	    ($x_o, $y_o, $mag, $err) = ($xcen,$ycen,"   ",0);
	}
        if ( sqrt( ($xcen-$x_o)**2 +($ycen-$y_o)**2 ) > 3 ) {
    	warn "Centroiding shifted object by 3 or pixels\n";
    	warn "$x_o -> $xcen , $y_o -> $ycen\n";
    	warn "revert to manual pointing? (y/n)\n";
    	my $ans = <STDIN>;
    	if ( $ans =~ /y/ ) {
    	    $mpc_code="H";
    	    warn "You should remeasure this with the H option on!";
    	    ($x_o, $y_o, $mag, $err) = ($xcen,$ycen,"   ",0);
    	} else {
    	    if ( $mpc_code =~ /S/ ) { 
    		$mag = "   ";
    	    }
    	    if ( $mpc_code =~ /I/ ) { 
    		$mag = "   ";
    	    }
    	    $mpc_code="M";
	    $mag = "   ";
    	    warn "You should remeasure this with the M option on!";
    	}
        }
      }
    }
}
## }
if ( $mpc_code =~ /S/ ) {
    $mag = "   ";
}
if ( $mpc_code =~ /I/ ) {
    $mag = "   ";
}

my ($ra,$dec) = xy2sky($root, $x_o, $y_o);

#$ra =~ s/:/ /g;
#$dec =~ s/:/ /g;

if ( ! $mag =~ /\d+/ ) {
    $filter = " ";
}
my $mpc = "";
warn "MPC CODE: $mpc_code" if $debug;
if ( $mpc_code =~ /^[SHItMEW]+$/ ) {
  $mpc .= sprintf " %-11.11s %1sC%16.16s %12.12s%12.12s        %5.5s %1.1s      568\n",
	$obj_name,$mpc_code,$date,$ra,$dec,$mag,$filter;
} else {
  $mpc .= sprintf " %-11.11s  C%16.16s %12.12s%12.12s        %5.5s %1.1s      568\n",
	$obj_name,$date,$ra,$dec,$mag,$filter;
}
warn $mpc if $debug ;
print $mpc;
chdir($cdir);

exit 0;




sub xy2sky {
    my $root = shift;
    my $xcen = shift;
    my $ycen = shift;
    my $astfile = $root.".tmpradec";
## call hansastone to get the RA/DEC
    my $tmpxy = $root.".tmpxy";
    open(XY,"> $tmpxy") || die "error opening temporary file $tmpxy \n";
    print XY "$xcen  $ycen \n";
    close(XY);
    `xy2skypv $root".fits" $tmpxy $astfile`;
    my $astfile = $root.".tmpradec";
    open(RADEC,"<$astfile");
    my $line = <RADEC>;
    my ($ra, $dec, $x, $y) = split ' ',$line;
    close(RADEC);
    
    ### clean up the temperary input files
    unlink($astfile);
    unlink($tmpxy);


### convert the RA/DEC to radians
##    $ra = $ra*DD2R;
##    $dec = $dec*DD2R;

### get the sting format of these
    my $rah = int($ra/15.0);
    my $ramf = ($ra/15.0 - $rah)*60.0;
    my $ram = int($ramf);
    my $ras = ($ramf - $ram)*60.0;
    my $sign=1;
    my $dsign="+";
    $dec < 0 ? $sign=-1 : $sign=1;
    $sign < 0 ? $dsign="-" : $dsign="+";
    $dec = $sign*$dec;
    my $decd = int($dec);
    my $decmf = ($dec- $decd)*60.0;
    my $decm = int($decmf);
    my $decs = ($decmf - $decm)*60.0;
    #slaCr2tf(2,$ra,my $rsign,my @ra);
    #slaCr2af(1,$dec,my $dsign,my @dec);
    $ra = sprintf("%02d %02d %06.3f",$rah,$ram,$ras);
    $dec = sprintf("%1s%02d %02d %06.3f",$dsign,$decd,$decm,$decs);

    return ($ra,$dec);
}


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
