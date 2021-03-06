#!/usr/bin/env perl


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
my ($apin, $apout, $apcor, $aperr) = ( 5, 18, 0.20 , 9.99) ;
if ( ! -e $root.".apcor" ) {
    warn "Aperture correction doesn't exist? $root.apcor \n using hardwired values\n" if ( !$quiet ) ;
} else { 
    open (APCOR,"< $root.apcor");
    ($apin,$apout,$apcor,$aperr) = split(' ',<APCOR>);
    close(APCOR);
}      

## get the zeropoint for the images
my $zpfile = sprintf("%s/zeropoint.used",$wdir);
my $zp=0.0;
warn "trying to use $zpfile \n" if ( $debug);
if ( -e $zpfile ) { 
    open (ZP,"<$wdir/zeropoint.used");
$zp = <ZP>;
close(ZP);
chomp $zp;
}else{
    warn "No ZP file for this frame ($root).  using 26.0\n" if ( !$quiet ) ;
}

my $insky = $apout+3*$apin;
my $outsky = $insky+10;

## get the date and exposure time from .mopheader
my $head =  $root.".mopheader";
if ( ! -e $head ) {
    print STDERR "Can't read $root.mopheader in $wdir... trying to run stepZjmp\n" if ( !$quiet);
	`stepZjmp -f $root`;
}
my ($exptime,$MJD) = split(' ',`gethead $root.mopheader EXPTIME MJD-OBSC`);
my ($year,$mon,$day) = split('-',`gethead $root.fits DATE-OBS `);
my ($filter) = `gethead $root.fits FILTER`;
chomp $filter;
chomp ($MJD);
$day = $day + $MJD - int($MJD) + $exptime/(24.0*3600.0*2.0);
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

my $do_res = MOP_execute("dophot.pl", $cmd);
my ($x_o, $y_o, $mag, $err) = ($xcen,$ycen,0,0);
if ( $Bucket::Control::err ) {
    warn "Photometry failed \n";
} else {
    foreach my $line ( split "\n", $do_res ) {
	if ( $line =~ /DOPHOT/ ) {
	    ### this is the line for dophot that has the photometry
            
	    $line =~ /DOPHOT\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s*/;
	    ($x_o, $y_o, $mag, $err) = ($1,$2,$3,$4);
	    warn "$line \n" if ( $debug ) ;
	    warn ":: $mag :: $apcor\n" if ( $debug ) ;
	    if ( $mag =~ /\d+\.\d*/ ) {
  	      $mag = $mag - $apcor;
	    } else {
	      $mag = 0;
            }
	    ### allow the use of manual center if pixel shift is larger then 3 pixels
	    if ( sqrt( ($xcen-$x_o)**2 +($ycen-$y_o)**2 ) > 3 ) {
		warn "Centroiding shifted object by 3 or pixels\n";
		warn "revert to manual pointing? (y/n)\n";
		my $ans = <STDIN>;
		if ( $ans =~ /y/ ) {
		    ($x_o, $y_o, $mag, $err) = ($xcen,$ycen,0,0);
		}
	    }
	}
    }
}

warn "getting the RA/DEC for $x_o,$y_o\n" if ($debug );
my ($ra,$dec) = xy2skypv($root, $x_o, $y_o);

$ra =~ s/:/ /g;
$dec =~ s/:/ /g;
my $mpc .= sprintf " %-11.11s  C%16.16s %11.11s %11.11s         %5.1f %1.1s      568\n",
	$obj_name,$date,$ra,$dec,$mag,$filter;

print $mpc;
chdir($cdir);

exit 0;




sub xy2sky {
    my $root = shift;
    my $xcen = shift;
    my $ycen = shift;
## call hansastone to get the RA/DEC
    my $tmpxy = $root.".tmpxy";
    open(XY,"> $tmpxy") || die "error opening temporary file $tmpxy \n";
    print XY "$xcen  $ycen \n";
    close(XY);
    warn "$tmpxy has $xcen, $ycen\n";
    ### use hansastron to get the ra/dec with an optional stop at get_cenphot along the way
    my $pltfile = $root.".mkpltsol.usno";
    if (!(-e $pltfile)) { # re-run mkpltsol if not previously successful
        warn "runing mkpltsok... \n";
	my $pltfail = $root.".mkpltsol.FAILED";
	if (-e $pltfail)  {
	    $result = `/bin/rm $pltfail`;
	}
	$result=MOP_execute("runmkplt.pl","-f $root","force");
    } # now  can get back to running hansastone
    warn "Running hansastone \n";
    $result=MOP_execute("hansastone","$root","force");
    if ( $Bucket::Control::err ) {
	warn "Failed while running hansastone\n";
	warn "Should we use the wcs in the header? (y/n) ";
	my $ans=<STDIN>;
	if ( $ans =~ /y/  ) {
	    MOP_execute("wcsUpdate.pl","-f $root --force");
	    #if ( $Bucket::Control::err ) {
	#	die "get_cenphot.pl failed also \n";
	#    }
	    $result=MOP_execute("hansastone"." $root");
	    ### if $bucket::control::err is not 0 then the previous task failed
	    warn "Giving up on $root \n" if ($Bucket::Control::err ) ;
	    exit(0);
	} else {
	    warn "OK, skipping $root \n";
	    exit(0);
	}
    }
    my $astfile = $root.".tmpradec";
    open(RADEC,"<$astfile");
    my $line = <RADEC>;
    my ($ra, $dec) = split ' ',$line;
    close(RADEC);
    
    ### clean up the temperary input files
    unlink($astfile);
    unlink($tmpxy);


### convert the RA/DEC to radians
    $ra = $ra*DD2R;
    $dec = $dec*DD2R;

### get the sting format of these
    slaCr2tf(2,$ra,my $rsign,my @ra);
    slaCr2af(1,$dec,my $dsign,my @dec);
    $ra = sprintf("%02d:%02d:%02d.%02d",@ra);
    $dec = sprintf("%1s%02d:%02d:%02d.%01d",$dsign,@dec);

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
