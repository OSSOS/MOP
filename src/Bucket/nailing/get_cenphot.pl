#!/usr/bin/perl -w

## DETERMINE THE Astrometric projection of a CD image and write that to the image header

use Bucket::Control;
use Getopt::Long;
use Pod::Usage;
use strict;
use warnings;
use File::Basename;

my $me = basename($0);
### create the file PROGRAME_NAME.FAILED
die "There's already a $me.OK file here\n" if (-e "$me.OK" );
system("touch $me.FAILED")==0 || 
    die "Could not create a FAILED file for $me";

my $DR2D = 180.0/3.1415972;

$ENV{"SEX_CONFIG"} || 
    die "Define the environment variable SEX_CONFIG.";

my $configfile = "$ENV{SEX_CONFIG}/default.sex";

sub logmsg {
	my $msg = shift(@_);
	my $name = $0;
	print STDERR "$name: $msg \n";
	return;
}

### default parameter settings
my $help = 0;  ## do you want the help page?
my $root = "";  ## name of the image to do solution for
my $status =0;  ## Status of subrouting calls, moslty for SLA routines
my $i = 1;  ## everyone body needs a counter.
my $verbose; ## what's happening

my $force=0;
my $quiet=1;
my $dozp =0 ;

my $result = GetOptions('h|help|?' => \$help
			,'f|frame=s' => \$root
			,'v|verbose+' => \$verbose
			,'quiet' => \$quiet,
			,'dozp'=> \$dozp,
			,'force'	=> \$force
			) || pod2usage();

## Get the name of the image from the input file
$root =~ s/.fits$//;
my $image = "$root.fits";

## Set the variable for the wcsTools -v (verbose) flag
my $wcs_verbose= $verbose ? "-v" : "" ;
## Set the output to queit mode if flagged
my $output = "";
$output = " 2>>/dev/null" if ( $quiet ) ;


## Check to see if imwcs has already run on the image.  imwcs leaves
## WRA/WDEC keywords in the image header

if (`gethead  $image GETCEN | wc -l ` != 0 && !$force ) {
        my $suc = `gethead $image GETCEN`;
	chomp $suc;
	if ( $suc == 1 ) {
	    warn "Using existing wcs\n" if ( !$quiet) ;
	    system("touch $me.OK")==0 ||
		die "Could not create an OK file for $me";
	    exit ($suc);
	}
    }


### Create a backup copy of a keyword to keyword_ORIG
sub backup {
    my $image = shift ;
    my $keyword = shift ;
    my $keybck = shift ;
    
    ## check that the keyword exits;
    my $cmd = "gethead $keyword $image";
    my $origval = `$cmd`;
    chomp $origval;
    if ( $? != 0 ) {
	logmsg ("Error using wcsTool program gethead");
	logmsg ("$cmd");
	logmsg("STOP");
	exit ;
    }
    $origval or warn "Cann't make backup of non-existent keyword: $keyword\n";
    
    ## check the backup name doesn't exist
    $cmd = "gethead $keybck $image";
    my $result = `$cmd`;
    if ( $? != 0 ) {
	logmsg ("Error using wcsTool program gethead");
        logmsg ("$cmd");
	logmsg ("STOP");
	exit ;
    }
    chomp ( $result);
    if ( $result eq "" ) {
        ## COPY the KEYWORD to KEYBCK
        $cmd = "keyhead -hr $image $keybck=$keyword";
        $result = `$cmd`;
	if ( $? == 0 ) {
	    logmsg("Successfuly ran: $cmd") if ( $verbose) ;
	} else {
	    logmsg("Failed to run: $cmd");
	    logmsg("STOP\n");
	    exit -1 ;
	}	 
	## gethead the keywords to be certain they went in.
	$cmd = "gethead $keybck $image";
	$result = `$cmd`;
        chomp ( $result ) ;
        $result eq $origval or die "$0: Failed to insert keyword $keybck.\n";
        return "Successfuly copied $keyword to $keybck";
    } else {
	return "$keybck already exists.  ($result)";
    }
}

## GETKEYWORD
sub gethead {
	my $keyword = shift (@_);
	my $image = shift(@_);
	my $cmd = "gethead $keyword $image";
	my $result = `$cmd`;
        $? == 0 or die "Failed to run wcsTOOL: $cmd\n";
	chomp $result;
	return $result;
}

## The USNO catalog file needs to have this name for imwcs to
## understand the format

my $usno_stars = "usno_stars";

### READ the ORIGINAL RA and DEC from the IMAGE HEADER

my $header = "$root.mopheader";
-e $header || MOP_execute("stepZjmp","-f $root","force");
die "Failed while running stepZjmp " if ( $Bucket::Control::err ) ;
-e $header || die "Couldn't read or create $header?\n";

my $xcenter = gethead("CRPIX1",$header) ;
my $ycenter = gethead("CRPIX2",$header) ;
my $RAdeg = gethead("CRVAL1",$header);
my $DECdeg = gethead("CRVAL2",$header);
my $p     = gethead("PIXSCALE",$header);
logmsg "CRVAL [RA,DEC] [$RAdeg,$DECdeg]" if ( $verbose ); 
logmsg "CENTER: $xcenter $ycenter" if ( $verbose);
logmsg "POS: $RAdeg $DECdeg " if ( $verbose);


##THIS SHOULD BE IN MOPHEADER
my $maxcount = 30000;
logmsg("Warning: MAXCOUNT set to $maxcount: Please change to use mopheder") if ( !$quiet ) ;

#my $maxcount = `gethead $root.mopheader MAXCOUNT`;
#chomp($maxcount);


### assuming x/y pix scales are same
my $CD1_1 = -1*$p/3600.0;
my $CD2_2 = $p/3600.0;
my $RAcen = $RAdeg + (1024.0 - $xcenter )*$CD1_1;
my $DECcen = $DECdeg + (2048.0 - $ycenter)*$CD2_2;
logmsg "CD1/CD2: $CD1_1, $CD2_2" if ( $verbose);
logmsg "RAcen/DECcen: $RAcen $DECcen ($xcenter,$ycenter)" if ($verbose);

my $catfile = "stars.sex";
my $cmd = '';

my $coo = "$root.coo";
unlink $coo;
my $center = "$root.cen";
unlink $center;

my $mkpltsol = "$root.mkpltsol.usno";
if ( ! -e $mkpltsol ) {
    ### try and build one
    logmsg ("attempting to build $root.mkpltsol.usno") if ($verbose);
    eval {
	#MOP_execute("stepZjmp","-f $root")  && die "StepZjmp failed for $root " if ( $Bucket::Control::err );
	MOP_execute("step0jmp","-f $root -w 4 -t 5 -m 20000","force");
	die "Step0jmp failed for $root " if ( $Bucket::Control::err ) ;
	MOP_execute("step1jmp","-f $root -w 4 -t 5 -m 20000","force") ;
	die "Step1jmp failed for $root " if ( $Bucket::Control::err ) ;
	MOP_execute("mkpltsol","$root","force") ;
	die "mkpltsol failed " if ( $Bucket::Control::err ) ;
    };
    warn " There was an error $@ \n" if ( $@ ) ;
}
if ( ! -e "$root.mkpltsol.WARNING"  ){
    logmsg "get_cenphot: using $mkpltsol as input file.\n" if ($verbose);
    open (MKPL,"< $mkpltsol") || 
	die "couldn't open $mkpltsol for read\n";
    open (CENTER,"> $center") || 
	die "couldn't open $center for writing\n";
    open (USNO, "> $usno_stars" ) || 
	die "couldn't open $usno_stars for write\n";
	print USNO "id\tra\tdec\tmagr\tmagb\n";
	print USNO "---------\t-----------\t---------------\t------\t------\n";
    while (<MKPL>) {
	next if /^\#/;
	my ( $x, $y, $ra, $dec, $mag, $id, $blank)  ;
	($x, $y, $blank, $blank, $ra, $dec, $mag, $id ) = split ' ';
	printf USNO "%s\t%10.6f\t%+10.6f\t%4.1f\t%4.1f\n", 
	$id, $ra, $dec, $mag, $mag;
	printf CENTER "%7.2f\t%7.2f\t%4.1f\t%5.3f\n",
	$x, $y, $mag, 0.01;
    }
    close MKPL;
    close CENTER;
    close USNO;
} else {
    ## run sEXTRACTOR on the image
    my $thresh = 1.5;

    logmsg ("Using Sextractor config file: $configfile") if ($verbose) ;

    $cmd = "sex -c $configfile $image -DETECT_THRESH $thresh -ANALYSIS_THRESH $thresh -SATUR_LEVEL $maxcount -CATALOG_NAME $catfile ";

    logmsg ("$cmd") if ( $verbose );

    system($cmd.$output) == 0  or die "SEX returned error code: $? \n" ;

    ## sExtractor gives poor centroids, use IRAF to get better ones.
    ## Also reject bright stars inside the center.csh task
    $cmd = "center.csh $image $catfile $coo";
    logmsg ($cmd) if ( $verbose ) ;
    $result=`$cmd$output`;
    $cmd ="sort -n +2 $coo | head -250 | grep -v \"#\" > $center";
    logmsg($cmd) if ( $verbose ) ;
    system($cmd) == 0 or die "SORT returned error code: $?\n";

    ## Get the USNO catalog stars
    $cmd = "get_usno.pl --ra $RAcen --dec $DECcen --file $usno_stars";
    logmsg ( $cmd) if ( $verbose);
    $result = `$cmd$output`;
}

## remove the old wcs from the image.
#$result = 'delwcs $image';

foreach my $keyword ( "RA", "DEC", "CRVAL1", "CRVAL2", "CRPIX1", "CRPIX2", "PIXSCAL1" ) {
    my $kd = $keyword;
    if ( length($keyword) > 6 ) {
	$kd =~ s/..$// ;
    }
    backup($image, $keyword, $kd."_o");
}

## Build the WCS usinc the wcs tools
$cmd = "imwcs  $wcs_verbose -p is -h 200 -j $RAdeg $DECdeg -x $xcenter $ycenter -c $usno_stars -o -a 0 -p $p -d $center -w $image ";
logmsg ($cmd) if ( $verbose ) ;
$result = `$cmd$output`;
logmsg($result) if ( $verbose ) ;

## Refind the WCS with 8 terms.
$cmd = "imwcs $wcs_verbose -p is -n 8 -o -h 200 -c $usno_stars -d $center  -w $image";
logmsg($cmd) if ( $verbose ) ;
$result = `$cmd$output`;
logmsg($result) if ( $verbose ) ;

## Find all the stars the match USNO candidates
$cmd = "immatch -h 200 -f -d $center -c $usno_stars $image > $root.immatch";
logmsg($cmd) if ($verbose);
$result  = `$cmd$output`;
logmsg($result) if ( $verbose) ;

## Compare the matched star list to the available list to test goodness of fit.
$result = `grep match $root.immatch`;
$result =~ m/\D*(\d*)\D*(\d*).*/;
my $frac = 0;
$frac = $1/$2 if $2;
print STDERR "get_cenphot: $frac ($1/$2) of USNO stars matched for $root\n";


## Compare the IRAF photometry to the USNO, to get ZP.  This is rough
## and should be skipped if we actually have standards.
## if frac < 0.5 then we shouldn't accept the photometry
if ( $dozp && $frac > 0.5 ) { 
    ### reasonable defaults if no local parameters set
    my ($apin, $apout, $apcor, $aper, $zp) = (4, 15, 0.3, 0.5, 26.0);
    if ( -e "$root.apcor" ) {
       ($apin, $apout, $apcor, $aper) = split(' ',`cat $root.apcor`);
    } else {
	warn "There is no $root.apcor file?";
    }
    if ( -e "zeropoint.used" ) { 
       my $zp = `cat zeropoint.used`;
       chomp($zp);
    } else { 
	warn "No zeropoint.used file?";
    }
    my ($exptime) = split(' ',`gethead $root.mopheader EXPTIME `);
    my ($airmass) = `gethead $root.fits AIRMASS`;
    chomp($airmass);
    my ($filter) = `gethead $root.fits FILTER `;
    chomp($filter);
    chomp($exptime);
    $apout = $apout > 0 ? $apout : 10;
    $apin = $apin > 0 ? $apout : 4;
    my $sky = $apout + 3*$apin;
    my $out = 10 + $sky;
    $cmd = "--file $root.fits";
    $cmd .= " --ap $apout --insky $sky --outsky $out";
    $cmd .= " --zeropoint $zp --maxcount $maxcount --exptime $exptime";
    logmsg($cmd) if ($verbose);
    open(IMMATCH,"<$root.immatch");
    my @allmag=();
    ## Crank up IRAF for every mag given the x/y from immatch
    while(<IMMATCH>) {
	## skip lines that start with a #
	my($x,$y,$ras,$decs,$umag)=split ;
	next if ( $x =~ m/\#/ ) ;
	logmsg( "$x $y $umag ") if ($verbose);
	## add the x/y location to the command string
	my $this_cmd = $cmd." --xcen $x --ycen $y";
	logmsg( "$this_cmd" ) if ( $verbose ) ;
        my $do_res = MOP_execute("dophot.pl", $this_cmd,"force");
        my ($x_o, $y_o, $mag, $merr) = ($x,$y,0,0);
        if ( $Bucket::Control::err ) {
           warn "Photometry failed \n";
        } else {                
           foreach my $line ( split "\n", $do_res ) {
             if ( $line =~ /DOPHOT/ ) { 
                ### this is the line for dophot that has the photometry
                $line =~ /DOPHOT\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s+(\d*\.*\d*)\s*/;
                ($x_o, $y_o, $mag, $merr) = ($1,$2,$3,$4);
             }
 	   }
        }
        logmsg("$x $y $mag $merr $umag") if ( $verbose ) ;

    ### offset for the filter
    ###  according to M.R.Kidger http://www.iac.es/galeria/mrk/comets/USNO_Landolt.htm
    ###
    ### B: Landolt = 1.097*USNO_B - 1.216
    ### V: Landolt = 1.064*USNO_V - 0.822
    ### R: Landolt = 1.031*USNO_R - 0.417
    ### 
    ### where B/V/R are Johnson and USNO_V = (0.125*(5*USNO_R+3*USNO_B))
    ### 
    ### I will assume that g=R+0.4
    #### r= R
    #### i= R-0.4
        my %zp_off = (
	"g.MP9401" => 1.097*($umag+0.8)-1.216,
	"r.MP9601" => $umag*1.031 - 0.417,
	"i.MP9701" => -0.8
	);
	my %airmass = (
	"g.MP9401" => 0.150,
	"r.MP9601" => 0.100,
        "i.MP9701" => 0.050
	);
        my $zp_off = defined $zp_off{$filter} ? $zp_off{$filter}+$airmass{$filter}*($airmass-1) : 0 ;
	if ( $zp_off>0 && $mag =~ /^\d+.?\d*$/ && $umag =~ /^\d+.?\d*$/) {
	    my $dmag = $zp_off - $mag + $zp if ( $mag > 0 ) ;
	    logmsg("DMAG: $dmag") if ($verbose);
	    $allmag[$#allmag+1]=$dmag if ( $mag > 0 ) 
	}
    }
    ## get the median
    close(IMMATCH);
    my @zp = sort @allmag;
    $zp = $zp[int($#zp/2.0)];

    ## assume a normal dist and get the error 
    my $zp_err = ($zp - $zp[($#zp/6.0)]);
    $zp_err = $zp_err/($#zp - 1) if ( $#zp > 1 ) ;


    ## set the ZP in the usno_zp file
    logmsg( "Zeropoint determined to be $zp +/- $zp_err\n" ) if ( $verbose) ;
    open (ZP,">$root.usno_zp");
    printf ZP "%5.2f %4.2f\n",$zp,$zp_err;
    `sethead -h $root.fits USNOZP=$zp`;
    `sethead -h $root.fits ZPSCAT=$zp_err`;
    `sethead -h $root.fits USNO_N=$#zp`;
    
    close(ZP);
}

## Get rid of some tricky files
##`mv usno_stars $root.usno`;
##unlink $center;
##unlink $coo;
##unlink $catfile;
## keep the .immatch file for later MPC file creation
#unlink "$root.immatch";

my $code = $frac > 0.5 ? 1 : -1 ;
my $setres = `sethead $root.fits GETCEN=$code`;

system("touch $me.OK")==0 || 
    die "failed to create $me.OK file" ;


print STDERR $setres;
exit($code);

__END__

=head1 SYNOPSIS

B<get_cenphot.pl> -f file.fits

=head1 OPTIONS

=over 8

=item B<-f> 

Name of the fits file to rebuild the astrometric WCS for.  Checks for WRA keyword and skips file if 
that keyword exists.

=back

=head1 USAGE

The B<get_cenphot.pl> script uses I<sex> to find stars I<IRAF> to centroid them I<get_usno.pl> to find
the USNO stars in the field I<imwcs> to determine the solution and I<immatch> to check if the solution 
is anygood.  The last line of output is the fraction of USNO catalog stars that were found in the field.

=head1 NOTE

There is currently no 'drop out' if the solution is crap.  Should there be? 
