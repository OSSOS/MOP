#!/usr/bin/env perl

## accept on STDIN a list of image with the RA/DEC location of an candidate.
## Display the images and allow opperator to confirm/reject the candidates.


#use Bucket::MOP;
use Bucket::Control;
use File::Temp qw/ tempfile tempdir /;
use File::Basename;
use Getopt::Long;

use  strict;

my $debug=0;
my $help;
my $sec=256;
my $flist;
my $cat;
### mimium size of circle to mark on display
my $rad = 3.0/3600.0;

my $ds9 = "ds9";
#$ds9 = "a3i438fry124h.bc.hsia.telus.net:64361";

GetOptions('d|debug' => \$debug,
	   'f|frames:s' => \$flist,
	   'c|cat:s' => \$cat,
           'h|help' => \$help,
	   'ds9:s' => \$ds9,
           's|sec=s' => \$sec) || pod2usage(2);

-e $flist || die "The frames file $flist doesn't exist?\n";
## check if ds9 is accessable
my $check = `xpaaccess -c $ds9`;
warn "Checking for $ds9 give $check\n" if ($debug);
my $mopast_flag = $debug ? "--debug" : "--quiet"; 

my $image_base_dir = $ENV{'IMAGE_BASE_DIR'} ? $ENV{'IMAGE_BASE_DIR'} : '';

$check =~ /yes/ || system("ds9&") ;
while ($check =~ m/no/ ) { 
  warn "Sleeping while we wait for ds9 to start\n";
  sleep 3;
  $check = `xpaaccess -c ds9`;
}


my $obj_name = $flist;
$obj_name =~ s/\.(.*)//;
my $ext = $1;
my $MPC_FILE = $obj_name.".MPC";

warn "Checking/Nailing $obj_name\n" if ( $debug);

my @files;
my $nfiles=0;
open (FRAMES,"<$flist");
while (<FRAMES>) {
	next if ( m/^\#/ );
	warn $_ if ( $debug) ;
	my ($file,$mjdate,$ra,$dec,$dra,$ddec,$ang) = split ;
	$files[$nfiles]{RA} = $ra;
	$files[$nfiles]{MJDATE} = $mjdate;
	$files[$nfiles]{DEC} = $dec;
	$files[$nfiles]{DRA} = 2*$dra/3600.0 > $rad ? 2*$dra/3600.0 : $rad ;
	$files[$nfiles]{DDEC} = 2*$ddec/3600.0 > $rad ? 2*$ddec/3600.0 : $rad ;
	$files[$nfiles]{ANG} = $ang+90;
	$files[$nfiles]{FILE} = $image_base_dir.$file;
	### create the file_id tag
	$files[$nfiles]{FILE_ID} = basename($file);
	$files[$nfiles]{FILE_ID} =~ s/.fits$//;
	$nfiles++;

}


## now we have the full range of RA/DEC and files to look at
my @frames = sort {$a->{RA} <=> $b->{RA}} @files;

my $du =  $frames[0]{DRA} ;
my $dv =  $frames[0]{DDEC};
my $ang = $frames[0]{ANG};

my $dx = abs($du*cos($ang/57.3) - $dv*sin($ang/57.3));
my $dy = abs($dv*cos($ang/57.3) + $du*cos($ang/57.3));
				 
my $ramin = $frames[0]{RA} - $dx;
my $ramax = $frames[$#frames]{RA} + $dx;


my @frames = sort {$a->{DEC} <=> $b->{DEC}} @files ;

my $decmin = $frames[0]{DEC} - $dy;
my $decmax = $frames[$#frames]{DEC}+ $dy;

warn "$ramin $decmin $ramax $decmax\n" if ( $debug) ;
my @frames = sort {$a->{MJDATE} <=> $b->{MJDATE} } @files;


## foreach image create a section that is xsec/ysec wide centered on decCEN and raCEN
## first clear the $ds9 frames
`xpaset -p $ds9 frame delete all`;
## create a script to send to IRAF if the user decides they want this object
foreach my $frame ( @frames ) {

	my $cmd = "sky2xy $frame->{FILE} $frame->{RA} $frame->{DEC} J2000";
	warn "$cmd\n" if ($debug);
        my ($ra,$dec,$epoch,$arrow,$x,$y,$check) = split(' ',`$cmd`);
	$check == 1 or die "Cann't get x/y from sky2xy?";

	my $cmd = "sky2xy $frame->{FILE} $ramax $decmin J2000";
	warn "$cmd\n" if ($debug);
        my ($ra,$dec,$epoch,$arrow,$xl,$yl,$check) = split(' ',`$cmd`);
	$check == 1 or die "Cann't get x/y from sky2xy?";

	my $cmd = "sky2xy $frame->{FILE} $ramin $decmax J2000";
	warn "$cmd\n" if ($debug);
        my ($ra,$dec,$epoch,$arrow,$xu,$yu,$check) = split(' ',`$cmd`);
	$check == 1 or die "Cann't get x/y from sky2xy?";

        $xl = $xl - $sec < 1 ? 1 : int($xl - $sec);

        
        $yl = $yl - $sec < 1 ? 1 : int($yl - $sec);
	my $xmax = `gethead $frame->{FILE} NAXIS1`;
	chomp ($xmax);
	$xl = $xl > $xmax ? $xmax - 1 : $xl;
        $x = $x - $xl + 1;
	my $ymax = `gethead $frame->{FILE} NAXIS2`;
	chomp ($ymax);
	$yl = $yl > $ymax ? $ymax - 1 : $yl;
        $y = $y - $yl + 1;

        $xu = $xu + $sec > $xmax ? $xmax : int($xu + $sec);
	$xu = $xu < 2 ? 2 : $xu;
        $yu = $yu + $sec > $ymax ? $ymax : int($yu + $sec);
	$yu = $yu < 2 ? 2 : $yu;
	warn "$xl:$xu, $yl:$yu\n" if ($debug);

        $frame->{XL} = $xl;
        $frame->{YL} = $yl;
	print stderr "Displaying $frame->{FILE} box [$xl:$xu,$yl:$yu] \n";
        my $cmd = "imcopy ".$frame->{FILE}."'[$xl:$xu,$yl:$yu]' -";
        system("$cmd | xpaset $ds9 fits new");
	system("xpaset -p $ds9 scale mode zscale");
	system("xpaset -p $ds9 scale datasec no");
	system("xpaset -p $ds9 cmap invert yes");
        system("echo \"icrs; box $frame->{RA} $frame->{DEC} $frame->{DRA} $frame->{DDEC} $frame->{ANG} \" | xpaset $ds9 regions  ");
	$frame->{USNO} = $frame->{FILE};
	$frame->{USNO} =~ s/\.fits$/\.immatch/;
	open (USNO,"<$frame->{USNO}");
	while (<USNO>) {
	 next if ( m/^#/ ) ;
	 my ($x,$y,@rest ) = split ;
		   warn "checking if USNO star @ $x $y is in [$xl:$xu,$yl:$yu]  \n" if ($debug);
	    
	 if ( $x < $xu && $y < $yu && $x > $xl && $y > $yl ) {
	   $x = $x - $xl + 1;
	   $y = $y - $yl + 1;
	   my $cmd = "echo \"physical; cross point $x $y\"|xpaset $ds9 regions";
	   warn $cmd if ( $debug) ;
	   system($cmd);
	 } 
	}
	close(USNO);
}
## Allign the images using the WCS
my $ans;
do {
    system("xpaset -p $ds9 match frames wcs ");
    system("xpaset -p $ds9 frame first ");
    warn "Place Region around detection, S to skip target, B to load blink, M to measure it\n";
    $ans =  <STDIN>;
    chomp $ans ;
	## queu to the last frame incase user wants a blank frame
    if ( $ans =~ m/^M$/ ) {
        my $res = `xpaset -p $ds9 frame show all`;
        $res = `xpaset -p $ds9 frame last`;
	foreach my $frame ( @frames ) {
	    $res = `xpaset -p $ds9 frame next`;
	    my $mess=0;
	    warn "$res\n" if ( $debug && $res ) ;
	    open(REGION,"xpaget $ds9 regions |");
	    while (<REGION>) { 
	        print if ( $debug);
		### things that have ellipse and cirlce marks are 
		### objects we want output astrometry for.
		if ( m/ellipse/ || m/circle/ )  { 
                    $mess=1;
		    m/\D+(\d*\.?\d*),(\d*\.?\d*)/;
		    $frame->{XCEN} = $1 + $frame->{XL} - 1;
		    $frame->{YCEN} = $2 + $frame->{YL} - 1;
	            $frame->{MPC} .= "#L $frame->{FILE_ID} measured inside confirm @ $frame->{XCEN} $frame->{YCEN}\n";
		    warn "Measuring in $frame->{FILE_ID} @ $frame->{XCEN} $frame->{YCEN} \n";
		} 
	    }
	    close(REGION);
	    ## measure the object in this frame if a ellipse or circle was detected
	    if ($mess == 1 ) { 
		### call mopast to get astrometry and spit out an astrometric line
		my $args = " --frame $frame->{FILE} --xcen $frame->{XCEN} --ycen $frame->{YCEN} --name $obj_name $mopast_flag";;
	        warn "$args \n" if ($debug);
		my $result = MOP_execute("mopast2.pl",$args);
		$frame->{MPC} .= $result unless ($Bucket::Control::err) ;
		### print out the returned data if an error occured
		warn "mopast said: $result \n" if $Bucket::Control::err ;
	    }  else {
		### stick in a commend that no astrometry was measured for this frames
	        $frame->{MPC} .= "#L $frame->{FILE_ID} $obj_name not found in $frame->{FILE} \n";
	    }
	}
    } elsif ( $ans =~ m/^B$/ ) {
	my $fnum = `xpaget $ds9  frame `;
	chomp($fnum);
	my $frame = $frames[$fnum-1];
	warn "finding blink image for ds9 frame:$fnum (file_id): $frame->{FILE_ID} \n" if ($debug ) ;
	my $bl = MOP_execute("blank_find.pl"," --file_id '$frame->{FILE_ID}' --ra $frame->{RA} --dec $frame->{DEC}");
	warn "$bl" if ( $debug ) ;
	my @fline = split(' ',$bl);
	if (@fline == 7 ) {
	    $frames[$#frames+1]{FILE} = shift @fline;
	    $frames[$#frames]{MJDATE} = shift @fline;
	    $frames[$#frames]{RA} = shift @fline;
	    $frames[$#frames]{DEC} = shift @fline;
	    $frame=$frames[$#frames];
	    my $cmd = "sky2xy $frame->{FILE} $frame->{RA} $frame->{DEC} J2000";
	    warn "$cmd\n" if ($debug);
	    my ($ra,$dec,$epoch,$arrow,$x,$y,$check) = split(' ',`$cmd`);
	    $check == 1 or die "Cann't get x/y from sky2xy?";
	    
	    my $cmd = "sky2xy $frame->{FILE} $ramax $decmin J2000";
	    warn "$cmd\n" if ($debug);
	    my ($ra,$dec,$epoch,$arrow,$xl,$yl,$check) = split(' ',`$cmd`);
	    $check == 1 or die "Cann't get x/y from sky2xy?";
	    
	    my $cmd = "sky2xy $frame->{FILE} $ramin $decmax J2000";
	    warn "$cmd\n" if ($debug);
	    my ($ra,$dec,$epoch,$arrow,$xu,$yu,$check) = split(' ',`$cmd`);
	    $check == 1 or die "Cann't get x/y from sky2xy?";
	    
	    $xl = $xl - $sec < 1 ? 1 : int($xl - $sec);
	    $x = $x - $xl + 1;
	    $yl = $yl - $sec < 1 ? 1 : int($yl - $sec);
	    $y - $y - $yl + 1;
	    my $xmax = `gethead $frame->{FILE} NAXIS1`;
	    chomp ($xmax);
	    my $ymax = `gethead $frame->{FILE} NAXIS2`;
	    chomp ($ymax);
	    $xu = $xu + $sec > $xmax ? $xmax : int($xu + $sec);
	    $yu = $yu + $sec > $ymax ? $ymax : int($yu + $sec);
	    
	    $frame->{XL} = $xl;
	    $frame->{YL} = $yl;
	    print stderr "Displaying $frame->{FILE} box [$xl:$xu,$yl:$yu] \n";
	    my $cmd = "imcopy ".$frame->{FILE}."'[$xl:$xu,$yl:$yu]' -";
	    `$cmd | xpaset $ds9 fits new`;
	    `xpaset -p $ds9 scale mode zscale`;
	} else  {
	    warn "no blank found \n";
	}
    } elsif ( !($ans =~ m/^S$/ ) ) {
	my $fnum = `xpaget $ds9  frame frameno`;
	chomp($fnum);
	my $frame = $frames[$fnum-1];
	$frame->{MPC} .=  "#[$obj_name] $ans\n";
    } elsif ( $ans =~ m/^S$/  ) {
	exit(-1);
    }
} until ( $ans =~ m/^M$/ ) ;

## append the MPC record stuff to the specified output file 
open (MPC,">>$MPC_FILE");
foreach my $frame (@frames) { 
	print MPC $frame->{MPC};
}
close (MPC);
   

exit(0);

