#!/usr/bin/env python
#/*+
#************************************************************************
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
#* (c) <year>.				(c) <year>.
#* National Research Council		Conseil national de recherches
#* Ottawa, Canada, K1A 0R6 		Ottawa, Canada, K1A 0R6
#* All rights reserved			Tous droits reserves
#* 					
#* NRC disclaims any warranties,	Le CNRC denie toute garantie
#* expressed, implied, or statu-	enoncee, implicite ou legale,
#* tory, of any kind with respect	de quelque nature que se soit,
#* to the software, including		concernant le logiciel, y com-
#* without limitation any war-		pris sans restriction toute
#* ranty of merchantability or		garantie de valeur marchande
#* fitness for a particular pur-	ou de pertinence pour un usage
#* pose.  NRC shall not be liable	particulier.  Le CNRC ne
#* in any event for any damages,	pourra en aucun cas etre tenu
#* whether direct or indirect,		responsable de tout dommage,
#* special or general, consequen-	direct ou indirect, particul-
#* tial or incidental, arising		ier ou general, accessoire ou
#* from the use of the software.	fortuit, resultant de l'utili-
#* 					sation du logiciel.
#*
#************************************************************************
#*
#*   Script Name:	wcs_update.py
#*
#*   Purpose:
#*	given a megaprime CCD frame. update the wcs.
#*
#*   Functions:
#+	function_name	: Brief description
#+	function_name	: Brief description
#*	...
#*
#*   Date		: <mmm dd, yyyy>
#*
#*   CVS data:
#*	$Header: /home/observe/cvsroot/MOP/src/jjk/preproc/get_cenphot.py,v 1.1 2005/01/16 04:56:55 observe Exp $
#*
#*   Programmer		: JJ Kavelaars
#*
#*   Modification History:
#*   $Log $
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/
"""Update the WCS of a megaprime image by taking an input star list """

__Version__ = "$Revision 0.1$"


import optik, sys
from optik import OptionParser

if __name__=='__main__':
    parser=OptionParser()
    parser.add_option("--image",
                      action="store",
                      help="Image to modified WCS of",
                      dest="image")

    parser.add_option("--usno",
                      action="store",
                      help="USNO catalog stars",
                      dest="usno")

    parser.add_option("--stars",
                      action="store",
                      help="x,y location of stars in input image")

    (opt, args) = parser.parse_args()

    
    cmd = "imwcs -p is -h 200 -d "+opt.stars+" -c "+opt.usno+" -w "+opt.image
    os.system(cmd)
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
