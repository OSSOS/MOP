#!/usr/bin/env perl
#
##  Input RA, Dec and sma of object, and MJD of 1 or 2 frames,
## returns the rate of motion[arcsec/hr] and the direcion angle[deg], or
## returns change in RA and Dec[deg or arcsec] of object between 2 frames.
##  Inputs: ( MJD, RA[rad], Dec[rad], SMA[AU] ) or
##          ( MJD 1, MJD 2, RA[rad], Dec[rad], SMA[AU] )
##  Options: 
#

use Astro::SLA;
use Math::Trig;
use strict;
use Getopt::Long; 
Getopt::Long::Configure("pass_through");
require v5.6.0;

# Constants
use constant PI => atan2(1,1) * 4;
use constant G => 6.67259e-11;  #grav. const. [N m**2 kg**-2]
use constant Msun => 1.989e30;  #Solar mass [kg]
use constant AU => 1.4960e11;  #num of metres in 1AU
use constant RAD => 206264.8;  #num of arsecs in 1rad

# Initialize Parameters
my $change = '0';  #option variable with default 'false'
my $arcsec = '0';  #option variable with default 'false'
my $sexa = '0';  #option variable with default 'false'
my $loc = '0'; 
my $file = ''; # cfht megacam image to use as source of variable
GetOptions ('file:s'=> \$file, 'change' => \$change, 'arcsec' => \$arcsec, 's' => \$sexa, 'loc' => \$loc);

my ($mjd1,$mjd2,$raObj,$decObj,$smaObj,$LONG,$LAT);

if ( $file ){
    my $gethead = `which gethead`;
    chomp $gethead;
    die "can't run gethead? \n" unless -f $gethead;
    #print "$gethead $file DETECTOR\n";
    my $detector = `$gethead $file DETECTOR`;
    chomp $detector;
    $detector =~ m/MegaCam/ || die "-file Only works on MegaCam images "; 
    $mjd1 = `$gethead $file MJDATE`;
    chomp $mjd1;
    $mjd2 = $mjd1 + 1/24;
    $raObj = `$gethead $file RA_DEG`;
    chomp $raObj;
    $raObj = $raObj*DD2R;
    $decObj = `$gethead $file DEC_DEG`;
    chomp $decObj;
    $decObj = $decObj*DD2R;

} else{
          
    if (!$change && !$loc && @ARGV != 4){
	die "4 arguements needed MJDATE RA DEC SEMI\n";
    }
    elsif ($change && @ARGV != 5){
	die "5 arguements needed MJDATE1 MJDATE2 RA DEC SEMI\n";
    }
    elsif (!$change && $loc && @ARGV != 6){
	die "6 arguements needed MJDATE RA DEC SEMI LONG LAT \n";
    }
    elsif($change && $loc && @ARGV != 7){
	die "7 arguements needed MJDATE1 MJDATE2 RA DEC SEMI LONG LAT\n";
    }
    
    $mjd1 = shift(@ARGV);  #MJD of 1st frame
    if ($change){
	$mjd2 = shift(@ARGV);
    }
    else{
	$mjd2 = $mjd1 + 1/24;
    }                      #MJD of 2nd frame

    if ($sexa){
	$raObj = hmsToRad(shift(@ARGV));
	$decObj = dmsToRad(shift(@ARGV));
    } else {
	my $raTmp = shift(@ARGV);
	$_ = $raTmp;
	if (m/:/){
	    die "-s option must be used for sexadecimal RA\n";
	}
	$raObj = degToRad($raTmp);
	my $decTmp = shift(@ARGV);
	$_ = $decTmp;
	if (m/:/){
	    die "--sexa option must be used for sexadecimal Dec\n";
	}
	$decObj = degToRad($decTmp);
    }
}
$smaObj = shift ;

if ($loc){
    $LONG = shift(@ARGV);
    $LAT = shift(@ARGV);
    if($LAT < -1*PI/2 || $LAT > PI/2 || $LONG < 0 || $LONG > 2*PI){
	die "bad location given\n";
    }
} else { 
    $LONG => -2.707;  #long. of CFHT[rad]   
    $LAT => 0.344;  #lat. of CFHT [rad]
}

# Checking for proper inputs
if ($change && $mjd2 == $mjd1){
    die "0 0\n";
}
if ($raObj < 0 || $raObj > (2*PI)){
    die "RA not within range 0-360\n";
}
if ($decObj > (PI/2) || $decObj < (-1 * (PI/2))){
    die "Dec not within range -90 - +90\n";
}
if ($smaObj <= 1){
    die "SMA must be > 1AU\n";
}


my (@rdSun1,@ESinit,@rdSun2,@ESfin);

# Obtain vector to Sun for 1st and 2nd frame
if ($mjd2 >= $mjd1){
  @rdSun1 = raDecSun( $mjd1 );  #RA and Dec of sun on MJD1 [rad]
#print "\nrdSun1 @rdSun1\n";
my @eclip = eqToEc(@rdSun1,$mjd1);
#print "ecliptic coor:  @eclip\n";
my @rect = getRectCoor(@eclip,0.9825);
#print "Rect coor: @rect\n";
##################
# In: RA,Dec[rad],MJD. Out: ecliptic longitude, latitude [rad]
sub eqToEc{
  if (@_ != 3){
    print "eqToEc failed\n";
    return();
  }
  my ($ra,$dec,$mjd) = @_;
  my ($long,$lat);
  slaEqecl ( $ra, $dec, $mjd,$long,$lat );
  
  return($long,$lat);
}

##################
  @ESinit = getRectCoor( @rdSun1,1 );
#print "@ESinit\n\n";

  @rdSun2 = raDecSun( $mjd2 );
#print "rdSun2 @rdSun2\n";
  @ESfin = getRectCoor( @rdSun2,1 );
#print "@ESfin\n\n";
@eclip = eqToEc(@rdSun2,$mjd2);
#print "ecliptic coor:  @eclip\n";
@rect = getRectCoor(@eclip,1);
#print "Rect coor: @rect\n";
}
else{
  @rdSun1 = raDecSun( $mjd1 );  #RA and Dec of sun on MJD1 [rad]
  @ESfin = getRectCoor( @rdSun1,1 );

  @rdSun2 = raDecSun( $mjd2 );
  @ESinit = getRectCoor( @rdSun2,1 );
}
my $sunRa = radToDeg(@rdSun1[0]);
my $sunDec = radToDeg(@rdSun1[1]);
# Vector from Earth to Object, Frame 1
my @EOinit = getRectCoor( $raObj, $decObj, 1 );  #Unit Vector to Obj.
my $ESEO = getAngle( @ESinit, @EOinit );  #Angle between ESinit and EOinit
my $EOSO = asin( (1/$smaObj) * sin($ESEO) ); #Angle between EOinit and SOinit (sine law)
my $ESSO = PI - $ESEO - $EOSO;  # ESinit and SOinit
my $dist = sqrt( 1 + $smaObj**2 - 2 * $smaObj * cos($ESSO));  #Distance from Earth to object. [AU]
@EOinit = getScaledVector( @EOinit, $dist );  #Scaled vector from E to Obj

# Vector from Sun to Obj. Frame 1
my @SOinit = subVectors( @EOinit, @ESinit );

# Vector from S to Obj Frame 2  (considering obj orbital motion)
my @up = crossProduct( @ESinit, @ESfin );
my @orbMotx = crossProduct( @up, @SOinit );  #vel vector of obj
my $time = abs ( ($mjd2 - $mjd1) * 86400 ); #time between frames [sec]
my $radius = ( $smaObj * AU );  #Obj sma [metres]
my $displm = ( $time * sqrt( G * Msun / $radius ));  #displ[m] of obj along vel vector
my $displ = ($displm / AU);  #displ[AU]
my @orbMot = getScaledVector(@orbMotx, $displ);  #obj scaled vel vector
my @SOfin = addVectors(@SOinit, @orbMot);

# Vector from E to Obj Frame 2
my @EOfin = addVectors(@ESfin, @SOfin);

# Reflex motion of obj
my $rate = radArcsec( &getAngle(@EOinit, @EOfin) );  #rate of motion of obj between frames [arcsec/hr]

my @coorInit = getRaDec(@EOinit);
my $raInit = ( shift(@coorInit) );  #[rad]
my $decInit = ( shift(@coorInit) );  #[rad]

my @coorFin = getRaDec(@EOfin);  
my $raFin = ( shift(@coorFin) );  #[rad] 
my $decFin = ( shift(@coorFin));  #[rad] 

###########TESTING##########

my $raI = radToDeg($raObj);
my $raF = radToDeg($raFin);
my $diff = $raF - $raI;
#print" $diff\n";
#print "$raObj $decObj\n";
#print "$raFin $decFin\n";

###########################


# Change in RA and Dec
my $raDiff = ($raFin - $raInit);  #
if ($raDiff > PI){                #change in RA [rad]
  $raDiff = ($raDiff - (2*PI));   #
}                                    
my $decDiff = ($decFin - $decInit);  #change in Dec [rad]

if ($mjd2 < $mjd1){
  $raDiff = -1 * $raDiff;
  $decDiff = -1 * $decDiff;
}
my $raDiffCorr = $raDiff * cos($decObj);
my $angle = radToDeg(atan2($decDiff,$raDiffCorr));
if ($angle >= 0){
  $angle = 180 - $angle;
}
else{
  $angle = -180 - $angle;
}
my $raDiffArcsec = radArcsec($raDiff);    #[arcsec]
my $decDiffArcsec = radArcsec($decDiff);  #[arcsec]

my $raDiffDeg = radToDeg($raDiff);   #[deg]
my $decDiffDeg = radToDeg($decDiff); #[deg]

##
#### Now for high inclination orbits ###
##
my $width = 0;
my $rateMax = 0;
my $rateMin = 99999;

if (!$change){ # only for default "rate output"

  # find ecl. lat of field (inclination)
  my ($ecLong,$ecLat);
  slaEqecl($raObj,$decObj,$mjd1,$ecLong,$ecLat);
  my $I = abs(int( radToDeg($ecLat) )) + 1; #starting incl.[deg]
  if ($ecLat < 0){
    $I = -1 * $I;
  } 
  my @iHat = (1,0,0);
  my @jHat = (0,1,0);
  my @kHat = (0,0,1);
  my @iPrime = getUnitVector(@SOinit);
  my @jPrime = getUnitVector(@orbMot);
  my @kPrime = getUnitVector( crossProduct(@SOinit,@orbMot) );
  my @incEOfin; 
  my @incOrbMot;
 
  while (abs($I) <= 90){
    
    # for inclinations other than that of the observed field
    my $inc = degToRad($I); #- degToRad($ecLat); #angle between the 2 obj mot vectors[rad]
   
    $incOrbMot[0] = cos($inc) * dotProduct(@jPrime,@iHat) + 
      sin($inc) * dotProduct(@kPrime,@iHat); # x component
    $incOrbMot[1] = cos($inc) * dotProduct(@jPrime,@jHat) + 
      sin($inc) * dotProduct(@kPrime,@jHat); # y component
    $incOrbMot[2] = cos($inc) * dotProduct(@jPrime,@kHat) + 
      sin($inc) * dotProduct(@kPrime,@kHat); # z component
    my $incDispl = $displ * sqrt(2);
    @incOrbMot = getScaledVector(@incOrbMot,$incDispl);
    
    my @incSOfin = addVectors(@SOinit, @incOrbMot);
    @incEOfin = addVectors(@ESfin, @incSOfin);
    
    # Reflex motion of object with greater inclination
    my $incRate = radArcsec( &getAngle(@EOinit, @incEOfin) );#rate of motion of obj between frames[arcsec/hr]
    if($incRate <= $rateMin){
      $rateMin = $incRate;
    }
    if($incRate >= $rateMax){
      $rateMax = $incRate;
    }

    my @incCoorFin = getRaDec(@incEOfin);  
    my $incRaFin = ( shift(@incCoorFin) );  #[rad] 
    my $incDecFin = ( shift(@incCoorFin));  #[rad] 
    
    # Change in RA and Dec for object with greater inclination
    my $incRaDiff = ($incRaFin - $raInit);  
    if ($incRaDiff > PI){                #change in RA [rad]
      $incRaDiff = ($incRaDiff - (2*PI));   
    }                                    
    my $incDecDiff = ($incDecFin - $decInit);  #change in Dec [rad]    
    if ($mjd2 < $mjd1){
      $incRaDiff = -1 * $incRaDiff;
      $incDecDiff = -1 * $incDecDiff;
    }

    my $incRaDiffCorr = $incRaDiff * cos($decObj);
    my $incAngle = radToDeg(atan2($incDecDiff,$incRaDiffCorr));
    if ($incAngle >= 0){
      $incAngle = 180 - $incAngle;
    }
    else{
      $incAngle = -180 - $incAngle;
    }
    my $angDiff = abs($incAngle - $angle);
    if ($angDiff > 180){
      $angDiff = 360 - $angDiff;
    }
    if ($angDiff > $width){
      $width = $angDiff;
    }
    if ($ecLat < 0){
      $I--;
    }
    else{
      $I++;
    }
  }#end while
}#end if

##
### Output
##
if ($change){
  if ($arcsec){
    print "$raDiffArcsec $decDiffArcsec\n";
  }
  else{
    print "$raDiffDeg $decDiffDeg\n";
  }
}
else{
  print "$rate $angle $width $rateMin $rateMax\n";
}


# END

#--------------------O---------------------#

# Params: string with hours, minutes, seconds separated by ':'.  Returns: angle in radians
sub hmsToRad{
  my $hms = $_[0];
  my ($hh,$mm,$ss) = split(':', $hms);
  $hh =~ s/^[+]//;
  my $deg = ( abs($hh) + abs($mm/60) + abs($ss/3600) ) * 15;
  my $rad = degToRad($deg);
  return($rad);
}
# Params: string with deg, arcmin, arcsec separated by ':'.  Returns: angle in radians
sub dmsToRad{
  my $dms = $_[0];
  my ($dd,$mm,$ss) = split(':', $dms);
  $_ = $dd;
  my $sign;
  if (m/^-/){
    $sign = -1
  }
  else{
    $sign = 1;
  }
  $dd =~ s/^[-+]//;
  my $deg = $sign * ( $dd + $mm/60 + $ss/3600 );
  my $rad = degToRad($deg);
  return($rad);
}
# Param: angle[rad].  Returns: angle[arcsec]
sub radArcsec{
  my $rad = $_[0];
  my $arcsec = ( $rad * RAD );
  return($arcsec);
}
# Param: angle[rad].  Returns: angle[deg]
sub radToDeg{
  my $rad = $_[0];
  my $deg = $rad * 180/PI;
  return($deg);
}

sub degToRad{
  my $deg = $_[0];
  my $rad = $deg * PI/180;
  return($rad);
}
# Params: MJD, returns RA and Dec of Sun [rad]
sub raDecSun {
  my $MJD = @_[0];
  my ($raSun, $decSun, $diam);
  slaRdplan( $MJD, 10, $LONG, $LAT, $raSun, $decSun, $diam );
  return($raSun, $decSun);
}

# Params: RA[rad], Dec [rad], distance[AU].  Returns vector array, origin = Earth
sub getRectCoor { 
  my ($ra,$dec,$d) = @_;
  my $phi = ( PI/2 - $dec );   # [rad]
  my $theta = $ra;
  my $rho = $d;
  
  my @coor;
  $coor[0] = ($rho * sin($phi) * cos($theta));
  $coor[1] = $rho * sin($phi) * sin($theta);
  $coor[2] = $rho * cos($phi);
  return(@coor);
}

# Params: vector array[AU].  Returns RA and Dec[rad]
sub getRaDec{
  my ($x,$y,$z) = @_;
  my $rho = getLength ($x,$y,$z); #[AU]
  my $theta = atan2 ($y,$x); #[rad]
  if ($theta < 0){
    $theta = $theta + (2*PI);
  }    
  my $phi = abs( acos ($z/$rho) ); #[rad]
  my $raOut = abs($theta);
  my $decOut = ( PI / 2 - $phi);
  return($raOut, $decOut);
}

sub getLength{
  my ($x, $y ,$z) = @_;
  my $length = sqrt( $x**2 + $y**2 + $z**2 );
  return($length);
}

# Params: array=(x,y,z).  Returns vector array of length 1.
sub getUnitVector {
  my ($x,$y,$z) = @_;
  my $length = sqrt( $x**2 + $y**2 + $z**2 );
  my @unitVector = ( $x/$length, $y/$length, $z/$length );
  return(@unitVector);
    
}

# Params: array: vector and scalar: length. Returns scaled vector array.
sub getScaledVector {
  my ($x,$y,$z,$length) = @_;
  my @scaledVector = getUnitVector($x,$y,$z);
  for ( my $i=0; $i<3; ++$i ){
    $scaledVector[$i] = $scaledVector[$i] * $length;
  }
  return(@scaledVector);
  
}

#  Params: 2 vector arrays.  Returns dot product.
sub dotProduct{
  my ($x,$y,$z,$x2,$y2,$z2) = @_;
  my $product = ($x * $x2) + ($y * $y2) + ($z * $z2);
  return($product);
}

# Params: 2 vector arrays, in order.  Returns vector array = v1 X v2
sub crossProduct {
  my @A = ( shift(@_),shift(@_),shift(@_) );
  my @B = ( shift(@_),shift(@_),shift(@_) );
  
  my @AxB;
  $AxB[0] = ( ($A[1] * $B[2]) - ($B[1] * $A[2]) ); #1st component
  $AxB[1] = ( ($A[2] * $B[0]) - ($B[2] * $A[0]) );
  $AxB[2] = ( ($A[0] * $B[1]) - ($B[0] * $A[1]) );
  return(@AxB);
}

#  Params: 2 vector arrays.  Returns the angle[rad] between 2 vectors
sub getAngle {
  my @uVector1 = getUnitVector( shift(@_),shift(@_),shift(@_) );
  my @uVector2 = getUnitVector( shift(@_),shift(@_),shift(@_) );
  my @dotProd = dotProduct( @uVector1,@uVector2 );
  my $angle = abs( acos(@dotProd) );
  return($angle);
}

# Params: 2 vector arrays.  Returns 1 vector array... the sum.
sub addVectors{
  my @added = ($_[0] + $_[3], $_[1] + $_[4], $_[2] + $_[5] );
  return(@added);
}

# Params: 2 vector arrays.  Returns 1 vector array... the difference.
sub subVectors{
  my ($x,$y,$z,$x2,$y2,$z2) = @_;
  my @diff = ($x - $x2, $y - $y2, $z - $z2 );
  return(@diff);
}


__END__

=head1 NAME

rate - gives rate of reflex motion of an object or the change in its position between two observations

=head1 SYNOPSIS

./rate.pl [options] MJD1 (MJD2) RA Dec SMA (Long Lat)

output: rate angle +-angle rateMax rateMin

 Options:
   -s          sexadecimal 
   --loc       location of observatory (default Mauna Kea)
   --change    change in position
   --arcsec    output in arcsecs

=head1 OPTIONS

=over 8

=item B<-s>

RA and Dec are input in sexadecimal

=item B<--loc>

the longitude and latitude (in radians) of the obeserver's location

=item B<--change> 

output: [change in RA] [change in Dec].  Both are in degrees

=item B<--arcsec> 

output the change in position in arcsecs rather than degrees (only with --change)

=back

=head1 DESCRIPTION

 MJD1 - Modified Julian Date of first observation
 MJD2 - Modified Julian Date of second observation (for --change option)
 RA - Right Ascension (in degrees) of the observations
 Dec - Declination (in degrees) of the observations
 SMA - Semi-major axis (in Astronomical Units [AU]) of objects orbit 
 Long - Longitude of observatory (in radians)
 Lat - Latitude of observatory (in radians)

Outputs the rate of reflex motion (in arcsec/hr) of an object beyond Earths orbit or the change in RA and Dec (in arcsec) of an object between the two observations

=head1 EXAMPLE

./rate.pl --change --arcsec -s 52661 67:04:00 0:0:0 45

=cut

=head1 NOTES

The calculations are performed assuming the time between the two observations is small, and the object has a circular orbit, with the given semi-major axis, and inclination angle equal to that of the observations (wrt the ecliptic plane).  They take into account the motion of both the Earth and the object in their orbits.

=cut
