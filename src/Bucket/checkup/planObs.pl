#!/usr/bin/env perl

use strict;
use warnings;
use Bucket::MOP;
use Bucket::Control;
use File::Temp qw/ tempfile tempdir /;
use Astro::SLA;
use Astro::Time;

open (PLOT,">plot.dat");



## UNPACK FORMAT FOR THE MPCORB.DAT FILE
## DETERMINE THE PHYSICAL LOCATION
our ( $n , $tel, $name, $w, $phi, $h);
#$tel ="KPNO158";
#$tel ="PALOMAR200";
#$tel ="KPNO158";
$tel ="CFHT";
#$tel ="PALOMAR200";
#$tel ="ESO3.6";
#$tel ="VLT1";
slaObs ( $n, $tel , $name, $w, $phi, $h );
our $elong=-1*$w;
our $tzone=7;


## Should calculate if the field is above the horizon for 
## more than x hours.
sub visible($$$) {
    my ($date, $ra, $dec ) = @_;
    slaRdplan($date, 10, $elong, $phi, my $ra_sun, my $dec_sun, my $diam);

    # double slaZd ( double ha, double dec, double phi )
    # double slaAirmas ( double zd )
    # void slaRdplan ( double date, int np, double elong, double phi,
    #            double *ra, double *dec, double *diam )
    my $up = 0 ;
    my $sun_set=0;
 
    ## sunset occurs when increasing HA pushes the sun below the horizon.
    ## sunrise occurs when increasing HA pushes the sun above the horizon.
    ## Also need the moon the moon to be more that 40 degrees from the
    ## field.
    ### compute the RA/DEC of the moon at noon (lst=ra_sun) at the observes
    ### location.

    slaRdplan($date,3,$elong,$phi,my $moon_ra,my $moon_dec,my $mdiam);
    slaDr2tf(0,$moon_ra, my $sign, my @moon_ra);
    for ( my $time = $ra_sun; $time < $ra_sun+D2PI; $time += 0.1*D2PI/24.0 ) {
	
	my $lst = slaRanorm($time);
	my $ha_sun = $lst - $ra_sun;
	my $zd_sun = slaZd($ha_sun,$dec_sun,$phi);

	if ( $zd_sun > (90+12.0)*DD2R ) {
	    #slaCr2tf(0, $lst, my $sign, my @lst); 
	    ## sun is set, is the object up?
	    my $ha = $lst - $ra;
	    my $airmass = slaAirmas(slaZd($ha,$dec,$phi));
 	    if ( $airmass < 2.5 ) {
		#printf "Target up and Sun Down %02d:%02d:%02d ",@lst;
		#Target is observable... 
		## where is the moon
		my $ha_moon = $lst - $moon_ra;
		my $zd_moon = slaZd($ha_moon,$moon_dec,$phi);
		my $obj_moon_sep = slaSep($ra,$dec,$moon_ra,$moon_dec);
		my $sun_moon_sep = slaSep($ra_sun,$dec_sun,$moon_ra,$moon_dec);
		#printf "%4d %4d %4d ", int(DR2D*$zd_moon),int(DR2D* $obj_moon_sep),int(DR2D* $sun_moon_sep) ;
		if ( $zd_moon > (90*DD2R) || 
		     ($obj_moon_sep > 10*DD2R + $sun_moon_sep) ) { 

		    $up++;
		}
		#print $up."\n";
	    }
	} 
    } 
    return ( $up*0.1 );
 
}

## DETERMINE THE LST AND MJD OF CIRCUMSTANCE
## GET THE CURRENT TIME
my ($ss,$mn,$hh,$dd,$mm,$yy,$wday,$yday,$isdst) = gmtime(time);
$dd=01;
## re-zero variable for use in SLA
#
$yy = 1900 + $yy;
$mm++;
$yy=shift;
$mm=shift;
$dd=shift;
#$dd;
### get the UT hour of NOON at the observer location. (mid day)
$hh = 12-24.0*$elong/D2PI;
$mn = 0;
$ss = 0;
print "$yy $mm $dd\n";
my ($lst, $mjd) = ut2lst_tel($yy,$mm,$dd,0,0,0,$tel);

my $orbit=Bucket::MOP->new("orbits","cfhls","shift+add");
my $abg = Bucket::MOP->new("abg","cfhls","shift+add");
my $measure=Bucket::MOP->new("measure","cfhls","shift+add");
my $des=Bucket::MOP->new("object","cfhls","shift+add");
$des->selectRows({"ORDER" => "official", "WHERE" => " official LIKE 'l%' OR official RLIKE 'o[23].*' " , "GROUP" => " official "});
#$orbit->selectRows({"ORDER" => "official", "WHERE" => "offical LIKE 'l%' "});
 

my $status;
### mjd is now the ModifiedJulianDate at 0hrs UT.
#slaCaldj("2004","09","10",$mjd,$status);
### noon at observes location is 0hrs UT + 12hrs - 24*elong/D2PI
print "### PREPARED FOR $tel $yy/$mm/$dd\n";
print "#         LST                 DESIGNATION      RA         DEC        dRA   dDEC ANG.  UP  NUM. ARC  LAST OBS   RATE ''/hr MAG FILTER\n";
print "#  RISE           SET                      HH:MM:SS.SS  DD:MM:SS.S   arcseconds DEG   HRS OBS  DAYS   DATE       RA  DEC  EST. \n";


while ( my $row=$des->fetchRow ) {
    #my ( $ra, $dec, $delta, $status, $j1, $j2 );
    
    my $official = $des->{'official'};
    print STDERR "Checking $official \n";
    my $abg_filename=$official.".abg";
    open(ABG,"> ".$des->{'official'}.".abg");
    my $res=$abg->selectRows({"WHERE"=>"official LIKE '".$official."'"});
    warn "Bad orbit file in db (run fuse.pl) \n" if ( $res!= 1) ;
    $abg->fetchRow;
    print ABG $abg->{abg};
    close(ABG);
    #$measure->selecctRows({"WHERE" =>"provisional LIKE

    #print STDERR $mjd;
    my ( $ra, $dec, $dra, $ddec, $angle ) =Bucket::MOP::predict($abg_filename,$mjd,568);
    my ( $ra2, $dec2, $dra2, $ddec2, $angle2 ) =Bucket::MOP::predict($abg_filename,$mjd+1.0/24.0,568);
    unlink($abg_filename);

    my $raRate = ($ra2-$ra)*57.3*3600.0;
    my $decRate = ($dec2-$dec)*57.3*3600.0;
    #next if (abs($raRate)<0.01);
    #next if (sqrt($dra**2+$ddec**2) > 600.0 ) ;
    my (@ra, $rsign, @dec, $dsign, @lst_rise, $lsign, @lst_set );
    my $uptime = visible($mjd, $ra, $dec);

    if ( $uptime > -2. ) {
	my $sql = "SELECT avg(mag) mag , filter FROM measure m JOIN object o ON m.provisional LIKE o.provisional where o.official LIKE '".$official."' AND filter NOT LIKE '' group by filter order by mag desc ";
	my $mdbh = $measure->{_DBH};
	my $msth = $mdbh->prepare($sql);
	$msth->execute();
        $row=$msth->fetchrow_hashref();
	my $mag=0;
	if ( $row->{mag} ) { 
	  my $mag = $row->{mag};
	}
        my $filter="NA";
	if ($row->{filter}) {
	  $filter = $row->{filter};
 	}
        my ($lst_rise, $lst_set)=rise(rad2turn($ra),rad2turn($dec),rad2turn($phi),deg2turn(30.0));
        warn "Bad LST rise / Set " unless ( $lst_rise && $lst_set ) ;
	slaDr2tf(0, turn2rad($lst_rise), $lsign, @lst_rise);
	slaDr2tf(0, turn2rad($lst_set), $lsign, @lst_set);

	my $raoffset = 7.0/(60.0*57.3);
	my $decoff = 3.0/(60.0*57.3);
	#$ra = $ra + $raoffset;
	#$dec = $dec - $decoff;
	slaDr2tf(2, $ra, $rsign, @ra );
	slaDr2af(1, $dec, $dsign, @dec);

	my $dbh = $orbit->{_DBH};
	my $sql = "SELECT count(*) nobs, max(mjdate) ldate ,max(mjdate)-min(mjdate) arc FROM measure m JOIN object o ON m.provisional LIKE o.provisional WHERE official LIKE '".$official."' group by official having abs(ldate - ".$mjd.") > 180.0 ";
	my $sth = $dbh->prepare($sql);
	my $rv = $sth->execute();
	$row = $sth->fetchrow_hashref();
	next if ( ! $row ) ;
        #next if ( $row->{arc} < 500 );
        #next if ( $arc} < 500 );
	slaDjcl($row->{ldate},my $year, my $month, my $day, my $frac,  $status);
        print STDERR  "Bad date conversion \n" if ($status);
	printf "%02d:%02d:%02d:%02d -- %02d:%02d:%02d.%02d ",
	@lst_rise, @lst_set;
	my $this_object=$official;
        $official = "***  $official" if (sqrt($dra**2+$ddec**2) > 600.0 ) ;
	printf "%15s %02d:%02d:%02d.%02d %s%02d:%02d:%02d.%01d %5d %5d %4d  %4.1f %04d %04d %04d-%02d-%02d %5.1f %4.1f %4.1f %1s \n",
	$official, @ra, $dsign, @dec, $dra, $ddec, $angle, $uptime, $row->{nobs}, int($row->{arc})+1,$year, $month, $day, $raRate,$decRate, $mag, $filter ;
        print PLOT " $this_object $ra $dec $dra $ddec $angle\n" ;
        next;
	if ( $dra > 2040.0 ) {
	   ## offset by half the error ellipse in both ra/dec twice
	   #$raoffset = (16.0/60)/57.3;
	   #$decoff = 0/57.3/2.0;

	   $ra = $ra + $raoffset;
	   $dec = $dec - $decoff;
	   slaDr2tf(2, $ra, $rsign, @ra );
	   slaDr2af(1, $dec, $dsign, @dec);
	   #printf "%15s %02d:%02d:%02d.%02d %s%02d:%02d:%02d.%01d %5d %5d %4d  %4.1f %02d %03d %04d-%02d-%02d %4.1f %4.1f %4.1f %1s \n",
	   $ra = $ra - 2*$raoffset;
	   $dec = $dec + 2*$decoff;
	   slaDr2tf(2, $ra, $rsign, @ra );
	   slaDr2af(1, $dec, $dsign, @dec);
	   #printf "%15s %02d:%02d:%02d.%02d %s%02d:%02d:%02d.%01d %5d %5d %4d  %4.1f %02d %03d %04d-%02d-%02d %4.1f %4.1f %4.1f %1s \n",
        }
    }
}

close(PLOT);

