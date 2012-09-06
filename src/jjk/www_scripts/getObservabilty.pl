#!/usr/bin/env perl 

use Astro::SLA;
use warnings;
use Net::MySQL;
use strict;
use Term::ReadKey;

my $hostname="cadchd.hia.nrc.ca";
my $port=33306;
my $database="bucket";
my $user="lsadmin";

my $password="";
### Try and open the .dbrc file and get the needed information
open (DBRC,"< /home/cadc/kavelaar/.dbrc ");
while (<DBRC>) {
        my ( $type, $db, $db_user, $db_password ) = split ;
        chomp $db_password;
        $password = $db_password;
        last if ( $db_user =~ m/$user/ && $db =~ m/$database/ && $type =~ m/MYSQL/ ) ;
        $password='';
}

if( !$password ) { 

print STDERR "Please enter the DB password for $user on $hostname: ";

ReadMode 3; 
my $password=<STDIN>;
chomp $password; 
print "\n"; 
ReadMode 1;

chomp $password;
}


my $mysql = Net::MySQL->new(
                            hostname => $hostname,
                            port => $port,
                            database => $database,
                            user => $user,
                            password => $password
                            ) ||die "Cann't connect to DB";
			    
use constant LAT => 0.344;  #lat. of CFHT [rad]
use constant LONG => -2.707;  #long. of CFHT[rad]

### JD fraction at midnight in Hawaii.
my $mjd_midnight = 1.0-0.0833;


print STDERR "STARTING\n";


## determine the first day of the year

my %elong;
$elong{'leading_start'} = 250;
$elong{'leading_end'} = 235;
$elong{'opposition_start'} = 200;
$elong{'opposition_end'} = 160;
$elong{'trailing_start'} = 125;
$elong{'trailing_end'} = 110;

my $gal_lat_limit = 10;
my $count=0;
## determine the RA/DEC of the ecliptic survey fields.
my $fov = 0.97;
my %year = (
	    "A" => 2003,
	    "B" => 2004,
	    "C" => 2005,
	    "D" => 2006,
	    "E" => 2007
	) ;

my @keys = ( "pointing", 
	     "leading_start", "leading_end", 
	     "opposition_start", "opposition_end", 
	     "trailing_start", "trailing_end" ); 
    
foreach my $block ( "C", ) { 
    ## Start of the survey is on the JD of the Spring Equinox (there abouts)
    slaCaldj ( $year{$block}, 03, 21, my $start_date, my $status) ;
    ($status == 0) or 
	die "Error determinig the MJD of start of 2003.\n";
    ## Work out the position of the sun on the start date
    slaRdplan($start_date,10,LONG,LAT, my $ra_sun, my $dec_sun, my $diam_sun);
    ## Determine the lat/long of the Sun.
    slaEqecl( $ra_sun, $dec_sun, $start_date, my $long_sun, my $lat_sun);
    ## Loop over all the LAT/LONG combinations in the survey
    my $d_long;
    my $long_deg = 0;
    ## the increment of the long_deg is done at the end of the loop... 
    ## Get the list of pointings from the table.
    $mysql->query("SELECT p.id,ra,`dec`,name FROM pointings p LEFT JOIN vwfields v ON p.id=v.pointing where v.id IS NULL  and p.id >  4803 ");
    my $record_set = $mysql->create_record_iterator;
    while ( my $row = $record_set->each ) {
	my %values;
	my $ra_rad = DD2R*$row->[1];
	my $dec_rad = DD2R*$row->[2];
        $values{pointing} = $row->[0];
	$count++;
	my ($l2_rad,$b2_rad);
	( ! slaEqgal($ra_rad,$dec_rad,$l2_rad, $b2_rad) ) or
	    die "Cannot convert RA/DEC to l/b ? \n";
	next if (abs($b2_rad) < $gal_lat_limit*DD2R );
	
	## now convert the RA/DEC into Ecliptic Coords
	my ($long_rad,$lat_rad);
	(! slaEqecl($ra_rad,$dec_rad,$start_date,$long_rad,$lat_rad)) or 
	    die "Cannot convert RA/DEC to ECLIPTIC? ($ra_rad,$dec_rad)\n ";
	
	## Determine, roughtly, the number of days until this field comes to 
	## oppotition.
	my $elongation = ( $long_rad - $long_sun)*DR2D;
	$elongation += 360.0 if ( $long_rad < $long_sun ) ;
	my $opp_date = $elongation < 180 ? $elongation + 180 : $elongation  - 180 ;
	
	## But, the PreDiscover starts ~70 degrees (About 70 days) before opposition.
	my $delta = int($opp_date - 75) + $mjd_midnight ;
	
	## loop forward till field leaves the trailing window
	foreach my $range ( 'opposition', 'trailing', 'leading' ) {
	    foreach my $circ ( '_start', '_end' ) {
		my $t = "$range$circ";
		$values{$t} = 0;
	    }
	}
	## stick this into the DB
	
      DAY: for ( my $day = $start_date+$delta; $day < $start_date+$delta+155; $day++) { 
	  slaDjcl( $day, my $y , my $m, my $d, my $frac, my $stat) ;
	  ## Determine the RA/DEC of the Sun.
	  slaRdplan($day, 10, LONG, LAT, my $ra_sun_rad, my $dec_sun_rad, my $diam);
	  
	  ## Determine the lat/long of the Sun.
	  slaEqecl( $ra_sun_rad, $dec_sun_rad, $day, my $long_sun_rad, my $lat_sun_rad);
	  
	  ## get the fields elongation
	  my $elongation = slaRanorm($long_rad - $long_sun_rad)*DR2D;
	  slaCr2tf ( 1, $ra_sun_rad, my $ra_sign, my @ra_hms);
	  ## does this date fall in an observable window
	  foreach my $range ( 'opposition' , 'trailing', 'leading' ) {
	      my $start = $range."_start";
	      my $end = $range."_end";
	      if ( $elongation < $elong{$start} && $elongation > $elong{$end} ) {
		  ## is this field visible for more then 2.5 hours?
		  my $vis = visible($day, $ra_sun_rad, $dec_sun_rad, $ra_rad, $dec_rad);
		  #printf STDERR "%02d:%02d:%02d:%01d  ",@ra_hms;
		  #print STDERR "$elongation $y/$d/$m $vis\n";
		  next DAY if ( visible($day, $ra_sun_rad, $dec_sun_rad, $ra_rad, $dec_rad) < 2.5 ) ;
		  $values{$start} = $day if ( $values{$start} == 0 || 
					      ( $day < $values{$start} && 
						( $day < $values{$end} || $day -365 < $values{$end}) 
						)
					      ) ;
		  $values{$end} = $day if ( $values{$end} ==0 || 
					    ( $day - $values{$start} < 180 && 
					      $day > $values{$start} && 
					      $day > $values{$end}
					      )
					    );
	      }
	  }
      }
	#convert MJD to YYYY-MM-DD 
	foreach my $range ( 'opposition', 'trailing', 'leading' ) {
	    foreach my $circ ( '_start', '_end' ) {
		my $t = "$range$circ";
		#print "$t $value{$t}   ";
		slaDjcl( $values{$t}, my $y , my $m, my $d, my $frac, my $stat) ;
		$values{$t} = sprintf "%04d-%02d-%02d",$y,$m,$d;
		$y > 2002  or warn "Year range not reasonable field-> $values{id},$t,$values{$t}\n";
	    }
	}
	## stick this into the DB
	my $sql = "INSERT INTO `vwfields` ";
	my $sep = "";
	my $keys;
	my $values;
	foreach my $key ( @keys ) {
	    $keys .= sprintf "$sep `%s`",$key;
	    $values .= sprintf "$sep '%s'",$values{$key};
	    $sep = ",";
	}
	$sql .= " ( $keys ) VALUES ( $values ) ;\n";
	#print $sql;
	$mysql->query($sql);
	slaCr2tf ( 1, $ra_rad, my $ra_sign, my @ra_hms);
	slaCr2tf ( 1, $dec_rad, my $dec_sign, my @dec_hms);
	print "@ra_hms @dec_hms $values \n";
	#print " inserting $count \r";
	#$field->insert();
    }
}
$mysql->close();



## Should calculate if the field is above the horizon for 
## more than x hours.
sub visible($$$$$) {
    # double slaZd ( double ha, double dec, double phi )
    # double slaAirmas ( double zd )
    # void slaRdplan ( double date, int np, double elong, double phi,
    #            double *ra, double *dec, double *diam )
    my $up = 0 ;
    my $sun_set=0;
    my ($date, $ra_sun, $dec_sun , $ra, $dec ) = @_;
 
    ## sunset occurs when increasing HA pushes the sun below the horizon.
    ## sunrise occurs when increasing HA pushes the sun above the horizon.
    ## Also need the moon the moon to be more that 40 degrees from the
    ## field.

    for ( my $time = 0; $time < 2*DPI; $time += 0.1*(2*DPI)/24 ) {
	my $lst = slaRanorm(slaGmsta($date,$time/D2PI)-(10.0*DPI/12.0)-(22.0*DPI/60.0/12.0));
	my $ha_sun = $lst - $ra_sun;
	my $zd_sun = slaZd($ha_sun,$dec_sun,LAT);
        slaCr2tf ( 1, $ha_sun, my $sign, my @sunhms);
        slaCr2tf ( 1, $lst, $sign, my @lsthms);
        #printf STDERR "%f %f %02d:%02d:%02d.%01d %02d:%02d:%02d.%01d \n",$date,$time/D2PI,@sunhms,@lsthms;
	if ( $zd_sun > (90+15.0)*DD2R ) {
	    
	    ## sun is set, is the object up?
	    my $ha = $lst - $ra;
	    my $airmass = slaAirmas(slaZd($ha,$dec,LAT));
 	    if ( $airmass < 1.8 ) {
	      ## where is the moon
	      my $mj_date = $date + $time/D2PI ; 
              slaRdplan($mj_date,3,LONG,LAT,my $moon_ra,my $moon_dec,my $diam);
	      my $ha_moon = $lst - $moon_ra;
	      my $zd_moon = slaZd($ha_moon,$moon_dec,LAT);
	      my $obj_moon_sep = slaSep($ra,$dec,$moon_ra,$moon_dec);
	      my $sun_moon_sep = slaSep($ra_sun,$dec_sun,$moon_ra,$moon_dec);
              slaCr2af ( 1, $zd_moon, my $sign, my @hms);
              slaCr2tf ( 1, $ra, my $ra_sign, my @ra_hms);
	      if ( $zd_moon > (90*DD2R) || ($obj_moon_sep > 10*DD2R + 1.5*$sun_moon_sep) ) { 
		   ## && $obj_moon_sep  [Not clear what this was here for... ]
	      #if ( $sep*DR2D > 70.0 ) { 
		  $up++;
		  #printf STDERR "no_Moon %02d:%02d:%02d.%02d $sign%02d:%02d:%02d.%01d %f %f\n",@ra_hms,@hms,$mj_date,$obj_moon_sep*DR2D;
              }else{
		  #printf STDERR "Moon %02d:%02d:%02d.%02d $sign%02d:%02d:%02d.%01d %f %f\n",@ra_hms,@hms,$mj_date,$obj_moon_sep*DR2D;
	      }
	    }
	} 
    } 
    #print $up*0.1;
    #exit if ( $up > 30 ) ;
    return ( $up*0.1 );
 
}


