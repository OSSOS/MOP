#!/usr/bin/env perl 

use warnings;
#use Net::MySQL;
use DBD::mysql;
use strict;
use Term::ReadKey;



sub getType {
use strict;
use Astro::SLA;
use Math::Trig;

my $mjdate = shift;
my $ra = shift;
my $dec = shift;

use constant LAT => 0.344;  #lat. of CFHT [rad]
use constant LONG => -2.707;  #long. of CFHT[rad]

### JD fraction at midnight in Hawaii.
my $mjd_midnight = 1.0-0.0833;

my %elong;
$elong{'DiscoverEnd'} = 160;
$elong{'DiscoverStart'} = 200;
$elong{'PreDiscStart'} = 250;
$elong{'PreDiscEnd'} = 235;
$elong{'CheckupStart'} = 125;
$elong{'CheckupEnd'} = 110;


    ## Work out the position of the sun 
    slaRdplan($mjdate,10,LONG,LAT, my $ra_sun, my $dec_sun, my $diam_sun);
    ## Determine the lat/long of the Sun.
    slaEqecl( $ra_sun, $dec_sun, $mjdate, my $long_sun, my $lat_sun);
    ## Determine the lat/long of this field.
    slaEqecl( DD2R*$ra, DD2R*$dec, $mjdate, my $long, my $lat ) ;
    my $elongation = slaRanorm($long - $long_sun)*DR2D;
return $elongation;
#    foreach my $type ( 'Checkup', 'PreDisc', 'Discover' ) { 
#     my $t1 = $type."Start";
#     my $t2 = $type."End";
#     return $type if ( $elong{$t1} > $elongation && $elong{$t2} < $elongation ) ;
#    }


}


my $hostname="cadchd.hia.nrc.ca";
$hostname="gimli";
my $port=33306;
$port=3306;
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

if ( !$password ) { 
print STDERR "Please enter the DB password for $user on $hostname: ";

ReadMode 3; 
my $password=<STDIN>;
chomp $password; 
print "\n"; 
ReadMode 1;

chomp $password;
}


#my $mysql = Net::MySQL->new(
#                            hostname => $hostname,
#                            port => $port,
#                            database => $database,
#                            user => $user,
#                            password => $password
#                            ) ||die "Cann't connect to DB";
#

my $mysql = DBI->connect("DBI:mysql:database=bucket;host=localhost;mysql_socket=/tmp/mysql.sock", "lsadmin", "***REMOVED***", {'RaiseError' => 1});


my $sth=$mysql->prepare(q{SELECT mjdate,e.ra,e.dec,e.expnum FROM exposure e LEFT JOIN circumstance c ON e.expnum=c.expnum WHERE c.elongation IS NULL });
$sth->execute();
#my $record_set = $mysql->create_record_iterator;
while ( my $record = $sth->fetchrow_arrayref() ) {
    my $mjdate = $record->[0];
    my $ra = $record->[1];
    my $dec = $record->[2];
    my $expnum = $record->[3];
    my $elongation = getType($mjdate,$ra,$dec);
    my $mode = $elongation < 210 && $elongation > 150 ? 1 : 
		( $elongation < 260 && $elongation > 225  ? 3 :
		( $elongation < 135 && $elongation > 100 ? 2 : 0 ) ) ;
    my $sql = "INSERT INTO `circumstance` (`expnum`,`elongation`,`mode`) VALUES ($expnum, $elongation,$mode) ";
    print STDERR $sql."\n" ;
    $mysql->do($sql);
}


$mysql->disconnect();
