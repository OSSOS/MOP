#!/usr/bin/env perl
## Association each expnum with a VW pointing.
## run on the host that has the DB installed

use DBD::mysql;
use warnings;
use strict;
use Term::ReadKey;


### max allowed speration to consider this to be the same pointing
my $rad_max = 0.54; 

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

if ( ! $password ) {
print STDERR "Please enter the DB password for $user on $hostname: ";

ReadMode 3; 
my $password=<STDIN>;
chomp $password; 
print "\n"; 
ReadMode 1;

chomp $password;

}

my $mysql = DBI->connect("DBI:mysql:database=bucket;host=localhost;mysql_socket=/tmp/mysql.sock", "lsadmin", "***REMOVED***", {'RaiseError' => 1});
#my $mysql = Net::MySQL->new(
#                            hostname => $hostname,
#                            port => $port,
#                            database => $database,
#                            user => $user,
#                            password => $password
#                            ) ||die "Cann't connect to DB";

### get the list of exposures that don't have associations
my $sql = "SELECT e.expnum,radians(`ra`),radians(`dec`),object FROM exposure e LEFT JOIN association a ON e.expnum=a.expnum WHERE a.pointing IS NULL";
my $sth = $mysql->prepare($sql);
die "Bad Query ($sql) " unless $sth;
$sth->execute();

while ( my $row = $sth->fetchrow_arrayref() ) {
    ### figure out which VW this is associated with
    my $expnum = $row->[0];
    my $ra  = $row->[1];
    my $dec = $row->[2];
    my $object=$row->[3];

    ### compute the angular seperations between this field's pointing 
    ### and that of other exposures already in the DB
    my $x1 = cos($ra)*cos($dec);
    my $y1 = sin($ra)*cos($dec);
    my $z1 = sin($dec);

    my $table = "association as a JOIN exposure as e ON a.expnum=e.expnum JOIN pointings p ON a.pointing=p.id";
    my $cols = " pointing, name ";
    $cols .= ",degrees(acos(cos(radians(avg(e.ra)))*cos(radians(avg(e.dec)))*$x1 + sin(radians(avg(e.ra)))*cos(radians(avg(e.dec)))*$y1 + sin(radians(avg(e.dec)))*$z1))  d";
    my $where = " group by a.pointing order by d limit 1";
    my $ass_sql = " SELECT $cols,avg(e.ra),avg(e.dec) FROM $table $where ";


    my $sth2=$mysql->prepare($ass_sql);
    die "Bad Query ($ass_sql) " unless $sth2;
    ### print STDERR "$ass_sql \n";
    $sth2->execute();
    #my $set = $mysql->create_record_iterator;     
    my $pointing=0;
    

    my $this_row = $sth2->fetchrow_arrayref();
    $sth2->finish();
    if ( $this_row->[0] )  { 
	if ( $this_row->[2] < $rad_max ) { 
	    $pointing=$this_row->[0];
	}else{
	    ### OK..not close by field already in the association table, so get an RA/DEC from the pointing table....
	    $cols = " id, name ";
	    $cols .= ",degrees(acos(cos(radians(avg(p.ra)))*cos(radians(avg(p.dec)))*$x1 + sin(radians(avg(p.ra)))*cos(radians(avg(p.dec)))*$y1 + sin(radians(avg(p.dec)))*$z1))  d";
	    $ass_sql = "SELECT $cols FROM pointings p group by p.id order by d limit 1";
	    my $sth3 = $mysql->prepare($ass_sql);
	    die "bad query $ass_sql " unless $sth3;
	    $sth3->execute();
	    if ( my $next_row = $sth3->fetchrow_arrayref()) {
	        if ( $next_row->[2] < $rad_max ) { 
		    $pointing=$next_row->[0];
		} else {
		    ### No nearby pointing either.. create a new one
		    $sql="INSERT INTO pointings ( name, ra, `dec` ) VALUES (  '$object', degrees($ra), degrees($dec) )";
		    $mysql->do($sql);
		    $pointing= $mysql->{'mysql_insertid'};
		}
	    } else { 
		$pointing='NULL';
	    }
	    $sth3->finish();
	}
    } else { 
	$pointing='NULL';
    }
    $sql = "INSERT INTO association SET expnum=$expnum, pointing=$pointing ";
    my $sth4 = $mysql->prepare($sql);
    #print $sql,"\n";
    die "Failed to insert into association " unless	$sth;
    $sth4->execute();
    $sth4->finish();
}

$mysql->disconnect();
