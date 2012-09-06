#!/usr/bin/perl 
## get the list of LS-VW expousres.. insert entries in the LSVW mysql DB on cadchd.


use DBD::mysql;
#use Bucket::MOP;
use strict;
use warnings;
use Term::ReadKey;


my $hostname="cadchd.hia.nrc.ca";
$hostname="sciproc3";
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
$password=<STDIN>;
chomp $password; 
print "\n"; 
ReadMode 1;

chomp $password;
}

my $mysql = DBI->connect("DBI:mysql:database=bucket;host=localhost;mysql_socket=/tmp/mysql.sock", "lsadmin", "shift+add", {'RaiseError' => 1});


#my $mysql = Net::MySQL->new(
#			    hostname => $hostname,
#			    port => $port,
#			    database => $database,
#			    user => $user,
#			    password => $password
#			    ) ||die "Cann't connect to DB";
#

my @cols=("expnum","object","ra","dec","exptime","mjdate","filter","runid","qrunid","date","uttime");



### get the list of exposures from the CADC and insert them into the db
my $webhost='cadc.hia.nrc.ca';
#### for inside the cadc you need to set this to..
$webhost='haisla';
my $url = "http://$webhost/cadcbin/cfhtlsvw/exposure.pl ";
print STDERR "Getting exposure table from CADC\n $url\n";
open(EXP,"/usr/bin/curl --silent $url | ");
print STDERR "Scanning list for new exposures \n";
while (<EXP>) {
    my @values=split;
    
    my $expnum = $values[0];
    my $mjdate = $values[5]; 
    print STDERR "checking if $expnum is new        ";
    ### First check if we've alread got an entry for this exposure.. if so then skip it.
    my $sql = "SELECT count(*) from bucket.exposure where expnum=$expnum ";
    my $sth = $mysql->prepare($sql);
    die "\n Bad query \n $sql \n" unless  $sth;
    $sth->execute();
    my @ref = $sth->fetchrow_array();
    $sth->finish();
    my $count = $ref[0];
    if ( $count > 0 ) {
	print STDERR "$expnum \r";
	next;
    }
    
    ### now add to the exposure table too.
    $sql = "INSERT INTO `exposure` (";
    my $sep = '';
    foreach my $col ( @cols ) {
	$sql .= $sep."`".$col."`";
	$sep=",";
    }
    $sep ="";
    $sql .= " ) VALUES ( " ;
    foreach my $value ( @values ) {
	$sql .= $sep."'".$value."'";
	$sep=",";
    }
    $sql .= " ) ";
    print STDERR "  Adding $values[0] \n";
    $sth= $mysql->prepare($sql);
    $sth ||die "Bad expousre insert, ($sql)\n";
    $sth->execute();
    $sth->finish();

}

$mysql->disconnect;
