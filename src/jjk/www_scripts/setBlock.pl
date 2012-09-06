#!/usr/bin/env perl 

use warnings;
use Net::MySQL;
use strict;
use Term::ReadKey;

my $hostname="cadchd.hia.nrc.ca";
$hostname="localhost";
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
close DBRC;

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

### get the list of exposures that don't have associations
my $sql = "SELECT pointing,qrunid,e.expnum,radians(`ra`),radians(`dec`),mjdate FROM exposure e JOIN association a ON e.expnum=a.expnum LEFT JOIN blocks b ON e.expnum=b.expnum WHERE b.block IS NULL order by e.expnum";

my $result = $mysql->query($sql);
print $mysql->query($sql);


die "Bad Query ($sql) " unless $mysql->has_selected_record;
my $rows = $mysql->create_record_iterator;     


my $block='';
my $qname='';
my $freeze=1;
my $this_night=0;

while ( my $row = $rows->each ) {
    my $pointing = $row->[0];
    my $qrunid = $row->[1];
    my $expnum = $row->[2];
    my $night  = $row->[5]-0.0833;
    ### now check to see if this pointing has a block associated with it...
    $sql = "SELECT block,qname FROM blocks b join association a on b.expnum=a.expnum where a.pointing=$pointing";
    $mysql->query($sql);
    die "Bad SQL ($sql ) " unless $mysql->has_selected_record;
    my $rec_set = $mysql->create_record_iterator;
    my $record = $rec_set->each;
    if ( $record ) {
	$this_night=0;
	### Ok this pointing is from a known block... so make the connection.
	$block=$record->[0];
	$qname=$record->[1];
    } elsif ( $this_night != int($night) ) {
	$this_night=int($night);
	$freeze=0;
	### This is a new pointing... so add a new block type
	$sql = "SELECT count(distinct block) from blocks";
	$mysql->query($sql);
	$mysql->has_selected_record || die "Bad select query ($sql) \n";
	my $rec_set = $mysql->create_record_iterator;
	my $record=$rec_set->each;
	my $bnum=$record->[0];
	$bnum++;
	$block = "B".$bnum;
	$sql = "SELECT count(distinct qname) from blocks where qname like '%$qrunid%' " ;
	$mysql->query($sql);
	$mysql->has_selected_record || die "Cann't even count? ($sql) \n";
	my $rec_set2 = $mysql->create_record_iterator;
	my $record2=$rec_set2->each;
	my $alpha=$record2->[0];
	my @letter=("A","B","C","D","E","F","G","H","I");
	$qname=$qrunid."$letter[$alpha]";
    }
    $sql = "INSERT into blocks SET block='$block', qname='$qname', expnum=$expnum" ;
    $mysql->query($sql);
    $mysql->get_affected_rows_length ==1 || die "Bad insert result ($sql) \n";
}
