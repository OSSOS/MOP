#!/usr/bin/env perl
### given the bounds of the QUEUE runs precompute the number of nights
### each field of CFHTLS-VW is visible for

use Net::MySQL;
require 'getType.pl';
use strict;

my $mysql = Net::MySQL->new(
                            database => 'bucket',
#			    host=> 'cadchd.hia.nrc.ca',
#                            port=> 33306,
                            user => 'lsadmin',
                            password => '***REMOVED***'
                            ) ||die "Cann't connect to DB";

### build a list of all the queue runs
my $sql = "SELECT runid FROM runs WHERE runid LIKE '06%'  ";
$mysql->query($sql);

my $record_set = $mysql->create_record_iterator;
my @runs;
while ( my $record = $record_set->each ) {
	push @runs, $record->[0];
}

### for each run compute number of nights the run is available and insert 
### the result into the apropriate table
foreach my $run ( @runs ) { 
    foreach my $type ( "opposition", "leading", "trailing" ) {
	my $delete = "DELETE FROM `$type` WHERE runid LIKE '$run' ;";
	print $delete;
	#$mysql->query($delete);
	my $start = $type."_start";
	my $end = $type."_end";
	my $sql =  "INSERT INTO `$type` ( id, pointing, runid, Nights ) ";
	$sql .= " SELECT v.id, v.pointing, r.runid,  ";
	$sql .= " least( to_days( EndDate ) , to_days( $end )  )  - ";
	$sql .= " greatest( to_days( StartDate ) , to_days( $start )  )  +1 AS Nights";
	$sql .= " FROM  `vwfields` v ";
	#$sql .= " LEFT JOIN `$type` t ON v.id=t.id";
	$sql .= ", `runs` r";
	$sql .= " WHERE least( to_days( EndDate ) , to_days($end )  )  - greatest( to_days( StartDate ) , to_days( $start )  )  +1 >3 and r.runid like '$run' ;";
	#$mysql->query($sql);
	print " $sql \n";
    }
}

