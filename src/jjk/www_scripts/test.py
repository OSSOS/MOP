#!/usr/bin/env perl 

use Net::MySQL ;


$sql="SELECT * FROM bucket.exposure LIMIT 10 " ;


my $hostname="sciproc3";
my $port=3306;
my $database="bucket";
my $user="lsadmin";
my $password="***REMOVED***";


my $mysql = Net::MySQL->new(
			    hostname => $hostname,
			    port => $port,
			    database => $database,
			    user => $user,
			    password => $password
			    ) ||die "Cann't connect to DB";


$mysql->query($sql);

my $record_set = $mysql->create_record_iterator;
         while (my $record = $record_set->each) {
             printf "First column: %s Next column: %s\n",
                 $record->[0], $record->[1];
         }

