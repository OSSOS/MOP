#!/usr/bin/env perl

         use strict;
         use DBI();

         # Connect to the database.
my $dbh = DBI->connect("DBI:mysql:database=bucket;host=localhost;mysql_socket=/tmp/mysql.sock", "lsadmin", "shift+add", {'RaiseError' => 1});
         # Now retrieve data from the table.
         my $sth = $dbh->prepare("SELECT * FROM exposure LIMIT 10 ");
         $sth->execute();
         while (my $ref = $sth->fetchrow_hashref()) {
	 	print $ref->{'expnum'}, $ref-{'object'};
         }
         $sth->finish();

         # Disconnect from the database.
         $dbh->disconnect();
