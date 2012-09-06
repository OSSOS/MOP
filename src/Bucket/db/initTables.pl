#!/usr/bin/env perl

### create the required mySQL tables for bucket if they don't already exist
use Bucket::MOP;

use Term::ReadKey;
use Getopt::Long;
use Pod::Usage;

use warnings;
use strict;
use Carp;

my $help = 0;
my $table = "";
my $debug = 0;
my $init  = 0;

GetOptions ('h|help|?' => \$help,
	    'd|debug' => \$debug,
	    't|table=s' => \$table,
	    'i|initial' => \$init
	    );

pod2usage() if ( $help);

my @tables = ( "received" , "status", "wcs", "measure", "exposure", "object", "source" );


## the initTables script must have tty access
my $on_a_tty = -t STDIN && -t STDOUT;
die "Must be run from an interactive terminal" unless $on_a_tty;

## check if they really want to initialize the database
if ( $init ) {
    if ( $table ) {
	print "Do you really want to initialize the table $table, [YES/no]? ";
    } else { 
	print "Do you really want to initialize ALL THE TABLES? [YES/no]? ";
    }
    my $ans = <STDIN>;
    chomp $ans;
    die "OK, I'll quit" unless ( $ans =~ m/YES/ ) ;
}

## get a connection to the DB.

## do we need to do this for one table or all of them?
if ( $table ) {
    my $thisTable = Bucket::MOP->new($table);
    $thisTable->dropTable() if ($init);
    $thisTable->createTable();
} else { 
    foreach my $table ( @tables ) {
	my $thisTable = Bucket::MOP->new($table);	
	$thisTable->dropTable() if ($init);
	$thisTable->createTable();
    }
}

__END__

=head1 SYNOPSIS

B<initTables.pl>: create the tables  needed for MOP.


=head1 USAGE

B<initTables.pl> [--initial] [--table tablename]

=head1 OPTIONS

=over 8

=item B<--initial>

Drop the tables if they exist and then create new versions of them.  

=item B<--table tablename>

Only create/initialize the named table

=back

=head1 DESCRIPTION

This script is used to run through the tables needed for the Bucket
software.  If you call the script with B<--initial|-i> then the
previous versions of the table are dropped.


