package Bucket::Table;

use 5.008;
use strict;
use warnings;
use Carp;
use DBI;


require Exporter;
use AutoLoader qw(AUTOLOAD);

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use Bucket::Table ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(

) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);


our $VERSION = sprintf "%d.%03d", q$Revision: 1.4 $ =~ /(\d+)/g;

# Preloaded methods go here.

use Term::ReadKey;

sub new {
    ### create a Table class object.  This is a custom class created
    ### for accessing/creating tables used by MOP
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    $self->{_NAME} = shift ;
    ### grab the user and possilbly the password as options
    my $user = shift if @_;
    my $password = shift if @_;

    ## if the user name and password 
    ## didn't come from the calling routing then ask for them
    ## here.  if only user is give then ask for that users password.
    if ( ! $password || ! $user) { 
	if ( !$user ) { 
	    ### turn on echo
	    ReadMode 1; 
	    print STDERR "db username: "; 
	    $user = <STDIN>;
	    chomp $user; 
	} 
	if ( !defined($password) ) {
	print STDERR "db password for $user  "; 
	ReadMode 3; ### turn off echo
	$password = <STDIN>; 
	chomp $password; 
	print "\n"; 
	ReadMode 1;  ### turn on echo
	}
    }
    my $driver_name="mysql";
    $self->{"_DB"} = "cfeps";
    DBI->install_driver($driver_name);
    ##  open the DB connection and store in the OO hash
    my $host='cadchd.hia.nrc.ca:33306';
    if ($ENV{'IMAGE_BASE_DIR'}) {
       $host=$ENV{'MYSQL_HOST'};
    } 
    $self->{"_DBH"} = 
	DBI->connect( "DBI:mysql:database=".$self->{_DB}.";host=$host", 
		      $user,$password, {'RaiseError' => 0, 'PrintError' => 1} ) ;
    ## bless the object and return to the caller
    bless($self, $class);
} 



## create a give CFEPS table.
sub createTable {
    my $self = shift;
    my $thisTable = $self->{_NAME};
    ## check that we know how to build this kind of table
    my %thisTable = %{$self->{_DEFS}};
    my $sql = "CREATE TABLE IF NOT EXISTS $thisTable ( ";
    my $sep = "";
#    foreach my $col ( @{$self->{_COLS}} ) {
    foreach my $col ( @{[keys %{$self->{_DEFS}}]} ) {
	$sql .= $sep;
	$sql .= " $col $thisTable{$col} ";
	$sep = ",";
    }
    ### if there is an _INDEX defined for this table then add
    ### the definition to the create statement
    $sql .= $sep.$self->{_INDEX} if $self->{_INDEX};
    $sql .= " )" ;
    my $dbh = $self->{"_DBH"};
    ## catch any errors during the transaction.
    eval { $dbh->do("$sql") } ;
    carp "Error creating table $thisTable ($sql): $@\n" if $@;
    return $@;
}


## fetch a row associated with a give where clause. 
## return the number of rows found.
sub selectRows {
    my $self=shift;
    my $where = "";
    my $groupby = "";
    my $orderby = "";
    my $limit = "";
    my $args = shift if @_;
    ### if the second argument is a HASH then use that.
    if ( ref $args eq "HASH" ) {
	$where = "WHERE ".$args->{WHERE} if ( $args->{WHERE} ) ;
	$groupby = "GROUP BY ".$args->{GROUP} if ( $args->{GROUP} );
	$orderby = "ORDER BY ".$args->{ORDER} if ( $args->{ORDER} );
	$limit = "LIMIT ".$args->{LIMIT} if ( $args->{LIMIT} ) ;
    } else { 
	$where = "WHERE ".$args if ($args);
    }
    my $dbh = $self->{_DBH};
    my $sth;
    ## first count the number of entries.. 
    my $sql = "SELECT COUNT(*) N from ".$self->{"_NAME"};
    my $ext = " ".$where." ".$groupby." ".$orderby." ".$limit;
    $sql .= $ext;

    eval { 
	$sth = $dbh->prepare("$sql");
	$sth->execute;
    } ;
    carp "Error on fetchRows for ".$self->{"_NAME"}.": $@ \n" if $@;
    my $row = $sth->fetchrow_hashref();
    
    ## Create a query that returns the actual rows.
    $sql = "SELECT ",;
    my $sep = "";
    ## get the cols in the desired order
    ## and create an array to bind the results to
    ## bind is packed with references to the hash for
    ## the table.
    my @bind = ();
    foreach my $col ( @{$self->{_COLS}} ) {
	$sql .= $sep.$col ;
	$sep = ",";
	push @bind, \$self->{$col};
    }
    $sql .= " FROM ".$self->{"_NAME"}." ".$ext;
   
    eval { 
	$sth = $dbh->prepare("$sql");
	$sth->execute;
	$sth->bind_columns(@bind);
    } ;
    carp "Error on selectRows for ".$self->{"_NAME"}.": $@ \n" if $@;
    ## store the statement handle to be stepped over else where
    $self->{_STH} = $sth;
    return $row->{'N'};
}


### fetch the next row for query.
sub fetchRow {
    my $self=shift;
    my $sth = $self->{_STH};
    my $row = $sth->fetchrow_arrayref if $sth;
    return $row;
}

## give the name of the table, check to see if the table name is valid
## and drop that table from the db if the name is valid.
sub dropTable {
    my $self = shift;
    my $thisTable = $self->{_NAME};
    ## check that cfeps contains this table
    my $sql = "DROP TABLE IF EXISTS $thisTable ";
    my $dbh = $self->{"_DBH"};
    ## catch any errors during the transaction.
    eval { $dbh->do("$sql") };
    carp "Error droping table $thisTable ($sql): $@ \n" if $@;
    return $@;
}

## insert a given row into a CFEPS table.
## an hash is sent over with a "column" => "value" 
## structure.
sub insertRow {
    my $self = shift;
    my $row = @_ ? shift : $self;
    my %row = %$row;
    my $thisTable = $self->{_NAME};
    my @cols = @{$self->{_COLS}};
    my $sep = "";
    my $vals = "";
    my $cols = "";
    foreach my $col ( @{[keys %{$self->{_DEFS}}]} ) {
	if ( defined $row{$col} ) { 
	    $cols .= $sep.$col;
	    $vals .= $sep."\'".$row{$col}."\'";
	    $sep = ",";
	}
    }
    my $sql = "INSERT INTO $thisTable ($cols) VALUES ($vals) ";
    my $dbh = $self->{"_DBH"};
    my $result=0;
    eval { 
	$result=$dbh->do("$sql");
    } ;
    return $result;
}

sub deleteRow {
    my $self=shift;
    my $where = @_ ? shift : 0;
    return unless $where ;
    my $sql = "DELETE FROM ".$self->{_NAME}." WHERE $where ";
    my $dbh = $self->{"_DBH"};
    return $dbh->do($sql);
}

sub updateRow {
    my $self = shift;
    my $row = @_ ? shift : $self;
    my %row = %$row;
    my $where = @_ ? shift : "file_id LIKE \'".$row->{file_id}."\'";
    my $thisTable = $self->{_NAME};
    my @cols = @{$self->{_COLS}};
    my $sep = "";
    my $vals = "";
    my $cols = "";
    foreach my $col ( @cols ) {
	if ( $row{$col} ) {
	  $vals .= $sep.$col."=\'".$row{$col}."\'";
	  $sep = ",";
	}
    }
    my $sql = "UPDATE $thisTable SET $vals WHERE ".$where ;

    my $dbh = $self->{"_DBH"};
    my $result=0;
    eval { 
	$result=$dbh->do("$sql");
    } ;
    return $result;
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Bucket::Table - Perl extension for accessing the CFEPS db system

=head1 SYNOPSIS

  use Bucket::Table;
  

=head1 ABSTRACT

  The CFEPS stores information about processing and results in mySQL
  tables.  These routines create/insert/get info from these tables

=head1 SUBROUTINES

=over 8

=item B<new($table,$hash,$user,$password)> Creates a new connection to the CFEPS
db. if user name and password are not  supplied then query for them.

=item B<createTable($table)> Creates the given table in the db.  The
structure of the tables is defined in the source code. 

=item B<dropTable($table)> Drop $table from the CFEPS db.  B<CAUTION> This
deletes all the info stored in the table.

=back 

=head2 EXPORT

None by default.



=head1 SEE ALSO

The source code.

=head1 AUTHOR

JJ Kavelaars

=head1 COPYRIGHT AND LICENSE

Copyright 2004 by CFEPS account

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
