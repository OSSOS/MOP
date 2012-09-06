package Bucket::MOP;

### The MOP package is a set of subroutines that define the 
### table structure specific to the Moving Object Pipeline.
###
### each table has it's own specific subrouting that is called 
### to define the columns for that table.

### use strict;  ### Turned of strict, cann't figure a way arounds this
use warnings;
use Carp;
use Bucket::Table;
require Exporter;

our $VERSION = sprintf "%d.%03d", q$Revision: 1.4 $ =~ /(\d+)/g;

our @ISA = ("Bucket::Table");

our %EXPORT_TAGS = ( 'all' => [ qw(
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
		 $fitradec
);

our $err=0;

### Command line to send lines of astrometry to 
our $observatories = "/Users/jjk/Projects/Software/orbfit/observatories.dat";
our $binEphem = "/Users/jjk/Projects/Software/orbfit/binEphem.405";
our $fitradec = "fit_radec -o $observatories -j $binEphem" ;
$fitradec = "fit_radec";
our $predict = "predict -o $observatories -j $binEphem";
$predict = "predict";

### predict the location of a KBO given an abg file, date and obs-code..
sub predict {
    use File::Temp qw/ tempfile tempdir /;
    use Astro::SLA;
    my $abg = shift;
    my $date = shift;
    my $code = shift;
    ## send the date and obs code to predict.                                                                            

    $date = $date+2400000;
    my ($wfh,$filename) = tempfile('tempXXXXX',SUFFIX=>'.pre');
    print $wfh "$code\n$date\n-1\n";
    #print "$code\n$date\n";
    close $wfh;
    #print "$predict $abg < $filename 2>>/dev/null | grep ICRS |\n";
    my $pid = open(PREDICT,"$predict $abg < $filename 2>>/dev/null | grep ICRS |");
    my $line = <PREDICT>;
    close(PREDICT);
    unlink $filename;
    chomp $line;
    my ($dum1,$dum2,$ra,$dec,$dra,$ddec,$ang)  = split(' ',$line);
    $ra =~ s/:/ /g;
    $dec =~ s/:/ /g;
    #print "$ra, $dec\n";
    my $i=1;
    my $status=0;
    my $ra_rad;
    my $dec_rad;
    slaAfin($ra,$i,$ra_rad,$status);
    $ra_rad = $ra_rad*15.0;
    $status == 0 || die "Error converting $ra to radians: code $status\n";
    $i =1;
    $status=0;
    slaAfin($dec,$i,$dec_rad,$status);
    $status == 0 || die "Error converting $dec to radians\n";
    return ($ra_rad,$dec_rad,$dra,$ddec,$ang );
}
    

## Determine the elonation of a particular RA/DEC to determine the type
## of observation [PreDiscovery,Discovery,Checkup,FirstYear,3rdOpposition]
## Pre-Discovery and Checkup of CFHTLS-VW observations.
##
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

}

### initialize using the standard TABLE method
sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);
    my $init = $self->{_NAME};
    $self->$init if $init;
    return $self;
}


sub unlock {
    ### release the file for other uses on the same task.
    my $self = shift;
    my $status= Bucket::MOP->new("status");
    $status->{"task"} = shift;
    $status->{"file_id"} = $self->{"file_id"};
    $status->{"success"} = @_ ? shift : 0 ;
    $status->{"comment"} = @_ ? shift : "none";
    my $tmp = `hostname`;
    chomp $tmp;
    $status->{"hostname"} = $tmp;
    $status->{"insertdate"} = undef;;
    ### update the status row for
    ### this file_id, this task... file must previous be locked
    my $where = " file_id LIKE \'".$status->{file_id}."\' AND ";
    $where .= " task LIKE \'".$status->{task}."\' AND ";
    $where .= " comment LIKE 'locked' "; 
    return ($status->updateRow($status,$where));
}


sub lock {
    ### insert a 'Lock' line in the status table for the given
    ### file_id and task;
    my $self = shift;
    my $task = shift;
    my $file_id = $self->{"file_id"};
    my $status=Bucket::MOP->new("status");
    my $statusQuery = Bucket::MOP->new("status");
    $status->{"file_id"}=$self->{"file_id"};
    $status->{"task"}=$task;
    $status->{"comment"}="locked";
    my $hostname =`hostname`;
    chomp $hostname;
    $status->{"hostname"}=$hostname;
    $status->{"success"}=0;
    $status->{"insertdate"}=undef;
    ### check if the file is already locked.
    my $where1 = "task LIKE '".$task."'" ;
    $where1 .= " AND file_id LIKE '".$file_id."'";
    my $where2 = " AND comment LIKE 'locked' ";
    my $rows = $statusQuery->selectRows($where1.$where2);
    if ( $rows != 0 ) {
	### file is already locked.
	$statusQuery->fetchRow;
	warn "$file_id is already locked by processor ".$statusQuery->{"hostname"};
	return 0;
    } 
    ### what kind of entry is there?
    $where2 = " AND comment NOT LIKE \'locked\' " ;
    $rows = $statusQuery->selectRows($where1.$where2);
    if ( $rows == 1 ) {
	return $status->updateRow($status,$where1.$where2);
    } elsif ( $rows==0 ) {
	return $status->insertRow;
    } else {
	return -1;
    }
}


sub getTriple {
    my $triples = Bucket::MOP->new("exposure");
    my $sql = " SELECT object,ceiling(mjdate),filter,chipid ";
    $sql .= " FROM exposure JOIN status ON exposure.file_id=status.file_id ";
    $sql .= " AND status.task LIKE 'find' ";
    $sql .= " WHERE status.success=0 OR status.comment IS NULL " ;
    my $dbh = $triples->{_DBH};
    my $sth = $dbh->prepare($sql);
    my @bind = (\$triples->{object},
		\$triples->{night},
		\$tiples->{filter},
		\$triples->{chipid});
    $sth->execute();
    $sth->bind_columns(@bind);
    $triples->{_STH}=$sth;
    return  $triples;
}


sub statusSelect {
    my $this = shift;
    ### a subroutine to select exposures that have not had 
    ### task run on them [according to the status table]  
    ### task the name of the task as the input field
    ### Returns a table structure with 'expnum', 'file'
    my $task = shift;
    my $condition = @_ ? shift  : "s.file_id is NULL";

    my $self = Bucket::MOP->new("");  ## a blank reference
    my $exposure = Bucket::MOP->new("exposure");
    my $received = Bucket::MOP->new("received");
    my $status = Bucket::MOP->new("status");
    
    ### the actual mySQL columns and what to bind them to
    my $columns = "r.file_id, r.active, e.date_obs, e.utc_obs ";
    @bind = ( \$self->{file_id}, \$self->{file}, \$self->{date}, \$self->{time} );

    ### The SQL command that gets the desired columns
    my $sql = "SELECT ".$columns ;
    $sql .= " FROM ".$exposure->{"_NAME"}." AS e";
    $sql .= " JOIN ".$received->{"_NAME"}." AS r";
    $sql .= " ON e.file_id=r.file_id ";
    $sql .= " LEFT JOIN ".$status->{"_NAME"}." AS s";
    $sql .= " ON e.file_id=s.file_id AND s.task LIKE \'$task\'";
    $sql .= " WHERE $condition ";

    my $sth;
    my $dbh = $self->{_DBH};
    eval {
	$sth = $dbh->prepare("$sql");
	$sth->execute;
	$sth->bind_columns(@bind);
    };
    die "Failed on leftJoin: $@\n" if $@;
    $self->{_STH} = $sth;
    return $self;
}


#### Table definitions follow below this area
sub wcs { 
    my $self = shift ;
    $self->{_DEFS} = { "file_id" => "VARCHAR(40) NOT NULL PRIMARY KEY",
		       "date_obs" => "DATE",
		       "utc_obs" => "TIME",
		       "naxis1" => "DOUBLE",
		       "naxis2" => "DOUBLE",
		       "ctype1" => "VARCHAR(15)",
		       "ctype2" => "VARCHAR(15)",
		       "crval1" => "DOUBLE",
		       "crval2" => "DOUBLE",
		       "radecsys" => "VARCHAR(15)",
		       "crpix1" => "DOUBLE",
		       "crpix2" => "DOUBLE",
		       "cdelta1"  => "DOUBLE",
		       "cdelta2"  => "DOUBLE",
		       "crota"  => "DOUBLE" };


    $self->{_COLS} = [ "file_id", "date_obs", "utc_obs", "naxis1", "naxis2", "ctype1", "ctype2",
		       "crval1", "crval2", "crpix1", "crpix2", "radecsys",
		       "cdelta1", "cdelta2", "crota" ];
}

sub exposure {
    my $self = shift ;
    $self->{_DEFS} = { "file_id" => "VARCHAR(40) NOT NULL PRIMARY KEY",
		       "expnum" => "INT NOT NULL" ,
		       "object" => "VARCHAR(20)",
		       "ra_deg" => "DOUBLE NOT NULL ",
		       "dec_deg" => "DOUBLE NOT NULL ",
		       "exptime" => "DOUBLE NOT NULL ",
		       "mjdate" => "DOUBLE NOT NULL",
		       "date_obs" => "DATE ",
		       "utc_obs" => "TIME", 
		       "filter" => "VARCHAR(10)",
		       "naxis1" => "INT",
		       "naxis2" => "INT",
		       "chipid" => "INT",
		       "runid" => "VARCHAR(10)",
		       "crval1" => "DOUBLE",
		       "crval2" => "DOUBLE",
		       "crpix1" => "DOUBLE",
		       "crpix2" => "DOUBLE",
		       "cd1_1"  => "DOUBLE",
		       "cd1_2"  => "DOUBLE",
		       "cd2_2"  => "DOUBLE",
		       "cd2_1"  => "DOUBLE" };


    $self->{_COLS} = [ "file_id", "expnum", "object", "ra_deg", "dec_deg",
                       "exptime", "mjdate", "date_obs", "utc_obs",
		       "filter", "naxis1", "naxis2",
                       "chipid", "runid",
		       "crval1", "crval2", "crpix1", "crpix2",
		       "cd1_1", "cd1_2", "cd2_2", "cd2_1" ];
}

sub measure {
    my $self = shift;
    $self->{_DEFS} = { "measure" => "INT NOT NULL PRIMARY KEY AUTO_INCREMENT",
		       "file_id" => "VARCHAR(20)",
		       "provisional" => "VARCHAR(20)",
		       "mjdate" => "DOUBLE",
		       "ra_rad" => "DOUBLE",
		       "dec_rad" => "DOUBLE",
		       "mag" => "DOUBLE",
		       "filter" => "VARCHAR(2)",
		       "observatory" => "INT",
		       "mpc" => "VARCHAR(80)"};
    $self->{_COLS} = [ keys %{$self->{_DEFS}} ];
}

sub residuals {
    my $self = shift;
    $self->{_DEFS} = { "measure" => "INT",
		       "time" => "DOUBLE",
		       "x" => "DOUBLE",
		       "x_resid" => "DOUBLE",
		       "y" => "DOUBLE",
		       "y_resid" => "DOUBLE" };
    $self->{_COLS} = [ keys %{$self->{_DEFS}} ];
}

sub orbits {
    my $self=shift;
    $self->{_DEFS} = { "official" => "VARCHAR(20)",
		       "a" => "DOUBLE",
		       "da" => "DOUBLE",
		       "e" => "DOUBLE",
		       "de" => "DOUBLE",
		       "i" => "DOUBLE",
		       "di" => "DOUBLE",
		       "Node" => "DOUBLE",
		       "dNode" => "DOUBLE",
		       "peri" => "DOUBLE",
		       "dperi" => "DOUBLE",
		       "time" => "DOUBLE",
		       "dtime" => "DOUBLE"
};
    $self->{_COLS} = [ keys %{$self->{_DEFS}} ];
}
		       
sub abg {
    my $self=shift;
    $self->{_DEFS} = {"official" => "VARCHAR(20)",
		      "abg" => "BLOB"};
    $self->{_COLS}= [ keys %{$self->{_DEFS}}];
}

sub source {
    my $self = shift;
    $self->{_DEFS} = { "provisional" => "VARCHAR(20)",
		       "measure" => "INT" };
    $self->{_COLS} = [ "provisional", "measure" ];
}

sub des {
	my $self = shift;
	$self->{_DEFS} = { "official" => "VARCHAR(20)",
			"provisional" => "VARCHAR(20)" };
	$self->{_COLS} = [keys %{$self->{_DEFS}}];
}

sub object {
	my $self = shift;
	$self->{_DEFS} = { "official" => "VARCHAR(20)",
			"provisional" => "VARCHAR(20)" };
	$self->{_COLS} = [keys %{$self->{_DEFS}}];
}

sub received {
    my $self = shift ;
    $self->{_DEFS} = { "file_id" => "VARCHAR(40) NOT NULL PRIMARY KEY",
                       "archive" => "VARCHAR(100) NOT NULL",
                       "active" => "VARCHAR(100)" };
    $self->{_COLS} = [ "file_id", "archive", "active" ];
}


sub status {
    my $self = shift ;
    $self->{_DEFS} = { "file_id" => "VARCHAR(40) NOT NULL",
                       "task" => "VARCHAR(40)",
                       "success" => "BOOL",
                       "comment" => "VARCHAR(40)",
                       "hostname" => "VARCHAR(40)",
                       "insertdate" => "TIMESTAMP"
                       };
    $self->{_COLS} = [ "file_id", "task", "success", "comment", "hostname",
                       "insertdate" ];
    $self->{_INDEX} = "PRIMARY KEY (file_id,task) ";
}

sub mpc {
    my $self = shift ;
    $self->{_DEFS} = { "id" => "INT NOT NULL PRIMARY KEY AUTO_INCREMENT",
                       "mpc_number" => "VARCHAR(5)",
                       "designation" => "VARCHAR(7)",
                       "discovery" => "BOOL",
                       "note_1" => "CHAR",
                       "note_2" => "CHAR",
                       "obs_date" => "DOUBLE",
                       "ra_deg" => "DOUBLE",
                       "dec_deg" => "DOUBLE",
                       "mag" => "DOUBLE",
                       "filter" => "CHAR",
                       "obs_code" => "INT",
                   };
    $self->{_COLS} = [ "id", "mpc_number", "designation", "discovery",
                       "note_1", "note_2", "obs_date", "ra_deg", "dec_deg",
                       "mag", "filter", "obs_code" ];
}


1;
__END__
### Documentation

=head1 NAME

Bucket::MOP - subclass of Bucket::Table. an extension for accessing the CFEPS db system

=head1 SYNOPSIS

use Bucket::MOP;

$table = Bucket::MOP->new($table_name);

$files = Bucket::MOP->statusSelect($task,$condition);

=head1 DESCRIPTION

The CFEPS stores information about processing and results in mySQL
tables.  The Bucket::MOP routines specify the structure of the tables and access 
info from these tables in a way that is useful for MOP

=head1 SUBROUTINES

=over 8

=item B<new($table_name)> Creates a new connection to the CFEPS db for 
the desired table type.  If $table_name is blank then a generic mySQL connection is made, with 
no associated table structure.  See the DEFINITION section for info about the specific tables.

=item B<selectStatus($task,[$condition])> Returns a table link with I<file_id> and 
I<file> locations as where status lines of for I<$task> meet I<$condition>.  If $condition
is not provided the "comment IS NULL" is used.

=back

=head2 Defiinitions

=over 8 

=item I<exposure> Contains keywords from a fits file.  

=item I<received> The location of the file on the compute system

=item I<wcs> The B<refined> WCS as determined by MOP.

=item I<measure> Lines of astrommetry.

=item I<source> Lines of astrometry that have been grouped to form a designated object

=item I<status> Table for keeping track of the status of processing on a particular image

=back

=head1 EXAMPLE

    $files = Table::MOP->statusSelect($task,"comment IS NULL");

selects all the files for which there is no comment in the I<status> table associated with $task. 
This list can then  be iterated over using the B<fetchRow> command:

    while ( $files->fetchRow ) {
        $this_file = $files->{file};
        $this_id = $files->{file_id};
        do_something_to $this_file, $this_id;
    }

=head1 SEE ALSO

Bucket::Table;

=head1 AUTHOR

JJ Kavelaars

=head1 COPYRIGHT AND LICENSE

Copyright 2004 by CFEPS account

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
