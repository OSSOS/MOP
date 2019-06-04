### control is a module that should be loaded by all MOP perl scripts. Control creates
### the .FAILED and .OK files and provides an convience method for executing a MOP 
### MOP program that should use the .OK/.FAILED protocal
package Control;

use strict;
use warnings;
use Carp;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
				   MOP_execute
				   my_sky2xy
) ] );

our @EXPORT = qw(
		 MOP_execute	
	 my_sky2xy
);

our $err;
our $start_dir;

BEGIN {
    ### to put this command at the start of every MOP perl script
    use File::Basename;
    use Fcntl;
    ### get the name of the directory we are starting in
    $start_dir = `pwd`;
    chomp $start_dir;
    my $me = $start_dir."/".basename($0);
    ### create the file PROGRAME_NAME.FAILED
    if ( -e "$me.OK" || -e "$me.FAILED"  )  {
    print STDERR "$me.OK/FAILED file exists. " ;
    print STDERR "Continue ? (y/n)";
    my $ans=<STDIN>;
    if ( $ans =~ /y/ ) {
       unlink($me.".OK");
       unlink($me.".FAILED");
    } else {
	die "exiting $me\n";
    }
    }
    ### create the file with exclusive
    sysopen (LOCK,"$me.FAILED",O_EXCL|O_CREAT|O_WRONLY) or
	die "Could not create FAILED file $me.FAILED";
    close LOCK;
}

END {
    #### put this call at the end of ever MOP script startup
    use Fcntl;
    use File::Basename;
    ### the name of the start_dir was set when the script BEGIN
    my $me = $start_dir."/".basename($0);
    if ( $? == 0 ) { 
	sysopen (LOCK,"$me.OK",O_EXCL|O_CREAT|O_WRONLY) or
	    die "Could not create OK file $me.OK \n";
	close LOCK;
    } 
}

sub MOP_execute {
    use File::Basename;
    ### set the error flag right from the start
    $err=1;
    ### execute a program for MOP
    my $prog = shift;
    my $me = basename($prog);
    my $ex_dir = `pwd`;
    chomp $ex_dir;
    my $prog_parms = shift;
    my $mode = @_ ? shift : "";

    ### some monkey business for mkpltsol and hansastone
    my $prefix = ( $prog =~ /mkpltsol/ || $prog =~ /hansastone/ ) ? $prog_parms."." : "";
    my $failed = $ex_dir."/".$prefix."$me.FAILED";
    my $ok     = $ex_dir."/".$prefix."$me.OK";

    if ( -e $ok || -e $failed ) {
	my $ans = "n";
	if ( !($mode =~ /force/) ) { 
	  print STDERR "$ok or $failed already exists? " ;
	  print STDERR "Continue ? (y/n)";
	  $ans = <STDIN>;
 	} else {
          $ans = "y";
	} 	
	if ( $ans =~ /y/ ) {
	  unlink ($ok);
	  unlink ($failed);
	} else { 
	  return 0;
	}

    }
    my $result="";
    eval {
	$result = `$prog   $prog_parms`;
    };
    carp "Command $prog   $prog_parms failed on execute: \n $?,$1 \n " if $@;
    ### programs the ended correctly and thus we trust their results produce a
    ### .OK file.
    if (-e $ok && -e $failed ) {
	unlink($failed);
	unlink($ok);
	$err=0;
    }
    return $result;
}


sub my_sky2xy ($$$) {
	my $file = shift;
	my $ra = shift;
	my $dec = shift;
	$file = $ENV{IMAGE_BASE_DIR} ? $ENV{IMAGE_BASE_DIR}."/".$file : $file;
 	my ($raout, $decout , $junk1 , $junk2, $x,$y, @rest)  = split ' ',`sky2xy $file $ra $dec`;
	return $x, $y;
}


__END__

=head1 SYNPOSIS

use Bucket::Control;

MOP_execute("program","params");

=head1 DESCRIPTION

Control contains a BEGIN and END block, these are blocks that any perl
script loading this module (through the use command) will run before
the program itself is run.  The BEGIN module checks for the existance
of a .OK and .FAILED file.  If those files exist then the program
exists with a warning.  If the files don't exist then .FAILED is
created and the script continues.

On completion perl starts the END script.  If no error flags have been
set then the a .OK file is created and the program exists. If an error
flag has been set then the .OK file NOT created.

B<MOP_execute("prpgram","param")> spawns new programs to the shell,
grabing the stdout of those programs to be sent back to the calling
program. MOP_execute checks for the existance of .FAILED and/or .OK
files before starting the external program and cleans those files up
before returning the results to the calling program.

=cut
