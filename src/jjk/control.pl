#!/usr/bin/env perl
##
## JJK Nov 2001
##
## This is a genral purpose control script. This script will
## listen to a file and pass the lines from the input file 
## to a program as command line options.
##
## USAGE: control.pl --program makepsf.csh --input makepsf_input.txt
##
##use lib "$ENV{HOME}/pipeline/perl";
use Getopt::Long;
use Pod::Usage; 

## Get the commandline options.  --program  --input are
## required. pod2usage will parse the material after
## __END__ into a usage command if the options aren't
## Specified

my $help = 0;
my $program = "";
my $params = "";
my $rmin = "";
my $rmax = "";
my $ang = "";
my $aw = "";
$result = GetOptions('h|help|?' => \$help,
		     'p|program=s' => \$program,
		     'i|input=s' => \$params,
		     'rn|rmin=s' => \$rmin,
		     'rx|rmax=s' => \$rmax,
		     'a|angle=s' => \$ang,
		     'aw|awidth=s' => \$aw) || pod2usage();

pod2usage() if $help;
pod2usage() if !$program;
pod2usage() if !$params;

$motion = "-rn $rmin" if $rmin;
$motion = "$motion -rx $rmax" if $rmax;
$motion = "$motion -a $ang" if $ang;
$motion = "$motion -aw $aw" if $aw;

##
## Let the user know what's going on.
##

print STDERR "Spawning $program with args from $params\n";

##
## Ensure the input file exists, a courtesy to the user.
##
system("touch $params");

##
## List on to $param using a tail call.  Keep track of the
## PID so we can kill this process later.
##
$tpid = open(STATUS,"tail -n +1 -f $params |") 
    or die "Can't open the command input file $params. $!\n";

##
## LOOP FOREVER :: Slurp up lines from $params and spawn $program
## All output (STDERR,STDOUT) are echoed back to the calling shell
##
while (<STATUS>) {
    chomp((@params) = split);
    # Listen for the word DONE on the input list
    # Exit if we get that.
    if ( $params[0] eq "DONE" ) {
	$result = kill 15, $tpid;
	print STDERR "($result) Caught $params[0]: exiting....\n";
	exit(0);
    }
    print STDERR "$program @params $motion\n";
    # The redirect here gets the stdout 
    # to echo back to the calling shell
    $result = `$program @params $motion 1>&2`;
    print STDERR "$results \n";
}
#print STDERR "$motion\n";

die "This area un-reachable: $$ out of control\n";


__END__

=head1 NAME

control - pass input to program as command line arguments

=head1 SYNOPSIS

control [options]

  Options:
    -i --input         input filename 
    -p --program       program
    -rn --rmin         minimum rate of motion
    -rx --rmax         maximum rate of motion
    -a --angle         mean direction of motion
    -aw --awidth       angle half width
    -h --help -?       detailed help

=head1 OPTIONS

=over 8

=item B<--input>

Name of the file that contains command lines for B<program>

=item B<--program> 

Name of the executable to spawn with args from B<input>.

=back

=head1 DESCRIPTION

B<control.pl> spawns a program with the lines read from the input file
being sent as command line options.  <The program> waits for the spawned
program to exit and then checks for more lines in the input file.  A
seperate spawn is created for each line in the input file.

=head1 EXAMPLE

control --input makepsf_input.txt --program makepsf.pl

=cut
