#!/usr/bin/env perl
##
## Compute the mag difference between two sets of photometry. 
## using the shifts file to align the lists of stars.
##
## USAGE: dmag-rp.pl --zp1 zeropoint1
##                   --zp2 zeropoint2
##                   --file1 file1.mag
##		     --file2 file2.mag
##                   --im image
##
use Getopt::Long;
use Pod::Usage; 

## Get the commandline options.  --program  --input are
## required. pod2usage will parse the material after
## __END__ into a usage command if the options aren't
## Specified

my $help = 0;
my $zp1 = 0.0;
my $zp2 = 0.0;
my $file1 = "";
my $file2 = "";
my $im = "";
my $verbose=0;

$result = GetOptions('h|help|?' => \$help,
		     'v|verbose' => \$verbose,
		     'zp1=s' => \$zp1,
		     'zp2=s' => \$zp2,
		     'file1=s' => \$file1,
		     'file2=s' => \$file2,
		     'im=s' => \$im) || pod2usage();

pod2usage() if $help;
pod2usage() if !$zp1;
pod2usage() if !$zp2;
pod2usage() if !$file1;
pod2usage() if !$file2;
pod2usage() if !$im;

##
## Let the user know what's going on.
##

print STDERR "Computing magnitude difference between $file1 and $file2\n";

##
## Get the transformation for the first file
##

## use the first file as the master and find matches to stars in that file.

open(MASTER,"< $file1") or 
		die "Can't open star photometry file $file1. $!\n";

my @dmag2;
my $n2=0;

FILE1: while ( <MASTER> ) {
   ($x1, $y1, $mag1, $merr1 ) = split(' '); 
   open (SLAVE,"< $file2") or 
		die "Can't open star photometry file $file2. $!\n";
   FILE2: while (<SLAVE> ) {
      ($x2, $y2, $mag2, $merr2 ) = split(' ');
      if ( sqrt( ($x1-$x2)**2 + ($y1 - $y2)**2) < 5 ) {
	 my $dmag = $mag2-$mag1;
         push @dmag2 ,$dmag if ( $mag1 > 0 );
         print STDERR "(1,2) $x1 $y1 $mag1 $x2 $y2 $mag2\n" if ($verbose);
	 last FILE2;
      }
   }
   close(SLAVE);
}
if ( $#dmag2 > 0 ) { 
   @smag2 = sort { $a <=> $b } @dmag2;
   $dmag2 = $smag2[int($#smag2/2.0)]
} else {
   $dmag2 = $zp2-$zp1;
}
print STDERR "zp1 = $zp1\n" if ($verbose);
print STDERR "zp2 = $zp2\n" if ($verbose);
print STDERR "dmag = $dmag2\n" if ($verbose);
printf "%6.3f %6.3f %6.3f %6.3f %6.3f\n", $dmag2, $zp1, $zp2, $zp2-$zp1+$dmag2, $zp2+$dmag2;

# All the following is done with CorrectMag.sh.
# So drop it from here.
#
#$dmag2 = $zp2-$zp1+$dmag2;
#
#open (FOUND, "< $im.comb.found") or
#		die "Can't open success file $im.comb.found\n";
#open (NEW, "> $im.corrected.comb.found") or
#		die "Can't open new success file $im.corrected.comb.found\n";
#while ( <FOUND> ) {
#    ($x1, $y1, $mag1, $ra1, $an1 ) = split(' ');
#    printf NEW "%10.2f%10.2f%10.2f%10.2f%10.2f\n", $x1, $y1, $mag1+$dmag2, $ra1, $an1;
#}
#close (FOUND);
#close (NEW);
#
#open (MISSED, "< $im.comb.missed") or
#		die "Can't open failure file $im.comb.missed\n";
#open (NEW, "> $im.corrected.comb.missed") or
#		die "Can't open new failure file $im.corrected.comb.missed\n";
#while ( <MISSED> ) {
#    ($x1, $y1, $mag1, $ra1, $an1 ) = split(' ');
#    printf NEW "%10.2f%10.2f%10.2f%10.2f%10.2f\n", $x1, $y1, $mag1+$dmag2, $ra1, $an1;
#}
#close (MISSED);
#close (NEW);

__END__

=head1 NAME

control - pass input to program as command line arguments

=head1 SYNOPSIS

control [options]

  Options:
    -i --input         input filename 
    -p --program       program
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
