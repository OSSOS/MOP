#!/usr/bin/env perl

## JJK Jun 2003
package File;
use strict;
use warnings;



sub read($) {

    my $dubug = 0;
    my $cand = shift;
    
    
## open the candidate file for read only
    open (CAND,"< $cand");
    
## frames are given by lines with a leading # 
## lines with ## indicate the start of a header line
    
## Get the names of the frames
    my $nf = 0;
    my @frames =();
    while (<CAND>) {
	chomp;
	last if ($_ =~ m/^##/);
	$_ =~ m/^#\s(\S*)\s*$/;

	$frames[$nf] = $1;
		 $frames[$nf] =~ s/^fk//;
	print "read $frames[$nf] as frame: $nf\n" if ( $debug );
	$nf++;
} 

## First line after a file name is the version number of the file.
$_ =~ m/## MOP version 1.0/ or die "Don't understand line\n $_\n Exiting\n";

my @headers;
my %keys = ();
my @keywords;
my $frame;
my $fnum=0;

while (<CAND>) {
        $_ =~ m/^#/ or last;
	if ( $_ =~ m/^##/ ) {
	   @keywords = split(' ',$_);
	   if ( $_ =~ m/MOP version/ ) {
	      my $line = $_;
	      ## check the version
	      $line =~ m/## MOP version (\d*\.\d*)\D*$/;
	      $1 == 1.0 or die "$line; I cann't read $1 MOP files\n";
	      $fnum ++;
           }
	   next;
	}
	$frame = $frames[$fnum];
	my @values = split(' ',$_);
	foreach my $keyword ( @keywords )  {
	   my $value = shift @values;
	   if ( $keyword =~ m/MJD-OBS-CENTER/ ) {
		$value .= " ".(shift @values)." ".(shift @values);
	   }
	   $keys{$frame}{$keyword} = $value ;
	}
}

print "$_ ....\n" if ( $debug);
if ($debug) {
  my $k = \%keys;
  foreach my $frame  ( keys %keys ) {
    foreach my $keyword ( keys %{$keys{$frame}} ) {
      print "$frame,$keyword,$k->{$frame}{$keyword}\n";
    }
  }
}


## get rid of the leading comment mark and white spaces
shift @keywords;
my @columns = @keywords;
my @lines;
my $line;
my $nlines=0;

while ( $line = <CAND> ) {
	foreach my $frame ( @frames )  {
	   print "$frame: $line" if ( $debug);
	   my @line = split(' ',$line);
	   foreach my $col ( @columns ) { 
	     $lines[$nlines]{$frame}{$col} = shift @line;
	   }
	   $line = <CAND>;
        }
	$nlines++;
}
my %file;
$file{"HDU"} = \%keys;
$file{"ARRAY"} = \@lines;

## Now we have the content of the file

close(CAND);

return (\%file);

}

__END__

=head1 SYNOPSIS

B<abg_gen.pl> generate a set of .abg files using output from MOP

=head1 USAGE

B<abg_gen.pl> --cand file.cands.comb

=head1 OPTIONS

=over 8

=item B<--cand>

A candidate file in the format produced by I<find.pl> as part of MOP.

=back

=head1 USAGE

B<abg_gen.pl> takes as input the found candidates file as generated 
by I<find.pl>.  The I<abg> file contains the orbit fit as dertmined by I<fit_radec> (G. Bernstein & B. Khushalani 2000, AJ 120 3323).  Each object in the cands.comb file generates it's own OBJ#.abg file which contains the fit and a file 
names MPC which contains the astrometric measures in MPC format.

=head1 NOTES

The names of the object abg and MPC files should be more intellegent.


