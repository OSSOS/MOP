#!/usr/bin/env perl
### Get images from the CADC archive of LS-VW observations, based on BLOCK/DATE combo.


require v5.6.0;
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use File::Path;
use File::Basename;
use File::Spec;


my ($date, $block, $help, $proc, $verbose);

my $called = GetOptions ('h|help' => \$help,
                         'd|date=s' => \$date,
                         'b|block=s' => \$block,
			 'f|field=s' => \$field,
			 'v|verbose' => \$verbose,
			 'p|proccessed' => \$proc
			 ) || pod2usage();

pod2usage() if $help || !$date || !$block;

## open the file and read the first 3 lines, these contain the names of the
## images to be postage stamped.


my $db_host="www.hia-iha.nrc-cnrc.gc.ca";

my $ext = "o";
$ext = "p" if ( $proc) ;

foreach my $file ( `curl 'http://$db_host/CFHTLSVW/block.php?date=$date&block=$block'`) { 
  chomp $file;
  my $cmd = "cadc_get.pl ".$file.$ext;
  print STDERR "downloading $file$ext  \n" if ( $verbose);
  system("$cmd");
}


__END__

=head1 NAME

get_lsvw - Retrieve the CFHTLS-VW images associated with a given block takeb on the specified date.

=head1 SYNOPSIS

get_lsvw [-p -v] [--list|-d date -b block ]

=head1 OPTIONS

=over 8

=item B<--block|-b>

LS-VW Observing BLOCK that you want to retrieve from the archive.

=item B<--date|-d>

Date of observations made. Only observations for the given date/block combo are retrieved.

=item B<--proc|-p>

Get the ELIXIR processed frames, default is to download the RAW frames.

=item B<--list>

List all the LS-VW blocks currently online at the CADC and the dates of observation. I<Not currently implemented>

=back

=head1 DESCRIPTION

B<get_lsvw> interagates the LS-VW database system to discover exposures made of a given block
on a given date.  Images that match these constraints are then retreived from the CADC via a
download poxxy.

=cut
   
