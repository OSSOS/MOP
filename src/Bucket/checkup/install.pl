#!/usr/bin/env perl
### install files in the bindir

use Getopt::Long;
use File::Copy;
use strict;

my $prefix = $ENV{HOME}."/MOP";
GetOptions('p|prefix:s' =>\$prefix) || pod2usage(2);


die "Cann't access $prefix/bin\n" unless -d "$prefix/bin";

my @script=("mpc_gen.pl", "getAstrom.pl","planObs.pl","getOrbit.pl","linkAstrom.pl");

foreach my $script (@script)  {
    unlink($prefix."/bin/".$script) if -e $prefix."/bin/".$script ;
    copy($script,$prefix."/bin/".$script) ||
	die "Failed to copy $script to ".$prefix."/".$script;
    chmod 0555,$prefix."/bin/".$script;
}


