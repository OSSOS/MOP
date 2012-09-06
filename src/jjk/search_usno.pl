#! /usr/bin/env perl
##
## JJK Dec 2002
##
## USNO search script that uses the cvizer access client installed at
## CADC.  This only works at the CADC.
##
## USAGE: search_usno.pl --ra|-a DEG --dec|-d DEG --radius|-r ARCSEC
##

use lib "/usr/cadc/misc/perllib/i686-linux";
use Getopt::Long;
use Pod::Usage; 
#use strict;

my $HOST = "vizier.hia.nrc.ca";
#my $HOST = "vizier.u-strasbg.fr";

my $aclient = "aclient";

#if (!(-f $aclient)) {
#   die "Can't find the aclient program\n";
#}

## pod2usage will parse the material after
## __END__ into a usage command if the options aren't
## Specified

## initialize the variable.
my $ra = 0;
my $dec = 0;
my $xsize = 0;
my $ysize = 0;
my $help = '';
my $outfile = "usno_stars";
my $nstars = 5000;

my $result = GetOptions('h|help|?' => \$help,
		     'x|xsize=f' => \$xsize,
		     'y|ysize=f' => \$ysize,
		     'a|ra=f' => \$ra,
		     'd|dec=f' => \$dec,
		     'f|file=s' => \$outfile,
		     'n|num=f' => \$nstars,
		     ) || pod2usage();

pod2usage() if $help;

##
## Let the user know what's going on.
##

#print STDERR "Search Vizer for USNO stars around $ra,$dec\n";

my $PMM = " $aclient $HOST  1660 pmm2";
if ( -e $outfile ) { 
   unlink ( $outfile) ;
}

## CONVERT TO MINUTES FOR BOX SIZE
$xsize = $xsize*60.0*2.0;
$ysize = $ysize*60.0*2.0;
##$ra = $ra/15.0;
if ( $dec > 0  ) {
$dec = "+".$dec;
}

#my $CMD = "$PMM -c $ra,$dec -ei -b $xsize,$ysize -sR -m $nstars";
my $CMD = "$PMM -c $ra,$dec -ei -r 7 -sR -m $nstars";
print STDERR "$CMD \n";
my @result = `$CMD`;

open ( USNO_LIST, "> $outfile");

my $count-0 ;
foreach my $line ( @result ) {
        $count++; 
	chomp $line ;
        #print $line,"\n";
	my ( $usno_id, $usno_pos, $usno_Bmag, $usno_Rmag ) = split (' ',$line);
	$usno_pos =~ m/([\d\.]*)[+-]([\d\.]*)/;
	my $ra=$1;
	my $dec=$2;
	if ( !( $usno_id =~ m/^\#.*/ ) )  {
	   printf USNO_LIST "%10.6f %10.6f %2.1f %d\n" , ( $ra,$dec, $usno_Rmag , $count)  ;
	}
}

exit(0)

__END__

=head1 NAME

search_usno - search the USNO catalog using vizier. Only works inside CADC.

=head1 SYNOPSIS

control [options]

  Options:
    -x --xsize         RA size of search field in DEGREEs
    -y --ysize         DEC size of search field in DEGREEs
    -a --ra	       RA of search center in DEGREEs
    -d --dec	       DEC of search center in DEGREEs
    -n --num	       number of stars to return
    -f --file          output file name



=head1 DESCRIPTION

B<search_usno.pl> spawns the aclient program and retrieves the USNO catalog stars withing B<radius> of the sky position B<ra>, B<dec>.  

The aclient program is needed as is access to the vizer catalog system.  Currently that restricts use of this script to the CADC.


=head1 EXAMPLE

search_usno --ra 123.456 -dec -34.567 -rad 1200 

=cut
