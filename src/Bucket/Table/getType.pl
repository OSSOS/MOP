#!/usr/cadc/misc/bin/perl
###############################################################
#                                                             #
#                                                          ## #
# BUCKET                                                  ##  #
# [Better Use  Control Keeps Everything Together]        ##   #
#                                                       ##    #
#                                                      ##     #
# A BUCKET to hold the suds            |              ##|     #
# for the MOP                          |             ## |     #
#                                      |       _    ##  |     #
#                                      |      |-\\ ##/- |     #
#                                      |      /_/-**|/- |     #
#                                      \----------------/     #
#                                                             #
###############################################################

## getType.pl
##
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
#    foreach my $type ( 'Checkup', 'PreDisc', 'Discover' ) { 
#     my $t1 = $type."Start";
#     my $t2 = $type."End";
#     return $type if ( $elong{$t1} > $elongation && $elong{$t2} < $elongation ) ;
#    }


}

1;
