#!/usr/bin/env perl


### get a bunch of astrometry lines based on the provisional designations

use strict;
use warnings;


use Bucket::MOP;
use Bucket::Control;

my $measure = Bucket::MOP->new("measure","cfhtlsvw","");

my $where="";
my $sep ="";
foreach my $name ( @ARGV ) {
    $where .= $sep." provisional LIKE '$name' ";
    $sep = " OR ";
}
$measure->selectRows({WHERE => $where});
while ( my $row = $measure->fetchRow ) {
    print $measure->{mpc}."\n";
}

