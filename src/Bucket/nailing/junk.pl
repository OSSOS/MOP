#!/usr/bin/env perl

use Astro::SLA qw(:constants :sla);
$rastr=shift;
$rastr=~s/:/ /g;
$decstr=shift;
$decstr=~s/:/ /g;
$i=1;
slaAfin($rastr,$i,$ra,$stat);
$i=1;
slaAfin($decstr,$i,$dec,$stat);
$ra=15.0*$ra*DR2D;
$dec=$dec*DR2D;
#print "$ra $dec\n";
$declow=int( ($dec-0.5)*3600000);
$decup=int(($dec+0.5)*3600000);
$raup=int(($ra+0.5)*3600000);
$ralow=int(($ra-0.5)*3600000);
$sql ="SELECT e.expnum from exposure e JOIN megaprime m ON e.expnum=m.expnum WHERE  ra< $raup AND ra > $ralow AND dec < $decup AND dec > $declow ";
print "use cfht\n";
print "go \n";
print "$sql \n";
print "go\n";
