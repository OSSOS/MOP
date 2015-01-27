#!/usr/bin/perl

use Gwyn;
$verbose=1;
#$dontsys=1;
use Getopt::Std;
getopts('dt:');

$exptimemin=$opt_t || 200;

$f=shift @ARGV;
$c=shift @ARGV;

unless ($c) {
  $c=$f;
  $f='R.MP9601';
}

if ($#ARGV>=0) {
  @crunidlist=@ARGV;
} else {
  @crunidlist=($c);
}

for (@crunidlist) {
  $usecrunid{$_}=1;
}


open IN,"$ENV{HOME}/mega/ADMegaList";
while (<IN>) {
  ($e,$ra,$dec,$exptime,$datetime,$filter,$crunid,$sdss,$rdate,$cr,@a)=split;
  next unless ($exptime>=$exptimemin);
  next unless ($filter eq $f);
  next unless ($usecrunid{$crunid});
  $rd=sprintf('%.2f %.2f',$ra,$dec);
  next if ($used{$rd});
  $used{$rd}=1;
 
  push @explist,$e;
}
&megarechash(@explist);
&megarecline();
if ($#explist>=99) {
  @explist = sort {$exptime{$b}<=>$exptime{$a}} @explist;
  $exptimemin=$exptime{$explist{99}};
  for $e (@explist) {
    if ($exptime{$e}>=$exptimemin) {
      push @temp,$e;
    }
  }
  @explist=@temp;
}

@explist=&randomize(@explist);
@explist=splice @explist,0,100;

@explist=sort {$a<=>$b} @explist;
#$dontsys=1;

for $e (@explist) {
  print $line{$e},"\n";
  $eo=$e.'o';
  unless (-s "$eo.fits" || $opt_d) {
    &psys("adGet -a CFHT $eo.fits.fz");
    ($size)=-s "$eo.fits.fz";
    #($nwarn,$nerr)=&fitsverify("$eo.fits.fz");
    #if ($nerr) {
    if ($size%2880) {
      die "ERROR: problem downloading $eo.fits.fz. Size=$size";
    }

    #&psys("imcopy $eo.fits.fz $eo.fits");
    #&psys("rm $eo.fits.fz");
  }
}

for (@explist) {
  $_=$_."o.fits.fz";
}
$nexp=$#explist+1;
print "Making flat from $nexp images\n";
exit if ($opt_d);
$explist=join ' ',@explist;
unless (-s "14B_r_flat.fits") {
  &psys("vcp vos:OSSOS/dbimages/calibrators/14B_r_flat.fits .");
}

&psys("cp 14B_r_flat.fits flat.$f.$c.fits");
&psys("gcombx $f $c $explist");
&psys("vcp flat.$f.$c.fits vos:sgwyn/flats");
