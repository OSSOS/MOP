#!/usr/bin/perl
use Gwyn;
$verbose=1;

@explist=&getexplist(@ARGV);

for $e (@explist) {
  $eo=$e.'o.fits';
  $ep=$e.'p.fits';

  ### maybe get the o file
  unless (-s $eo) {
    &psys("adGet -a CFHT $eo.fz");
    &psys("imcopy $eo.fz $eo");
  }
  ($filter,$crunid)=&getheadgen($eo,"filter","crunid");
  $filter=~s/ //g; $filter=uc($filter);
  $crunid=~s/ //g; $crunid=uc($crunid);

  $flat="flat.$filter.$crunid.fits";
  ### maybe get the flat
  unless (-s $flat) {
    &psys("vcp vos:sgwyn/flats/$flat .");
  }

  ### almost certainly run the fortran program
  unless (-s $ep) {
    &psys("cp $eo $ep");
    &psys("pitcairn $flat $eo");
  }
  next unless ($opt_V);

  ### complain if that didn't work
  unless (-s $ep) {
    die "ERROR: something went wrong creating $ep";
  }

  ### copy it to VOspace
  &psys("vcp -v $ep vos:OSSOS/dbimages/$e/");
  $_=`vls -l vos:OSSOS/dbimages/$e/$ep`;
  print;
  ($perm,$owner,$gr,$gw,$size,$mon,$day,$time,$file)=split;
  unless ($size>7e8) {
    die "ERROR: something went wrong copying $ep to VOspace";
  }

  ### put an annotation on it
  &psys("vtag vos:OSSOS/dbimages/$e ivo://canfar.uvic.ca/ossos#preproc_o36=success");
  $_=`vtag vos:OSSOS/dbimages/$e ivo://canfar.uvic.ca/ossos#preproc_o36`;
  print;
  chomp;
  unless ($_ eq "'success'") {
    die "ERROR: something went wrong tagging $ep";
  }
}
