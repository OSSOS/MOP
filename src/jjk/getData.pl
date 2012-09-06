#!/usr/bin/env perl

$file = shift ;
chomp $file;
$ext = shift;
chomp $ext;
$outfile = sprintf("%s%02d.fits",$file,int($ext));
#$outfile = $file..$ext.".fits";
exit(0) if -e $outfile;
print "Getting $file extension $ext and saving as $outfile\n";

$ext=$ext+1;

`adGet  -s -a CFHT -nocrc -op getData?extno=$ext $file -o $outfile` ;
if ( $ext<19 ) {
  $sec = "[-*,-*]";
  `imcopy $outfile$sec tmp.fits `;
  unlink($outfile);
  rename('tmp.fits', $outfile);
}

`wcsUpdate.pl -f $outfile`;
`fitsIngest.pl $outfile`;
