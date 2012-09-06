#!/usr/bin/env perl

foreach $file ( @ARGV ) {
   #`curl "http://cadcwww.hia.nrc.ca/proxies/megaprime?file=$file" > $file.fits.fz`
   `curl "http://cadcdata.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/proxies/megaprime?file=$file" > $file.fits.fz `  ;
   `imcopy $file.fits.fz $file.fits`;
   unlink("$file.fits.fz");
}
