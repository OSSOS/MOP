#!/usr/bin/perl

# This is Holman's version of prog3 for the CFHT tno search pipeline modified by JJ to change some thresholds.
# It writes the names of the three files at the top of the output
# file.  Then it propagates the two header lines from the other files.
# 
# Finally, it searches for linear, constant rate motion.
#

$failed = `touch "step3jjk.FAILED"`;
open(FAILED,"> step3jjk.FAILED");
print FAILED "step3jjk @ARGV\n";
close(FAILED);
print STDERR "step3jjk @ARGV\n";

use Getopt::Long;
GetOptions('f1|file1:s','f2|file2:s','f3|file3:s','rn:f','rx:f','a:f','w:f', 'fr:f', 'mf:f', 'ma:f', 'h|help');

# -f1 image_file1 (w/o .fits extension) -f2 image_file2  -f3 image_file3 -rn min rate -rm max rate -a angle -w width -fr max_flux_ratio -mf max_median_flux -ma minimum_area -h/? help line

$im1 = $opt_f1;
$im2 = $opt_f2;
$im3 = $opt_f3;
$rn  = $opt_rn;
$rx  = $opt_rx;
$angle = $opt_a;
$width = $opt_w;
$max_flux_ratio = $opt_fr;
$min_area = $opt_ma;
$minimum_median_flux = $opt_mf;

$convertdate="convert_mjd";

$unidfile1 = "$im1".".unid.matt";
$unidfile2 = "$im2".".unid.matt";
$unidfile3 = "$im3".".unid.matt";
$result    = "$im1".".moving.matt";

# Prepare header.

open(OUTFILE,"> $result");

print OUTFILE "# $im1\n";
print OUTFILE "# $im2\n";
print OUTFILE "# $im3\n";

open(INFILE,"< $unidfile1");
## skip the ## lines as they describe the headers, # lines contain the values
while(<INFILE>) {
	if ( ! m/^\#\#/ ) {
	   last;
	}
	print OUTFILE $_;
}
print OUTFILE $_;
while(<INFILE>) {
	if ( ! m/^\#\#/ ) {
	   last;
	}
	print OUTFILE $_ ;
}
$line = $_;
($tmp, $mjd_mid, $exptime, $thresh, $fwhm1, $maxcount, $ra, $dec, $expnum) = split(' ', $line);
$time1 = $mjd_mid;
$date1 = `$convertdate $mjd_mid`;

printf OUTFILE "# %10s %8.2f%6.2f%6.2f%9.1f%11.5f%11.5f%9d\n", 
    $date1, $exptime, $thresh, $fwhm1, $maxcount, $ra, $dec, $expnum;
while(<INFILE>) {
	if ( ! m/^\#\#/ ) {
	   last;
	}
	print OUTFILE $_;
}
$line = $_;
($tmp, $pixscale, $chipid, $crpix1, $crpix2, $naxis1, $naxis2, $instrum) = split(' ', $line);
$platescale = $pixscale;
print OUTFILE $line;
close(INFILE);

open(INFILE,"< $unidfile2");
while(<INFILE>) {
	if ( ! m/^\#\#/ ) {
	   last;
	}
	print OUTFILE $_;
}
print OUTFILE $_;
while(<INFILE>) {
	if ( ! m/^##/ ) {
	   last;
	}
	print OUTFILE $_ ;
}
$line = $_;
($tmp, $mjd_mid, $exptime, $thresh, $fwhm2, $maxcount, $ra, $dec, $expnum) = split(' ', $line);
$time2 = $mjd_mid;
$date2 = `$convertdate $mjd_mid`;

printf OUTFILE "# %10s %8.2f%6.2f%6.2f%9.1f%11.5f%11.5f%9d\n", 
    $date2, $exptime, $thresh, $fwhm2, $maxcount, $ra, $dec, $expnum;
while(<INFILE>) {
	if ( ! m/^##/ ) {
	   last;
	}
	print OUTFILE $_ ;
}
$line = $_;
($tmp, $pixscale, $chipid, $crpix1, $crpix2, $naxis1, $naxis2, $instrum) = split(' ', $line);
$platescale = $pixscale;
print OUTFILE $line;
close(INFILE);

open(INFILE,"< $unidfile3");
while(<INFILE>) {
	if ( ! m/^\#\#/ ) {
	   last;
	}
	print OUTFILE $_;
}
print OUTFILE $_;
while(<INFILE>) {
	if ( ! m/^##/ ) {
	   last;
	}
	print OUTFILE $_;
}
$line = $_;
($tmp, $mjd_mid, $exptime, $thresh, $fwhm3, $maxcount, $ra, $dec, $expnum) = split(' ', $line);
$time3 = $mjd_mid;
$date3 = `$convertdate $mjd_mid`;

printf OUTFILE "# %10s %8.2f%6.2f%6.2f%9.1f%11.5f%11.5f%9d\n", 
    $date3, $exptime, $thresh, $fwhm3, $maxcount, $ra, $dec, $expnum;
while(<INFILE>) {
	if ( ! m/^##/ ) {
	   last;
	}
	print OUTFILE $_ ;
}
$line = $_;
($tmp, $pixscale, $chipid, $crpix1, $crpix2, $naxis1, $naxis2, $instrum) = split(' ', $line);
$platescale = $pixscale;
print OUTFILE $line;
close(INFILE);

printf OUTFILE "##     RMIN    RMAX   ANGLE   AWIDTH\n";
printf OUTFILE "# %8.1f%8.1f%8.1f%8.1f\n", 
    $rn, $rx, $angle, $width;
printf OUTFILE "##   X       Y     X_0     Y_0        FLUX       SIZE   MAX_INT  ELON           \n";


close(OUTFILE);

$search    ="search_jjk";
$linearity = 3.0;

$max_flux_ratio = $opt_fr;
$min_area = $opt_ma;
$minimum_median_flux = $opt_mf;


`$search $linearity $rn $rx $angle $width $platescale ${max_flux_ratio} ${minimum_median_flux} ${min_area}  $unidfile1 $time1 $fwhm1 $unidfile2 $time2 $fwhm2 $unidfile3 $time3 $fwhm3 >> $result`;

`touch "step3jjk.OK"`;
