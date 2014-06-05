#!/usr/bin/env perl
##
## Compute the mag difference between three sets of photometry. 
## using the shifts file to align the lists of stars.
##
## USAGE: trans.pl --shift shifts
##	  	      --file1 file1.phot
##		      --file2 file2.phot 
##		      --file2 file3.phot
##
use Getopt::Long;
use Pod::Usage; 

## Get the commandline options.  --program  --input are
## required. pod2usage will parse the material after
## __END__ into a usage command if the options aren't
## Specified

my $help = 0;
my $trans = "";
my $file1 = "";
my $file2 = "";
my $file3 = "";
my $verbose=0;

$result = GetOptions('h|help|?' => \$help,
		     's|shift=s' => \$trans,
		     'v|verbose' => \$verbose,
		     'file1=s' => \$file1,
		     'file2=s' => \$file2,
		     'file3=s' => \$file3) || pod2usage();

pod2usage() if $help;
pod2usage() if !$trans || !$file1 || !$file2 || !$file3 ; 


my $files;
$files[0]=$file1;
$files[1]=$file2;
$files[2]=$file3;
print STDERR $files[1]." \n";
##
## Let the user know what's going on.
##

print STDERR "Computing magnitude difference between $file1 and $file2 and $file3\n";

##
## Get the transformation for the first file
##

open(TRANS,"< $trans " ) or die "Can't open transform file  $trans. $!\n";

my $i = 0;

my @xoff;
my @yoff;

while ( <TRANS> ) {
	($xoff[$i],$d,$d,$yoff[$i],$d,$d) = split (' ');
	$i++;
}
close(TRANS);




## create a 'master' coordinate file that has the x,y positions of the stars  in 
## each frame
my @pfiles;
my @apcor;
my @aperr;

print STDERR "\n".$#files."\n";
for (my $j=0; $j<=$#files; $j++ ) {
    my $file=$files[$j];
    $image = $file;
    $image =~ s/\..*$// ;
    unlink("$image.acoo") if ( -f "$image.acoo" ) ;
    open(COO,"> $image.acoo") or die "Can't open temporary coo file $image.acoo \n";
    for (my $i=0; $i<=$#files; $i++) {
	my $phot=$files[$i];
	open (PHOT,"< $phot ") or die "Can't open input photometry file: $file \n";
	while(<PHOT>) {
	    ($id, $x, $y, $mag ) = split(' ');
	    printf COO "%8.2f %8.2f %8.2f %5d\n", $x-($xoff[$i]-$xoff[$j]), $y-($yoff[$i]-$yoff[$j]),$mag,$id;
	}
	close(PHOT)
    }

    open(APCOR,"< $image.apcor") or die "Cann't open aperture correction file.";
#    $_=<APCOR>;
#    chomp $_;
#    ($apin, $apout, $apco, $aper) = split / /, $_;
    my $apin;
    my $apout;
    my $apco;
    my $aper;
    while (<APCOR>) {
	($apin, $apout, $apco, $aper) = split(' ');
	break;
    }
    print STDERR "$apin, $apout, $apco, $aper\n";
    close(APCOR);

    $apcor[$j]=$apco;
    $aperr[$j]=$aper;

    open(ZP,"<$image.zeropoint.used") or die "Cann't open zeropoint file";
    $zp=<ZP>;
    close(ZP);
    chomp $zp ;
    unlink($image.".amag") if ( -f $image.".amag");
    my $cmd="daophot.sh -i $image.fits -c $image.acoo -a $apin -z $zp -o $image.amag";
    print STDERR "\n$cmd\n";
    system('which daophot.sh');
    system($cmd);
    if ( -f 'daophot.OK') {
        unlink('daophot.FAILED') if ( -f 'daophot.FAILED');
        unlink('daophot.OK') if ( -f 'daophot.OK');
    } else {
	die "daophot failed to run properly."
    }
    $pfiles[$j]=$image.".amag";
}
		

## use the first file as the master and find matches to stars in that file.

open(MASTER,"< $pfiles[0]") or 
		die "Can't open star photometry file $pfiles[0]. $!\n";

print STDERR "\n\nAperture corrections: $apcor[0], $apcor[1], $apcor[2]\n\n";

my @dmag2;
my @dmag3;
my $n2=0;
my $n3=0;
my $n1=0;
FILE1: while ( <MASTER> ) {
   my ($x1, $y1, $mag1,$merr1,$id1 ) = split(' '); 
   open (SLAVE,"< $pfiles[1]") or 
		die "Can't open star photometry file $file2. $!\n";
   FILE2: while (<SLAVE> ) {
      my ($x2, $y2, $mag2, $merr2, $id2 ) = split(' '); 
      if ( sqrt( ($x1-($x2-$xoff[1]))**2 + ($y1 - ($y2 - $yoff[1]))**2) < 3 ) {
	  push @xoff2,($x2-$x1);
	  push @yoff2,($y2-$y1);
	  my $dmag = $mag2-$apcor[1]-($mag1-$apcor[0]);
	  push @dmag2 ,$dmag if ( $mag1 > 0 );
	  print STDERR "(1,2) $x1 $y1 $mag1 $x2 $y2 $mag2 $dmag\n" if ($verbose);
	  last FILE2;
      }
   }
   close(SLAVE);
   open (SLAVE,"< $pfiles[2]") or 
		die "Can't open star photometry file $file3. $!\n";
   FILE3: while (<SLAVE> ) {
      my ($x3, $y3, $mag3, $merr3, $id2 ) = split(' '); 
      if ( sqrt( ($x1-($x3-$xoff[2]))**2 + ($y1 - ($y3 - $yoff[2]))**2) < 3 ) {
	  push @xoff3,($x3-$x1);
	  push @yoff3,($y3-$y1);
	  my $dmag = $mag3-$apcor[2]-($mag1-$apcor[0]);
	  push @dmag3, $dmag;
	  print STDERR "(1,3) $x1 $y1 $mag1 $x3 $y3 $mag3 $dmag\n" if ($verbose);
	  last FILE3;
      }
   }
   close (SLAVE);
   $n1++;
}
close (MASTER);

## Compute the offset between frames, based from star matching.
if ( $#dmag2 > 0 ) { 
   @smag2 = sort { $a <=> $b } @dmag2;
   $dmag2 = $smag2[int($#smag2/2.0)];
   $emag2 = abs($smag2[int($#smag2/4.0)]-$dmag2);
   @soff2 = sort { $a <=> $b } @xoff2;
   $dx2=$soff2[int($#soff2/2.0)];
   @soff2 = sort { $a <=> $b } @yoff2;
   $dy2=$soff2[int($#soff2/2.0)];

} else {
   $dmag2 = 0;
   $dx2=$xoff[1];
   $dy2=$yoff[1];
}
if ( $#dmag3 > 0 ) { 
   @smag3 = sort { $a <=> $b } @dmag3;
   $dmag3 = $smag3[int($#smag3/2.0)];
   $emag3 = abs($smag3[int($#smag3/4.0)]-$dmag3);
   @soff3 = sort { $a <=> $b } @xoff3;
   $dx3=$soff3[int($#soff3/2.0)];
   @soff3 = sort { $a <=> $b } @yoff3;
   $dy3=$soff3[int($#soff3/2.0)];

} else {
   $dmag3 = 0.;
   $dx3=$xoff[2];
   $dy3=$yoff[2];
}


open(SHIFTS,">shifts");

if ( abs($dmag2) > 0.05 || abs($dmag3) > 0.05 ) { 
    printf SHIFTS "# Got shifts of $dmag2 and $dmag3\n";
    printf SHIFTS "# Magnitude shifts don't match apcor, they should.\n";
} 

#printf SHIFTS "%5.1f 1 0 %5.1f 0 1 %8.3f %8.3f %4d\n", (0.0,0.0,0.0,0.0,$n1);
printf SHIFTS "%5.1f 1 0 %5.1f 0 1 %8.3f %8.3f %4d\n", (0.0,0.0,$apcor[0],$aperr[0],$n1);
#printf SHIFTS "%5.1f 1 0 %5.1f 0 1 %8.3f %8.3f %4d\n", ($dx2,$dy2,$dmag2,$emag2,$#dmag2);
printf SHIFTS "%5.1f 1 0 %5.1f 0 1 %8.3f %8.3f %4d\n", ($dx2,$dy2,$dmag2+$apcor[1],$emag2,$#dmag2);
#printf SHIFTS "%5.1f 1 0 %5.1f 0 1 %8.3f %8.3f %4d\n", ($dx3,$dy3,$dmag3,$emag3,$#dmag3);
printf SHIFTS "%5.1f 1 0 %5.1f 0 1 %8.3f %8.3f %4d\n", ($dx3,$dy3,$dmag3+$apcor[2],$emag3,$#dmag3);
#printf "$xoff[2] 1 0 $yoff[2] 0 1 $dmag3 $emag3 $#dmag3\n";


close(SHIFTS);
