#!/usr/bin/env perl
#!/usr/cadc/misc/bin/perl
# JJK july 2001
# Run J-M.'s and Matt's object finding systems... then intersect the 
# results.  

use Getopt::Long;
##
## seasonally determined parameters.
##


my ($rmin, $rmax, $ang, $awidth,$union);

$union=0;

GetOptions('p|prefix:s','d|directory:s',
           'rn|rmin=s' => \$rmin,
           'rx|rmax=s' => \$rmax,
           'a|angle=s' => \$ang,
           'aw|awidth=s' => \$awidth,
           'u|union'    => \$union);


$lock = "find.FAILED";
$finished = "find.OK";

# parameters for the moving object system
##
##  These a hard coded into the find algorithym
##
# detection threshold for J-M. code.
$tjmp = 2.7;
# detection threshold for Matt code.
$tmatt = 1.3;

# Maximum linear counts
$maxcount = 30000;

chomp($file_name = $opt_d);
chomp($prefix = $opt_p);


chdir $file_name;
$filename = $file_name;
if ( -e $finished || -e $lock ) {
	exit() ;
}
`touch $lock`;


$dirname = $file_name;

$file_name = "proc-these-files";

print STDERR $file_name."\n";

if ( -f $file_name ){

    open (IMAGE, "< $file_name" );
    
    
# run the detection step and compute the lines for the match step
    
    $jmline2="";
    $mattline2="";
    $i = 1;
    while ( <IMAGE>   ) {
	($image, $fwhm) = split(' ');
	if ( $image ne "#" ) {
	    $image = $prefix.$image;
	    $images[$i] = $image;
	    if ( ! ($rmin && $rmax && $ang && $awidth) ) {
## base the rates on 25 AU (rmax and awidth) and 200AU (rmin and Angle)
		$file=$image.".fits";
		print $file."\n";
		my @rate200=split ' ', `rate.pl --file $file 250 `;
		my @rate25=split ' ', `rate.pl --file $file 19 `;
		$ang = sprintf "%8.1f", $rate200[1];
		$awidth = sprintf "%8.1f", 1.2*$rate25[2];
		$rmin = sprintf "%8.1f", 0.8*$rate200[3];
		$rmin = $rmin < 0.5 ? 0.5 : $rmin ;
		$rmax = sprintf "%8.1f", 1.2*$rate25[4];
	    }
	    if ( ! -f $image.mopheader ) {
	        execute_prog("stepZjmp", "-f $image");
	    }
	    my $jmp_fwhm=$fwhm;
	    execute_prog("step1jmp", "-f $image -w $jmp_fwhm -t $tjmp -m $maxcount");
	    execute_prog("step1matt", "-f $image -w $fwhm -t $tmatt -m $maxcount");
	    $i=$i+1;
	}
    }
    print STDERR "--> $mattline2 <<-\n";
    if ( $i ne 4 ) {
	die "Too few files in $filename to allow matching.\n";
    }
@sorted = sort @images ;
@images = @sorted ;
    
# run the match step
    $mattline2 = "-f1 $images[1] -f2 $images[2] -f3 $images[3]";
    execute_prog("step2jmp", "$images[1] $images[2] $images[3]");

    print STDERR "Using JMP transformation\n";
    execute_prog("step2matt_jmp", "$mattline2");
    
# run the moving step
    execute_prog("step3jmp", "$mattline2 -rn $rmin -rx $rmax -a $ang -w $awidth");

    execute_prog("step3matt", "$mattline2 -rn $rmin -rx $rmax -a $ang -w $awidth");

    if ( $union ) { 
	$union = "-a";
    } else {
	$union = "";
    }
    execute_prog("comb-list", "$images[1] $union");

    if ( -e "Object.planted" ) {
	$result= `pipe-eff -f $images[1] -m 20 -s 0.5` ;
    }
    
    $candidates = $images[1].".cands.comb";
    if ( -e $candidates) {
        if ( -e "no_candidates" ) {
	   $result=`/bin/rm no_candidates`;
	}

#add image names to apcor files
	if (!(-f 'aper.corr')) {
	    `touch aper.corr` ;
	    `cat $images[1]."apcor" >> aper.corr`;
	    `cat $images[2]."apcor" >> aper.corr`;
	    `cat $images[3]."apcor" >> aper.corr`;
	}
        $result = `wc -l $candidates`;
	($wc,$file) = split(' ',$result); 
	$wc = $wc - 20 ;
	$wc = $wc/4.0;
	# $result =`bright_matt $mattline2`;
	print "Plate solution\n";
        open (RESULTS,">> check_me.cl");
	print RESULTS " printf(\"$wc candidates\\n\"); cd $dirname ; check $images[1].cands.comb\n";
	close RESULTS;
	$result = `runmkplt.pl -f $images[1]`;
	if (-e $images[1].".mkpltsol.OK") { 
	    unlink($images[1]."mkpltsol.FAILED") }
	$result = `runmkplt.pl -f $images[2]`;
	if (-e $images[2].".mkpltsol.OK") {
	    unlink($images[2]."mkpltsol.FAILED") } 
	$result = `runmkplt.pl -f $images[3]`;
	if (-e $images[3].".mkpltsol.OK") {
	    unlink($images[3]."mkpltsol.FAILED") }
	$result =`measure3 $images[1]`;
	print STDERR $images[1].".measure3.OK";
      	if (-e $images[1].".measure3.OK") {
	    unlink($images[1]."measure3.FAILED") }
	foreach my $thisImage ( @images ) { 
	    next if ( ! -e $thisImage.".fits" ) ;
	    `get_usno.pl --image $thisImage --num=1000 --gwyn`;
	    `checkwcs.pl --file1 $thisImage.bright.jmp --file2 $thisImage.usno_xy --output $thisImage.usno_check`;
	}
    } else {
	$result = `touch no_candidates`;
    } 

    close IMAGE;
    `touch $finished`;
    unlink($lock);
    
} else {
    print STDERR "File $file_name doesn't exist yet.."; 
}



exit();

sub execute_prog {
    my $prog = $_[0];
    my $prog_parms = $_[1];

    system("$prog $prog_parms");
    -e $prog.".OK" or die "$prog failed with $prog_parms\n" ;
    unlink($prog.".FAILED");
    unlink($prog.".OK");

}
