#!/usr/bin/env perl
# JJK july 2001
# USAGE:  match.pl
# nothing get sent here.. the "proc-these-files" is hardwired in...

touch "match.pl.FAILED"

$file_name = "proc-these-files";

# get the image names from the file $file_name
if ( -e $file_name ){
    #open (M_APCOR, "> aper.corr");
    open (IMAGE, "< $file_name" );    
    $i = 1;
    while ( <IMAGE>   ) {
	($image, $fwhm) = split(' ');
	print "Working on $image\n";
	if ( $image ne "#" ) {
	    if ( -e $image.".obj.jmp" ) {
		print "$image.obj.jmp already exists. Fine.\n"
	    } else {
		if ( -e $image.".obj.psf" ) {
		    print "$image.obj.psf exists. Copying to $image.obj.jmp\n";
		    system("cp $image.obj.psf $image.obj.jmp");
		} else {
		    system("step1jmp -f $image -w $fwhm -t 5 -m 20000");
		}
	    }
	    $images[$i] = $image;
	    $fwhms[$i] = $fwhm;
	    $i=$i+1;
	}
    }
    print "match.csh ".$images[1]." ".$images[2]." ".$images[3]."\n" ;
    $result = `match.csh $images[1] $images[2] $images[3]` ;
}

touch "match.pl.OK"

exit
