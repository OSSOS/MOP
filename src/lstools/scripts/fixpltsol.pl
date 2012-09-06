#!/usr/bin/env perl
# LA 2004/02
# go back and re-run mkpltsol where it failed (cleanup)

use Getopt::Long;

GetOptions('p|prefix:s','d|directory:s') ;

chomp($file_name = $opt_d);
chomp($prefix = $opt_p);

chdir $file_name;
$filename = $file_name;
$dirname = $file_name;
$file_name = "proc-these-files";

print STDERR "$dirname $file_name\n";
if ( -e $file_name ){
    open (IMAGE, "< $file_name" );    
    $i=1;
    while ( <IMAGE>   ) {
	($image) = split(' ');
	if ( $image ne "#" ) {
	    $image = $prefix.$image;
	    $images[$i] = $image;
	    $i=$i+1;
	}	    
    }
    @sorted = sort @images ;
    @images = @sorted ;
    
    print STDERR "$images[1] $images[2] $images[3]\n";
    $candidates = $images[1].".cands.comb";
    if ( -e $candidates) {
        if ( -e "no_candidates" ) {
	    $result=`/bin/rm no_candidates`;
	}
	$result = `mkpltsol $images[1]`;
	if (-e $images[1].".mkpltsol.OK") { 
	    `/bin/rm $images[1]."mkpltsol.FAILED"` }
	$result = `mkpltsol $images[2]`;
	if (-e $images[2].".mkpltsol.OK") {
	    `/bin/rm $images[2]."mkpltsol.FAILED"` } 
	$result = `mkpltsol $images[3]`;
	if (-e $images[3].".mkpltsol.OK") {
	    `/bin/rm $images[3]."mkpltsol.FAILED"` }
	$result =`measure3 $images[1]`;
      	if (-e $images[1].".measure3.OK") {
	    `/bin/rm $images[1]."measure3.FAILED"` }
    } 
    
    close IMAGE;
    
} else {
    print STDERR "File $file_name doesn't exist yet..\n"; 
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
