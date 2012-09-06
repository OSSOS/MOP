#!/usr/bin/env perl

require v5.6.0;

use Getopt::Long;
use Pod::Usage;
use File::Path;
use File::Basename;
use File::Spec;


my $file = `ls *cands.comb`;
my $root = "/home/salish/1/kavelaar/public_html/test2/";
my $loc = "./";
my $width = 64;

my $imcopy = "imcopy";
my $convert = "convert";

my $gethead = "gethead";

my $called = GetOptions ('h|help' => \$help,
			 'f|file=s' => \$file,
			 'r|root=s' => \$root,
			 'l|location=s' => \$loc,
			 'w|width=f' => \$width
		        ) || pod2usage();

pod2usage() if $help ;
pod2usage() if !$file;

## open the file and read the first 3 lines, these contain the names of the
## images to be postage stamped.

open(CAND,"< $file") or die "Cann't open candidate file.\n";

my %image;
my $comment;
my @names = ("im1","im2","im3");

foreach my $name ( @names )  {
      my $line = <CAND>;
      chomp $line;
      ($comment,$image{$name},my $dummy) = split(' ',$line);
      $comment =~ m/^\#/  or die "$file has unexpected format.\n";
      $image{$name} =~ s/^fk//;
      $image{$name} =~ s/$/\.fits/;
      -e $image{$name} or die "Cann't access required image $image{$name}\n";
}

my ($x1,$x2,$y1,$y2);

my %x,%y,%xo,%yo;
my $xm,$ym;
my $xn,$yn;
my $count=0;

`mkdir -p $root`;

$chip = `$gethead IMAGEID $image{im1}`;
chomp $chip;
$rootfile = $image{im1};
$rootfile =~ s/\.fits//;
$htmlfile = "${rootfile}C$chip.html";
$htmlfile = File::Spec->catfile($root,$htmlfile);
open (HTML,"> $htmlfile") or die "Cann't create HTML file $htmlfile\n";

print HTML "<HTML>";
print HTML "<FORM METHOD=POST ACTION=confirm.pl>\n";
print HTML "<INPUT TYPE=HIDDEN NAME=loc VALUE=$loc>\n";
print HTML "<table cellpadding=0 cellspacing=0 border=0>\n"; 
while ( <CAND> ) { 
	chomp;
	!( m/^\#/ )  or next ;
	$count++;
$lwx = $width;
$lwy = $width;
$uwx = $width;
$uwy = $width;
	foreach my $name ( @names ) {
	  my $line = <CAND>;
	  ($x{$name},$y{$name},$xo{$name},$yo{$name}) =  split(' ',$line);
  	}
	$MPC = "$rootfile.MPC.$count";
	-e $MPC or die "MPC file ($MPC) doesn't exist?\n";
	$newMPC = File::Spec->catfile($root,"${MPC}C$chip");
	`cp $MPC $newMPC`;
	$color = $color eq "yellow" ? "olive" : "yellow";
	
        #figure out postage stamp boundries ;
        ( $xm ) = sort { $x{$a} <=> $x{$b} } keys %x ;
        ( $ym ) = sort { $y{$a} <=> $y{$b} } keys %y ;
        ( $xn ) = sort { $x{$b} <=> $x{$a} } keys %x ;
        ( $yn ) = sort { $y{$b} <=> $y{$a} } keys %y ;
        $xim=$xm;
	$yim=$ym;
	if (( $x{$xm} - $lwx ) < 0 ) {
	    $uwx = $uwx + $lwx - $x{$xm} ;
	    $lwx = $x{$xm} - 1 ;
	    $xim = $xm;
        }
	if (( $x{$xn} + $uwx ) > 2048 ) {
	    $lwx = $uwx - ( $x{$xn} - 2048 )  + $lwx;
	    $uwx = ( 2048 - $x{$xn}); 
	    $xim = $xn;
	}
	if (( $y{$yn} + $uwy ) > 4096 ) {
	    $lwy = $uwy - ( $y{$yn} - 4096 )  + $lwy;
	    $uwy = ( 4096 - $y{$yn}); 
	    $yim = $yn;
	}
	if (( $y{$ym} - $lwy ) < 0 ) {
	    $uwy = $uwy + $lwy - $y{$ym} ;
	    $lwy = $y{$ym} - 1;
	    $yim = $ym ;
        }
	$uwx = $xo{$xim} + $uwx ;
	$lwx = $xo{$xim} - $lwx ;
	$uwy = $yo{$yim} + $uwy ;
	$lwy = $yo{$yim} - $lwy ;
	print HTML "<TR bgcolor=$color >";
 	foreach $name ( @names ) {
	   $xl = int($lwx - $xo{$name} + $x{$name});
	   $xu = int($uwx - $xo{$name} + $x{$name});
	   $yl = int($lwy - $yo{$name} + $y{$name});
	   $yu = int($uwy - $yo{$name} + $y{$name});
	   $xa1 = int($x{$name} - $xl );
	   $xa2 = int($x{$name} - $xl - 15);
	   $ya = int($y{$name} - $yl);
	   $coord = "[$xl:$xu,$yl:$yu]";
	   $imagefile = $image{$name};
	   $imagefile =~ s/\.fits//;
	   $fits = "$imagefile$count.fits";
	   $giffile = "${imagefile}${count}C$chip.gif";
	   $gif = File::Spec->catfile($root,"$giffile");
	   unlink $fits;
	   $result = `$imcopy "$image{$name}$coord" $fits`;
	   print stderr $result;
	   $cmd = "$convert  -equalize +contrast +contrast +contrast +contrast +contrast +contrast $fits -fill none -stroke red -draw 'circle $xa1,$ya $xa2,$ya' $gif";
	   $result = `$cmd`;
	   print HTML "<TD Align=center ><img src=$giffile></TD>\n";
	   unlink $fits;
	}
	print HTML "<TD><input type=checkbox name=\"a[]\" value=\"$MPC\"></TD>";
	print HTML  "</TR>";
	print HTML "<TR bgcolor=$color ><TD colspan=4 ><PRE>";
	open (MPC,"<$MPC");
	while (<MPC>) {
	  print HTML $_;
	}
	  print HTML "</PRE></TD></TR>\n";
	print HTML "<TR bgcolor=black><TD colspan=4>%nbsp;</TD></tR>";
}
print HTML  "</TABLE><input type=submit></FORM></HTML>\n";
close (HTML);


__END__

=head1 NAME

extract.pl - extract postage stamps from around candidate objects based on there location in the cands.comb file.  Produce a www page with the extracted images.

=head1 SYNOPSIS

extract.pl [options]

  Options:
   -f --file		Input .cands.comb format file
   -r --root		Root directory of the www page that will be created

=cut

