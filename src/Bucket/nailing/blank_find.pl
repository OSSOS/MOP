#!/usr/bin/env perl

## Input: observations catalog and RA, DEC, DRA, DDEC, EPOCH.
## Output: a single frame from a different epoch then input but of the same RA/DEC


## Convert segsi-string to decimal
sub seg2deg {
        my $str = shift ;
	my $sign = $str =~ m/^-/ ? -1 : 1;
	$str =~ s/[\:\;\-\/]/ /g ;
        my ($hr,$mm,$ss) = split " ",$str;
        my $dec = $hr + ($mm + $ss/60.0)/60.0;
	$dec = $dec*$sign;
        return $dec;
}

use Getopt::Long;
use Bucket::Control;
use Bucket::MOP;
use Pod::Usage; 
use IPC::Open2;
use File::Path;
use File::Basename;
use File::Spec;
use File::Temp qw/ tempfile tempdir /;
use Astro::SLA;
use strict;

my ( $ra, $dec, $file_id, $sec, $debug, $quiet, $verbose, $help) ;

GetOptions( 'debug' => \$debug,
	   'r|ra=f' => \$ra,
	   'd|dec=f' => \$dec,
	   'h|help' => \$help,
	   'f|file_id=s' => \$file_id,
	   's|sec=s' => \$sec) || pod2usage(2);

pod2usage(1) if ($help);




warn "Looking through frame catalog for images containg RA/DEC\n" if ($debug);
### epoch into a date.
my @date;
my $status;

#slaDjcal(0,$epoch,@date,$status);
#$status==0 || die "Error converting $epoch to YY/MM/DD @date\n";
#my $date = sprintf("%04d-%02d-%02d",@date);
my $exp = Bucket::MOP->new("wcs","cfhls","shift+add");

my $raRAD = $ra/57.3;
my $decRAD = $dec/57.3;
my $x1 = cos($raRAD)*cos($decRAD);
my $y1 = sin($raRAD)*cos($decRAD);
my $z1 = sin($decRAD);

### angular seperation between the requested location and chip center
my $dangle = "degrees(acos(";
$dangle .= "cos(radians(crval1 + (naxis1/2.0 - crpix1)*cdelta1/3600.0))";
$dangle .= "*cos(radians(crval2+(naxis2/2.0 - crpix2)*cdelta2/3600.0))*$x1 ";
$dangle .= " + sin(radians(crval1 + (naxis1/2.0 - crpix1)*cdelta1/3600.0))";
$dangle .= "*cos(radians(crval2 + (naxis2/2.0 - crpix2)*cdelta2/3600.0))*$y1 ";
$dangle .= " + sin(radians(crval2+ (naxis2/2.0 - crpix2)*cdelta2/3600.0))*$z1";
$dangle .= "))";
### imsize is the size of the chip + the error ellipse size (as a circle) 
my $imsize = " GREATEST(naxis1*cdelta1/3600.0,naxis2*cdelta2/3600.0)*2.0 ";
my %select = ( "WHERE" => "$dangle < $imsize " );
#$select{ORDER} = " ABS(TO_DAYS(date_obs) - TO_DAYS('$date') ) DESC";
$select{ORDER} = " $dangle ";
$select{WHERE} .= "AND file_id NOT LIKE '$file_id'";

my $nrows=$exp->selectRows(\%select);

$nrows > 0 || die "Failed to find an image near $ra, $dec  ";

my $received = Bucket::MOP->new("received","cfhls","shift+add");
while(my $row=$exp->fetchRow ){
       #compare to global RA/DEC and see if we're close to the target.
       $received->selectRows("file_id LIKE '".$exp->{file_id}."'") || next;
       $received->fetchRow;
       my ($x, $y) = my_sky2xy ($received->{active}, $ra, $dec);

       my $possible=($y>0 && $y<$exp->{naxis2} && $x>0 && $x<$exp->{naxis1}) ? 1 : 0 ;
       ## use this field
       my $ang=0;
       
       my $file = $ENV{IMAGE_BASE_DIR} ? $ENV{IMAGE_BASE_DIR}."/".$received->{active} : $received->{active};
       my ( $YY,$MM,$DD ) = split '[-:/]',$exp->{date_obs};
       my ( $hh,$mm,$ss ) = split '[-:/]',$exp->{utc_obs};
       my $dfrac = ($hh + ($ss/60.0 + $mm)/60.0)/24.0;
       my $mjdate = 0;
       slaCaldj($YY,$MM,$DD,$mjdate,my $status);
       $status == 0 || die "error converting $exp->{date_obs} \n";
       $mjdate = $mjdate + $dfrac;
       printf "$file $mjdate $ra $dec 0 0 0 \n" if ( $possible ) ;
       exit(0) if ( $possible ) ;
}
warn "Lookup completed\n";

exit(-1) ;



__END__


=head1 NAME

obs_find.pl - search a catalog of images for those which may contain a particular moving object.

=head1 SYNOPSIS

obs_find.pl [--sec ## --debug --orbfit directory] --abg file.abg --cat file.cat

  Args:
    --abg	an orbit file in orbfit format
    --cat	a catalog of images
  Option:
    --sec       width of image section to extract (in pixels)
    --help	Display this info.
    --orbfit    home directory of the predict program 
    --debug	display debug message

=head1 ARGUMENTS

=over 8

=item B<--abg>

The abg file is produce by I<fit_radec> (G. Bernstein & B. Khushalani 2000, AJ 120 3323).   
The location of the object is calculated using I<predict>.

=item B<--cat>

A catalog of fits images which may contain the object whose orbit is
in the abg file provided.  The catalog file is produced using
I<build_cat.pl>

=head1 OPTIONS

=item B<--sec>

Size of image blocks to display when checking reality of a detection.

=item B<--orbfit>

This is the directory where I<fit_radec> and I<predict> are to be executed.
Some versions of these programs require that you execute them inside the directory where the observatory
and DES files live (assumed to be the same).  If orbfit is not given then the programs are exectuted in 
the directory where they resiside.

=back

=head1 USAGE

The I<obs_find.pl> program is part of the MOP.  This script is meant to produce 
a list of images sections of size I<sec> which may contain (based on B<predict>) the object of interest.

The output list can be used as input to B<check.pl> to accept
observations and measure their positions using local determined
astrometric measurements. B<check.pl doesn't exist yet!>



