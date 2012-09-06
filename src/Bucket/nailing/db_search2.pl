#!/usr/bin/env perl

## Input: abg file and observations catalog.
## Output: List of images and sections where the object is observable


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
use Pod::Usage; 
use IPC::Open2;
use File::Path;
use File::Basename;
use File::Spec;
use File::Temp qw/ tempfile tempdir /;
use Bucket::Control;
use Bucket::MOP;
use warnings;
use Astro::SLA qw(:constants :sla);


use strict;


my $abg;
our $debug=0;
my $sec=250;
my $flist="";
my $help;
GetOptions('a|abg:s' => \$abg,
	   'f|flist:s' => \$flist,
	   'd|debug' => \$debug,
	   'h|help' => \$help,
	   's|sec=s' => \$sec) || pod2usage(2);

-e $abg || pod2usage(1);

my $set = "";
### if a list of files to check was give then search those first.
warn "Trying to use input list $flist\n" if ( $debug);
if ( -e $flist ) { 
    my @set = ();
    open (FLIST,"<$flist");
    while (<FLIST>) {
	chomp;
	s/\.fits$//;
	push @set, $_ ;
    }
    $set = join(",",@set);
} 
warn "Set of images to search is $set " if ( $debug ) ;

### create the selection rule that will get the exposures
my %select = ();
$select{GROUP} = "date_obs";
$select{WHERE} = "naxis1 IS NOT NULL ";
if ( $set ) { 
    $select{WHERE} .= "AND FIND_IN_SET(file_id,\'$set\') ";
}

pod2usage(1) if ($help);


$abg = File::Spec->rel2abs($abg);
### password in the file?
my $exp = Bucket::MOP->new("wcs","cfhls","shift+add");
my $received = Bucket::MOP->new("received","cfhls","shift+add");


my $orbit = `grep a= $abg`;
print STDERR $orbit;
my $chiline = `grep Chi-squared $abg`;
chomp $chiline;
$chiline =~ m/\D*\s(\d+.\d+)\sDOF: (\d+)/;
print STDERR  "$chiline \n";
if ($1/$2 > 1.5 ) {
	print STDERR  "$chiline -- $1 DOF $2\n";
	my $ans = <STDIN>;
	exit if ( $ans =~ m/NO/ ) ;
}



### get a list of dates exposures where made
my $ndates = $exp->selectRows(\%select);

warn "Got a list of $ndates dates from the exposure db \n" if ( $debug) ;


my @values=();
my %rows=();
my %ra;
my %dec;
my $ntot =0;
$ndates = 0;
warn "Looking through wcs catalog for images containg target \n" if ($debug);
while ( my $row = $exp->fetchRow ) {
    $ndates ++;
    ## convert the YY-MM-DD of DATE-OBS to parts
    my ($YY, $MM, $DD ) = split('[-/:]',$exp->{'date_obs'});
    ## look up the RA/DEC for this DAY and see if any exposures
    ## are withing 1 degree
    my ($hh, $mm, $ss ) = split('[-/:]',$exp->{'utc_obs'});
    my $frac = $DD + ($hh + ($mm + $ss/60.0)/60.0)/24.0;
    my $date = "$YY $MM $frac";
    warn "Getting object position on $date \n" if ( $debug ) ;

    my ( $raRAD,$decRAD,$dra,$ddec,$ang ) = predict($abg,$date,568);
    warn "Predicted location using $abg on $date is  $raRAD $decRAD, $dra, $ddec,$ang \n" if ( $debug );
    my $err_circ = sqrt($dra**2 + $ddec**2)/3600.0;    
    my $exp2 = Bucket::MOP->new("wcs","cfhls","shift+add");
    my $x1 = cos($raRAD)*cos($decRAD);
    my $y1 = sin($raRAD)*cos($decRAD);
    my $z1 = sin($decRAD);
    ### angular seperation between the candidate location and chip center
    my $dangle = "degrees(acos(";
    $dangle .= "cos(radians(crval1 + (naxis1/2.0 - crpix1)*cdelta1/3600.0))";
    $dangle .= "*cos(radians(crval2+(naxis2/2.0 - crpix2)*cdelta2/3600.0))*$x1 ";
    $dangle .= " + sin(radians(crval1 + (naxis1/2.0 - crpix1)*cdelta1/3600.0))";
    $dangle .= "*cos(radians(crval2 + (naxis2/2.0 - crpix2)*cdelta2/3600.0))*$y1 ";
    $dangle .= " + sin(radians(crval2+ (naxis2/2.0 - crpix2)*cdelta2/3600.0))*$z1";
    $dangle .= "))";
    ### imsize is the size of the chip + the error ellipse size (as a circle) 
    my $imsize = " GREATEST(naxis1*cdelta1/3600.0,naxis2*cdelta2/3600.0)*2.0 + $err_circ*10.0";
    my %select = ( "WHERE" => "$dangle < $imsize AND date_obs='".$exp->{date_obs}."'" );
    if ( $set ) { 
       $select{WHERE} .= "AND FIND_IN_SET(file_id,\'$set\') ";
    }
    my $nmatches = $exp2->selectRows(\%select);
    $ntot += $nmatches;
    next unless $nmatches;
    warn "search through $nmatches frames from $exp->{date_obs} \n" if ( $debug ) ;

    ### 1 or more frame was within 2 degrees on this date
    ### so a more careful check is needed	    
    while ( my $row = $exp2->fetchRow ) {
	### get the location of the file from the received table
	$received->selectRows("file_id LIKE '".$exp2->{file_id}."'");
	$received->fetchRow;
        next unless -e $received->{active};
	my ($YY, $MM,$DD) = split '[-:/]',$exp2->{date_obs};
	my ($i, $stat, $dfrac);
	$i=1;
	slaDafin($exp2->{utc_obs},$i,$dfrac,$stat);
	$stat==0 || die "error converting $exp2->{utc_obs} to fractional day\n";
	$dfrac = $DD+$dfrac*15/(2*DPI);
	my $date = "$YY $MM $dfrac";
	my ( $ra, $dec, $dra, $ddec, $ang ) = predict($abg,$date,568);
	$ra = $ra * 180.0/DPI;
	$dec = $dec * 180.0/DPI;
	my ($x, $y) = my_sky2xy ($received->{active}, $ra, $dec);
	warn "$x $y ($exp2->{naxis1} , $exp2->{naxis2} ) \n" if ($debug);
	my $du =  $dra/$exp2->{cdelta1};
	my $dv =  $ddec/$exp2->{cdelta2};
	my $dx = abs($du*cos($ang/57.3) - $dv*sin($ang/57.3));
	my $dy = abs($dv*cos($ang/57.3) + $du*cos($ang/57.3));
	warn "Rotated error elipse is $dx $dy in pixels\n" if ($debug);
	warn "Error ellipse  mapping assumes x/y axis aling to RA/DEC axis (no rotation)\n" if ( $debug);
	my $possible=($y+$dy>-200 && $y-$dy<200+$exp2->{naxis2} && $x+$dx>-200 && $x-$dx<200+$exp2->{naxis1}) ? 1 : 0 ;
	print STDERR "$possible\n" if ($debug) ;
	## could be in this field
	if ( $possible ) { 
	    my ( $YY,$MM,$DD ) = split '[-:/]',$exp2->{date_obs};
	    my ( $hh,$mm,$ss ) = split '[-:/]',$exp2->{utc_obs};
	    my $dfrac = ($hh + ($ss/60.0 + $mm)/60.0)/24.0; 
	    my $mjdate = 0;
	    slaCaldj($YY,$MM,$DD,$mjdate,my $status);
	    $status == 0 || die "error converting $exp->{date_obs} \n";
	    $mjdate = $mjdate + $dfrac;
	    ### get the location of the file 
	    printf "$received->{active} $mjdate $ra $dec $dra $ddec $ang\n";
	}
    }
}

print STDERR "Lookup completed after checking $ntot frames on  $ndates dates \n";

exit(0) ;

sub predict {
    my $abg = shift;
    my $date = shift;
    my $code = shift;
    ## send the date and obs code to predict.                                                                            
    my $orbfit_home = dirname(`which predict`);
    -e $orbfit_home."/predict" || die "cann't find predict in $orbfit_home \n";

    ### store the current directory so we can come back after running predict
    my $pwd = `pwd`;
    chomp($pwd);


    my ($wfh,$filename) = tempfile(SUFFIX=>'.pre');
    print $wfh "$code\n$date\n-1\n";
    close $wfh;
    my $pid = open(PREDICT,
                   "(cd $orbfit_home; predict $abg ) < $filename 2>>/dev/null | grep ICRS |");
    my $line = <PREDICT>;
    close(PREDICT);
    unlink $filename;
    chomp $line;
    my ($dum1,$dum2,$ra,$dec,$dra,$ddec,$ang)  = split(' ',$line);
    $ra =~ s/:/ /g;
    $dec =~ s/:/ /g;
    my $i=1;
    my $status=0;
    my $ra_rad;
    my $dec_rad;
    slaAfin($ra,$i,$ra_rad,$status);
    $ra_rad = $ra_rad*15.0;
    $status == 0 || die "Error converting $ra to radians: code $status\n";
    $i =1;
    $status=0;
    slaAfin($dec,$i,$dec_rad,$status);
    $status == 0 || die "Error converting $dec to radians\n";
    return ($ra_rad,$dec_rad,$dra,$ddec,$ang );
}
    


#<<<<<<< db_search.pl
#sub sky2xy {
#	my $row = shift;
#	my $ra = shift;
#	my $dec = shift;
#	my %delta=();
#	#if ( $debug ) {
#	#   foreach my $key ( keys %$row ) {
#	#	warn "$key-> $row->{$key} \n";
#	#   }
#	#   warn "$ra $dec\n";
#	#}
#	$delta{RA} = 3600.0*cos($dec/57.3)*($ra - $row->{crval1} );
#	$delta{DEC} =3600.0* ($dec - $row->{crval2} );
#
#	my $cr = cos($row->{crota}/57.3);
#	my $sr = sin($row->{crota}/57.3);
#
#	my $x = ($delta{RA}*$cr + $delta{DEC}*$sr)/$row->{cdelta1} + $row->{crpix1} ;
#	my $y = ($delta{RA}*$sr + $delta{DEC}*$cr)/$row->{cdelta2} + $row->{crpix2} ;
#	warn ("$ra $dec -> $x $y\n") if ( $debug ) ;
#	return ($x,$y);
#}
#=======
#>>>>>>> 1.2

__END__


=head1 NAME

db_search.pl - search the mySQL db for those images that may contain a particular moving object.

=head1 SYNOPSIS

obs_find.pl [--sec ## --debug --flist file.flist ] --abg file.abg 

  Args:
    --abg	an orbit file in orbfit format
  Option:
    --flist	a list of images that might contain the object
    --sec       width of image section to extract (in pixels)
    --help	Display this info.
    --debug	display debug message

=head1 ARGUMENTS

=over 8

=item B<--abg>

The abg file is produce by I<fit_radec> (G. Bernstein & B. Khushalani 2000, AJ 120 3323).   
The location of the object is calculated using I<predict>.

=item B<--flist>

A list of fits images which may contain the object whose orbit is
in the abg file provided.  

=head1 OPTIONS

=item B<--sec>

Size of image blocks to display when checking reality of a detection.

=back

=head1 USAGE

The I<obs_find.pl> program is part of the MOP.  This script is meant to produce 
a list of images sections of size I<sec> which may contain (based on B<predict>) the object of interest.

The output list can be used as input to B<check.pl> to accept
observations and measure their positions using local determined
astrometric measurements. B<check.pl doesn't exist yet!>



