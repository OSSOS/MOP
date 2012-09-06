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

use strict;

my $orbfit_home = dirname(`which predict`);

my $cat;
my $abg;
my $debug=0;
my $sec=250;
my $help;
GetOptions('a|abg:s' => \$abg,
           'c|cat=s' => \$cat,
	   'd|debug' => \$debug,
	   'o|orbfit' => \$orbfit_home,
	   'h|help' => \$help,
	   's|sec=s' => \$sec) || pod2usage(2);

-e $cat || pod2usage(1);
-e $abg || pod2usage(1);
-e "$orbfit_home/predict" || pod2usage(1);

pod2usage(1) if ($help);


$abg = File::Spec->rel2abs($abg);

my $orbit = `grep a= $abg`;
warn $orbit;
my $chiline = `grep Chi-squared $abg`;
chomp $chiline;
$chiline =~ m/\D*\s(\d+.\d+)\sDOF: \d/;
warn "$chiline -- $1\n";
if ($1 > 4.5 ) {
	warn "Large Chi-Square. Continue?\n";
	my $ans = <STDIN>;
	exit if ( $ans =~ m/NO/ ) ;
}

warn "Opening $cat to load frame info\n" if ( $debug) ;
open (CAT,"< $cat" ) ;
my @keys = ();

#K A bunch of key words.
### only allow ONE Keyword header line in a file
my $line = <CAT> ;
chomp ($line);
$line =~ s/^#\S*// ;
@keys = split(' ',$line);
warn "Read of header line completed, @keys\n" if ($debug);

my @values=();
my %rows=();
my %ra;
my %dec;


warn "Looking through frame catalog for images containg target\n";

while ( $line =<CAT> ) {
    my %row=();
	## values and keys are both arrays so their order stays constant.
	## the order in %row is not fixed but that's OK
	@values = split(' ',$line);
	foreach my $key ( @keys ) {
	   $row{$key} = shift @values;   
	}

	## convert the YY-MM-DD of DATE-OBS to parts
        my ($YY, $MM, $DD ) = split('[-/:]',$row{'DATE-OBS'});
	$DD = $DD + $row{'MJDATE'} - int($row{'MJDATE'})+ $row{EXPTIME}/3600.0/2.0/24.0;
	warn "using predict to locate candidate\n" if ($debug);
        warn "$abg $row{FILENAME} ->$YY $MM $DD<- $row{CODE}\n" if ( $debug);
	my $pwd = `pwd`;
	chomp($pwd);

	## send the date and obs code to predict.
	my ($wfh,$filename) = tempfile(SUFFIX=>'.pre');
        print $wfh "$row{CODE}\n$YY $MM $DD\n-1\n";
	close $wfh;
	my $pid = open(PREDICT,
			"(cd $orbfit_home; predict $abg ) < $filename 2>>/dev/null | grep ICRS |");
	$line = <PREDICT>;
	close(PREDICT);
	if ( $debug ) {
	  my $cmd = "(cd $orbfit_home; predict $abg ) < $filename " ;
	  warn $cmd;
	  my $result = `$cmd`;
	  warn $result;
	  `cat $filename`;
	}

	unlink $filename;
	chomp $line;
	warn "Output of predict:$line:\n" if ($debug);
	my ($dum1,$dum2,$ra,$dec,$dra,$ddec,$ang)  = split(' ',$line);
	## wait till $pid exists (zombie prevention)
 
	warn "Predicted location $ra $dec\n" if ($debug);

	#compare to global RA/DEC and see if we're close the target.
	my $raDEG = seg2deg($ra)*15.0;
	my $decDEG = seg2deg($dec);
	my ($x, $y) = sky2xy (\%row, $raDEG, $decDEG);
	warn "$x $y $row{CRPIX1} $row{CRPIX2} \n" if ($debug);
        my $du =  $dra/($row{CD1_1}*3600.0);
        my $dv =  $ddec/($row{CD2_2}*3600.0);
	my $dx = $du*cos($ang/57.3) - $dv*sin($ang/57.3);
	my $dy = $dv*cos($ang/57.3) + $du*cos($ang/57.3);
	warn "Error ellipse  mapping assumes x/y axis aling to RA/DEC axis (no rotation)\n" if ( $debug);
 
	
	my $possible=($y+$dy>0 && $y-$dy<$row{NAXIS2} && $x-$dx>0 && $x+$dx<$row{NAXIS1}) ? 1 : 0 ;
	print "$possible\n" if ($debug) ;
	## could be in this field
	if ( $possible ) { 
	    printf "$row{FILENAME} $row{MJDATE} $raDEG $decDEG $dra $ddec $ang\n";
	}
}
warn "Lookup completed\n";

exit(0) ;


sub sky2xy {
	my $row = shift;
	my $ra = shift;
	my $dec = shift;
	my %delta=();
	if ( $debug  ) {
	   foreach my $key ( keys %$row ) {
		warn "$key-> $row->{$key} \n";
	   }
	   warn "$ra $dec\n";
	}
	$delta{RA} = cos($dec/57.3)*($ra - $row->{CRVAL1} );
	$delta{DEC} = ($dec - $row->{CRVAL2} );

	$delta{Y} = ($row->{CD1_1}*$delta{DEC} - $row->{CD2_1}*$delta{RA})/
	    ($row->{CD1_1}*$row->{CD2_2}-$row->{CD1_2}*$row->{CD2_1});
	$delta{X} = ($row->{CD2_2}*$delta{RA} - $row->{CD1_2}*$delta{DEC})/
	    ($row->{CD1_1}*$row->{CD2_2}-$row->{CD1_2}*$row->{CD2_1});

	my $x = $delta{X} + $row->{CRPIX1} ;
	my $y = $delta{Y} + $row->{CRPIX2} ;
	warn ("$ra $dec -> $x $y\n") if ($debug ) ;
	return ($x,$y);
}

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



