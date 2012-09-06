#!/usr/bin/perl 

## JJK Jun 2003


##
## LOGIC (If you beleave scripts should be so constrained)
## Read in a cands.comb file [produced by MOP::find.pl] and
## create orbfit .abg files for each object in the cand.comb 
## file
##


sub cand_comb($);
	## Long live emacs      {} 
use Getopt::Long;
use Pod::Usage;
#use strict;
use File::Basename;

my $dir = 0;
my $cand = 0;
my $help = 0;
my $debug =0;

my $orbfit_home = dirname(`which predict`);

GetOptions('h|help|?' => \$help,
	   'd|debug' => \$debug,
	   'c|cand=s' => \$cand ) || pod2usage();

pod2usage() if ( $help || !$cand );

my $cand_file = cand_comb $cand ;

print STDERR @{$cand_file->{ARRAY}}." object(s) in the candidate list\n";

my @frames = sort { $a cmp $b } keys %{$cand_file->{HDU}};

foreach my $frame ( @frames ) {
    `getcenter.pl -f $frame`;
}

## get the aperture corrections
foreach my $frame (@frames) {
    if ( -e $frame.".apcor" ) { 
	open (APCOR,"< $frame.apcor");
	my ($apin,$apout,$apcor,$aperr) = split(' ',<APCOR>);
	close(APCOR);
    } else {
	my ( $apin, $apout, $apcor, $aperr) =  ( 4, 10, 0.3, 9.99) ;
	warn "Aperture correction doesn't exist? Using hard coded values $frame.apcor";
    }
    $cand_file->{HDU}{$frame}{'APCOR'} = $apcor;
    $cand_file->{HDU}{$frame}{'AP'} = $apin;
    $cand_file->{HDU}{$frame}{'INSKY'} = $apout*5.0;
    $cand_file->{HDU}{$frame}{'OUTSKY'} = $apout*7.0;
}
    
## get the zeropoint for the images
-e "zeropoint.used" || die "Cann't get the zeropoint for this frame\n";
open (ZP,"<zeropoint.used");
$cand_file->{HDU}{$frames[0]}{'ZEROPOINT'} = <ZP>;
chomp $cand_file->{HDU}{$frames[0]}{'ZEROPOINT'}  ;
my $zp = $cand_file->{HDU}{$frames[0]}{'ZEROPOINT'} ;
close(ZP);

my $num_obj=0;
foreach my $line  ( @{$cand_file->{ARRAY}} ) {
        $num_obj++;
	my $mpc;
	my $chipid = sprintf "%x", `objname $frames[0]{'CHIP'}`;
	my $obj_name=sprintf "%s%s%s", $cand_file->{HDU}{$frames[0]}{'EXPNUM'},$chipid,$num_obj;
	print "$obj_name\n";
	foreach my $frame ( @frames ) {
	    my %this =  %{$line->{$frame}};
	    my $cmd = "dophot.pl --file $frame";
	    $cmd .= " --ap $cand_file->{HDU}{$frame}{AP}";
	    $cmd .= " --insky $cand_file->{HDU}{$frame}{INSKY}";
	    $cmd .= " --outsky $cand_file->{HDU}{$frame}{OUTSKY}";
	    $cmd .= " --maxcount $cand_file->{HDU}{$frame}{MAXCOUNT}";
	    $cmd .= " --zeropoint $zp";
	    $cmd .= " --exptime $cand_file->{HDU}{$frame}{EXPTIME}";
	    $cmd .= " --xcen $this{X} --ycen $this{Y}";
	    print "$cmd\n" if ($debug);
	    my $check;
	    my @dophot = split (' ',`$cmd | grep DOPHOT`);
	    $this{ERR} = pop @dophot;
	    $this{MAG} = pop @dophot;
	    $this{Y_0} = pop @dophot;
	    $this{X_0} = pop @dophot;
	    print "$this{X_0} $this{Y_0} $this{MAG} $this{ERR} \n" if ( $debug);
	    ($check,$check,$check,$this{RA},$this{DEC}) = split(' ',`xy2sky -j -n 2 $frame.fits $this{X_0} $this{Y_0} `);
	    
	    $this{RA} =~ s/:/ /g;
	    $this{DEC} =~ s/:/ /g;
	    $this{MAG} = $this{MAG} - $cand_file->{HDU}{$frame}{APCOR};
	    $mpc .= sprintf "     %7.7s  C%16.16s %11.11s %11.11s         %5.1f        568\n",
	    $obj_name,$cand_file->{HDU}{$frame}{'MJD-OBS-CENTER'},$this{RA},$this{DEC},$this{MAG};
	}
	open (FIT,"| (cd $orbfit_home; fit_radec ) > $obj_name.abg" );
	open (MPC,">> $obj_name.MPC");
	print FIT $mpc;
	print MPC $mpc;
	close(MPC);
	close(FIT); 
}


exit;

## Now loop over each directory and see if an object exists there
foreach my $dir ( @ARGV ) {
    
    my @files = split(' ',`ls $dir/*.fits`);

    ## OK.  There need to be fits files to proceed here
    @files > 0 or next ;

    my %date;
    my %ra;
    my %dec;
    my $avg=0;
    my $count=0;
    ## Get the date of obsevation and RA/DEC for this file
    foreach my $file ( @files ) {
	$file =~ s/\.fits//;
	-e "$file.mopheader" or `stepZjmp $file`;
	-e "$file.mopheader" or die "Failed to create mopheader?\n";
	$date{$file} = `gethead MJD-OBSC $file.mopheader`;
        chomp $date{$file};
	$ra{$file} = `gethead CRVAL1 $file.mopheader`;
	chomp $ra{$file};
	$dec{$file} = `gethead CRVAL2 $file.mopheader`;
	chomp $dec{$file};
    }
}




## cand_comb.pl
## subroutine to ingest the contents of a cand.comb
## designed for VERIOSN 1.0 of MOP

sub cand_comb($) {

    my $dubug = 0;
    my $cand = shift;


## open the candidate file for read only
    open (CAND,"< $cand");
    
## frames are given by lines with a leading # 
## lines with ## indicate the start of a header line
    
## Get the names of the frames
    my $nf = 0;
    my @frames =();
    while (<CAND>) {
	chomp;
	last if ( $_ =~ m/^\#\#/ ); 
	$_ =~ m/^#\s(\S*)\s*$/;
	
	$frames[$nf] = $1;
	$frames[$nf] =~ s/^fk//;
	print "read $frames[$nf] as frame: $nf\n" if ( $debug );
	$nf++;
    } 
	
## First line after a file name is the version number of the file.
    $_ =~ m/## MOP version 1.0/ or die "Don't understand line\n $_\n Exiting\n";
    
    my @headers;
    my %keys = ();
    my @keywords;
    my $frame;
    my $fnum=0;
    
    while (<CAND>) {
	$_ =~ m/^\#/ or last;
	if ( $_ =~ m/^\#\#/ ) {
	    @keywords = split(' ',$_);
	    if ( $_ =~ m/MOP version/ ) {
		my $line = $_;
		## check the version
		$line =~ m/\#\# MOP version (\d*\.\d*)\D*$/;
		$1 == 1.0 or die "$line; I cann't read $1 MOP files\n";
		$fnum ++;
	    }
	    next;
	}
	$frame = $frames[$fnum];
	my @values = split(' ',$_);
	foreach my $keyword ( @keywords )  {
	    my $value = shift @values;
	    if ( $keyword =~ m/MJD-OBS-CENTER/ ) {
		$value .= " ".(shift @values)." ".(shift @values);
	    }
	    $keys{$frame}{$keyword} = $value ;
	}
    }
    
    print "$_ ....\n" if ( $debug);
    if ($debug) {
	my $k = \%keys;
	foreach my $frame  ( keys %keys ) {
	    foreach my $keyword ( keys %{$keys{$frame}} ) {
		print "$frame,$keyword,$k->{$frame}{$keyword}\n";
	    }
	}
    }
    
    
## get rid of the leading comment mark and white spaces
    shift @keywords;
    my @columns = @keywords;
    my @lines;
    my $line;
    my $nlines=0;
    
    while ( $line = <CAND> ) {
	foreach my $frame ( @frames )  {
	    print "$frame: $line" if ( $debug);
	    my @line = split(' ',$line);
	    foreach my $col ( @columns ) { 
		$lines[$nlines]{$frame}{$col} = shift @line;
	    }
	    $line = <CAND>;
	}
	$nlines++;
    }
    my %file;
    $file{"HDU"} = \%keys;
    $file{"ARRAY"} = \@lines;
    
## Now we have the content of the file
    
    close(CAND);
    
    return (\%file);
    
}


__END__

=head1 SYNOPSIS

B<abg_gen.pl> generate a set of .abg files using output from MOP

=head1 USAGE

B<abg_gen.pl> --cand file.cands.comb

=head1 OPTIONS

=over 8

=item B<--cand>

A candidate file in the format produced by I<find.pl> as part of MOP.

=back

=head1 USAGE

B<abg_gen.pl> takes as input the found candidates file as generated 
by I<find.pl>.  The I<abg> file contains the orbit fit as dertmined by I<fit_radec> (G. Bernstein & B. Khushalani 2000, AJ 120 3323).  Each object in the cands.comb file generates it's own OBJ#.abg file which contains the fit and a file 
names MPC which contains the astrometric measures in MPC format.

=head1 NOTES

The names of the object abg and MPC files should be more intellegent.


