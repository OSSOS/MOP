#!/usr/bin/env perl

## get the name and location of a .fits image.  Attempt to refine the
## WCS. store the refinded wcs in the wcs table.

use Bucket::MOP;
use Bucket::Control;
use File::Basename;
use strict;
use warnings;


### gets files that have have status lines for task "wcs"
### where the success column IS NULL [ie, doesn't exist]OB

my $condition="success IS NULL OR success=0";
my $this_fileid=0;
$this_fileid=shift if ($#ARGV>-1);
$condition  = "WHERE file_di LIKE '".$this_fileid."'" if ( $this_fileid ) ;
my $files= Bucket::MOP->statusSelect("wcs",$condition);

## select a list of files to check that don't have wcs status lines 
## in the status table.  This is accomplished via a left join

while ( my $row = $files->fetchRow  ) {
    ## Pocessing the image so we should
    ## create values for the status table to indicate we're 
    ## processing the image [avoids race conditions]
    next unless $files->lock("wcs"); 	
    $files->{file_id} =~ m/^\d{6}o_\d\d$/ || next;  ### dont' do this file_ids that aren' odemeter
    print STDERR "processing .... $files->{file_id} \n";

    my $dir = dirname($files->{"file"}) ;
    my $image = basename($files->{"file"});
    $image =~ s/.fits$//; ### strip off the trailing .fits (if it's there)
    chdir $dir || eval {
	my $err = "Couldn't cd to $dir";
	$files->unlock("wcs",0,$err);
	die $err;
    };
    ### assume faliure
    my $success=0;
    eval {
	### not sure what happens if the program that MOP_execute calls dies, so 
	### i'm doing this inside an eval to catch those exceptions
	### this allows the unlock function to be called if needed.
	MOP_execute("get_cenphot.pl"," -f $image --force ","force");
    };
    eval {
	$files->unlock("wcs",0,"get_cenphot: bad wcs solution");
	next;
    } if ( $Bucket::Control::err ) ;
    ### get the wcs info from the image and store in the DB
    my $wcs = Bucket::MOP->new("wcs");

    ### check if there is already a wcs entry for this frame
    my $type = $wcs->selectRows({WHERE => "file_id LIKE \'$files->{file_id}\'" }) ?
	"updateRow" : "insertRow" ;
	
    my $cmd = "wcshead ".$files->{"file"};
    ($wcs->{file}, 
     $wcs->{naxis1}, $wcs->{naxis2}, 
     $wcs->{ctype1}, $wcs->{ctype2}, 
     $wcs->{crval1}, $wcs->{crval2}, 
     $wcs->{radecsys}, 
     $wcs->{crpix1}, $wcs->{crpix2}, 
     $wcs->{cdelta1}, $wcs->{cdelta2}, 
     $wcs->{crota}) = split ' ',`$cmd`;
    $wcs->{file_id} = $files->{file_id};
    $wcs->{date_obs} = $files->{date};
    $wcs->{utc_obs} = $files->{time};

    $wcs->$type || eval {
	my $err = "Insert of wcs failed";
	warn $err." ".$files->{file};
	$files->unlock("wcs",0,"$err");
	next;
    };
    $files->unlock("wcs",1,"Normal Termination");
}





