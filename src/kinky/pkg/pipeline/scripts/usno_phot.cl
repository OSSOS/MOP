#/* program to take the output mkpltsol and runs PHOT on those stars
#   to determine an internal calibration
#*/

procedure usno_phot (inlist) 

	string inlist {"",prompt="file list of images to do calibration for"}
	string prefix {"fk",prompt="prefix of usno searched image"}

	string *images
	string *apfile

begin

	string infile;
	int i;
	real dmag, derr;
	real apcor,aperr;
	int  apin,apout;
        real fwhm;	
	string plant;
	
	images = inlist;
	
	while ( fscan(images,infile,fwhm,plant)!=EOF  ) {
           if ( stridx("#",substr(infile,1,1)) != 0 )
             next;

	   apfile = infile//".apcor";
           i = fscan(apfile,apin,apout,apcor,aperr)	

	   infile = prefix//infile;

	   photpars.apertures=apout;

	   kdelete(infile//".usno.mag.1");
	   phot(infile,infile//".usno",infile//".usno.mag.1");
	   kdelete(infile//".phot");
	   txdump(infile//".usno.mag.1","XCEN,YCEN,MAG,MERR","PIER==0",> infile//".phot");

	   usno_calib("--usno "//infile//".usno --phot "//infile//".phot" ) | scan(dmag,derr) ; 
	   apcor = apcor+dmag;
	   aperr = sqrt(aperr**2 + derr**2);
	   printf("%4d %4d %8.3f %8.3f\n",apin,apout,apcor,aperr);
      }

end
