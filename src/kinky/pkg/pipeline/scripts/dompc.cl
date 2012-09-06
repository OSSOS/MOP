#/* program to take the output mkpltsol and runs PHOT on those stars
#   to determine an internal calibration
#*/

procedure dompc (inlist) 

	string inlist {"",prompt="file list of images to do calibration for"}
	string prefix {"fk",prompt="prefix of usno searched image"}

	string *dompclist

begin

	string infile;
	string t_inlist;
	int i;
        real fwhm;	
	string plant;
	
	t_inlist = inlist;
	dompclist = t_inlist;
	
	while ( fscan(dompclist,infile,fwhm,plant)!=EOF  ) 
           if ( stridx("#",substr(infile,1,1)) == 0 ) 
             break;
         
	 infile = prefix//infile;
	 kdelete("fkaper.corr");
	 measure3(infile);
	 usno_phot(t_inlist,> "fkaper.corr");
         mpcauto(infile//".cands.astrom");
end
