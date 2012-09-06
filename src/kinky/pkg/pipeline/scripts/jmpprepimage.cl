# JJK July 2001.
#
# This scripts contains a BIG kluge.  The base directory where the 
# enire system starts is hard coded on the last line for script.. this could
# be fixxed.
#
# populate dirtories and create the psf
#

procedure jmpprepimage (image,fwhm,thresh,maxlin)
	string image {"", prompt="Image to prep"}
	real   fwhm   {4.,prompt="Estimate of the image quality (FWHM)"}
	real   thresh {4,prompt="Threshold for PSF stars."}
	real   maxlin {20000,prompt="Maximum counts for linearity."}
        bool   plant  {yes,prompt="do you plan to plant fake objects?"}
	string filelist   {"proc-these-files",prompt="file to store image names in"}
	bool verbose  {no,prompt="Follow progress"}
	string *poplist

begin
	string timage, fimage, t_logfile
	string ndir, lastchar;
	real t_fwhm,n_fwhm,ffwhm, t_thresh, t_maxlin
	int wc, pos, nstars, trymakepsf
	string object
	bool tplant,fplant
	string parms, tmpdir
	string t_filelist,newfile, tmpname
	struct fline

	t_logfile = logfile ;
	t_fwhm = fwhm ;
	t_thresh = thresh ;
	t_maxlin = maxlin ;
	t_filelist = filelist ;
        tplant = plant ;

	show tmp | scan(tmpdir);
	parms = mktemp(tmpdir//"uparm");
	mkdir(parms);
	set uparm = parms//"/";

	timage = image
# set the number of images placed in the directory at 0
	wc = 0
	trymakepsf=0
	imgets(timage,"title")
	object = imgets.value

	# This is a recovery field if the first letter of the name is R
	if ( (stridx("R",object) == 1) ) {
		tplant=no
	}


	print "IS THIS THE RIGHT FILE?"
	### TURNED BACK ON FOR CFHT NOV 2002 

	#### WARNING WARNING WARNING
	#### PLANTING AND PSFBUILDING TURNED OFF!
	##tplant=no

	jmpmakepsf.base="./"

	iferr  { jmpmakepsf(timage,"psf",fwhm=t_fwhm,thresh=t_thresh,maxlin=t_maxlin,apmin=3,apmax=15)  }
	    trymakepsf=1 


	 if ( access(timage//".jmpmakepsf.OK") ) {
	    kdelete (timage//".jmpmakepsf.FAILED")
	    kdelete (timage//".jmpmakepsf.OK")
	    trymakepsf=0
	 } else {
	    trymakepsf=1
  	 };;

	print ( trymakepsf)

        if ( trymakepsf == 1 ) {
	    print "Failing over to makepsf from jmpmakepsf"
	    makepsf(timage,"psf",fwhm=t_fwhm,thresh=t_thresh,maxlin=50000,apmin=3,apmax=15)
            if ( access(timage//".makepsf.OK") ) {
	        kdelete (timage//".makepsf.FAILED")
	        rename(timage//".jmpmakepsf.FAILED",timage//".jmpmakepsf.ERROR")
	        kdelete (timage//".makepsf.OK")
	    } else {
	        print("Makepsf Failed")
            } ;;
	}


	t_fwhm = datapars.fwhm

	# create/addto list of images available in this directory
	if (!access(t_filelist) ) {
	   printf("# Files to be planted and searched\n",>t_filelist);
	   printf("#%18s %5s %s\n","image","fwhm","plant",>>t_filelist);
	   printf("%19s %5.1f %b\n",timage,t_fwhm,tplant,>>t_filelist)
	} else {
	# Lock other's off this file until I'm done
	  while ( access("prep_image_lock") ) sleep(1)
	  touch "prep_image_lock";
	  newfile=mktemp("proc");
	  poplist=t_filelist;
	  while(fscan(poplist,fline)!=EOF) {
	    if ( substr(fline,1,1) == "#" ) {
	      if ( access(newfile) ) {
	        print(fline,>> newfile);
	      }else{
	        print(fline,> newfile);
	      }
	    } else {

	      # a previous version didn't have this "plant" 
	      # setting in the processing file.  This ensures fplant
	      # has some value.
	      fplant=tplant;

	      print(fline) | scan(fimage,ffwhm,fplant);
	      if ( fimage == timage ) {
	        # Replace this file entry with the current set
		# and then tack on the rest of the file
	        printf("%19s %5.1f %b\n",timage,t_fwhm,tplant,>>newfile);
		break;
	      } else if ( fimage > timage ) {
		# stick in the current set, then the one just
		# read then tack on the rest of the file
	        printf("%19s %5.1f %b\n",timage,t_fwhm,tplant,>>newfile);
	        printf("%19s %5.1f %b\n",fimage,ffwhm,fplant,>>newfile);
		break;
 	      } else {
		# pass this line into the output file
	        printf("%19s %5.1f %b\n",fimage,ffwhm,fplant,>>newfile);
	      }
	    }
	  }
	  # cat on the rest of the original file.
	  while(fscan(poplist,fline)!=EOF) {
	    print(fline,>>newfile);
	  }
	  if ( fimage < timage ) {
	    #since fimage was the last line read, this implies that
	    # timage never got sent to newfile.  Append timage image 
	    printf("%19s %5.1f %b\n",timage,t_fwhm,tplant,>>newfile)
	  }
	  kdelete(t_filelist);
	  rename(newfile,t_filelist);
	  kdelete("prep_image_lock");
	}

	print "Doing some cleanup"
	delete(parms//"/*",verify-);
	rmdir(parms);
	set uparm = "./";
	

end


