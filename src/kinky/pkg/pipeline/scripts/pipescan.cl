#
# JJK July 2001.
#
# This scripts contains a BIG kluge.  The base directory where the 
# enire system starts is hard coded on the last line for script.. this could
# be fixxed.
#
# populate dirtories and create the psf
#

procedure pipescan (image,fwhm) 
	string image {"",prompt="Image to pipeline"}
	real   fwhm   {"",prompt="Estimate of the image quality (FWHM)"}
	string workdir {"",prompt="directory where images are stored"}
	string filelist   {"proc-these-files",prompt="file to store image names in"}
	string dirpipe  {"directory",prompt="named pipe to communicate progress"}
	bool verbose  {no,prompt="Follow progress"}
	string *poplist

begin
	string timage, fimage
	string ndir, lastchar, t_data, t_dest;
	string t_wdir, coofile;
	real t_fwhm,n_fwhm,ffwhm
	int wc, pos, nstars
	string object
	bool plant,fplant
	string parms
	string t_filelist,newfile, t_pipedir
	struct fline

	timage = image
	t_fwhm = fwhm
	t_wdir = workdir
	if  ( substr(t_wdir,strlen(t_wdir),strlen(t_wdir)) != "/" )
	    t_wdir = t_wdir//"/"
	t_data = t_wdir
	t_dest = t_wdir
	#timage = t_wdir//timage
	t_filelist = filelist
	t_pipedir = dirpipe


# set the number of images placed in the directory at 0
	wc = 0

	imgets(t_wdir//timage,"title")
	object = imgets.value

	# This is a recovery field if the first letter of the name is R
	coofile="default";

	if (verbose) print ("finding PSF stars\n");
	plant = no ;
	n_fwhm = -1;
	nstars = -1;
	step0matt("-f",t_wdir//timage,"-t",10,"-w",t_fwhm,"-m",30000)| scan(n_fwhm)
	coofile = t_wdir//timage//".coo";
	count(coofile) | scan(nstars) ;
	if ( verbose ) print ( "step0matt -> fwhm == ",n_fwhm)
	if ( verbose ) print ( "step0matt -> ",nstars," psfstars in ",coofile)
	if ( n_fwhm > 0 ) t_fwhm = n_fwhm;
	if (nstars<10) coofile = "default";
	plant = no;
	if ( stridx("R",object) != 1 ) {
	#   if (verbose) print("Creating PSF image\n");
	    print("PSF CONSTRUCTION TURNED OFF>>> NO PLANTING \n");
	#   makepsf(timage,"psf",fwhm=t_fwhm,apmin=3,apmax=15,base=t_wdir,coofile=coofile);
	#   plant = yes ;
	    plant = no;
	} 
	# create/addto list of images available in this directory
	t_filelist = t_data//t_filelist
	fimage=timage;
	if (!access(t_filelist) ) {
	   printf("# Files to be planted and searched\n",>t_filelist);
	   printf("#%18s %5s %s\n","image","fwhm","plant",>>t_filelist);
	   printf("%19s %5.1f %b\n",timage,t_fwhm,plant,>>t_filelist)
	} else {
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
	      fplant=plant;
	      print(fline) | scan(fimage,ffwhm,fplant);
	      if ( fimage == timage ) {
	        # Replace this file entry with the current set
		# and then tack on the rest of the file
	        printf("%19s %5.1f %b\n",timage,t_fwhm,plant,>>newfile);
		break;
	      } else if ( fimage > timage ) {
		# stick in the current set, then the one just
		# read then tack on the rest of the file
	        printf("%19s %5.1f %b\n",timage,t_fwhm,plant,>>newfile);
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
	    printf("%19s %5.1f %b\n",timage,t_fwhm,plant,>>newfile)
	  }
	  delete(t_filelist,verify-);
	  rename(newfile,t_filelist);
	}


	# if there are three images in the proc-file then we 
	# can plant and search.	   
	count (t_filelist) | scan(wc)
	if ( wc == 5 ) {
	# ndir is the directory created to store the image
  	  print(workdir,>> t_pipedir)
	}
	
	

end


