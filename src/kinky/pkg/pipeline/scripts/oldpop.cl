# JJK July 2001.
#
# This scripts contains a BIG kluge.  The base directory where the 
# enire system starts is hard coded on the last line for script.. this could
# be fixxed.
#
# populate dirtories and create the psf
#

procedure populate (image,fwhm) 
	string image {"",prompt="Name of the image to move"}
	real   fwhm   {"",prompt="Estimate of the image quality (FWHM)"}
	string destdir   {"",prompt="Top level directory for images"}
	string datadir   {"",prompt="Directory containing input data"}
	string filelist   {"proc-these-files",prompt="file to store image names in"}
	string dirpipe  {"directory",prompt="named pipe to communicate progress"}
	bool verbose  {no,prompt="Follow progress"}
	string *poplist

begin
	string timage, fimage
	string ndir, lastchar, t_data, t_dest;
	real t_fwhm,n_fwhm,ffwhm
	int wc, pos, nstars
	string object
	bool plant,fplant
	string parms
	string t_filelist,newfile

	timage = image
	t_fwhm = fwhm
	t_data = datadir
	t_dest = destdir
	t_filelist = filelist

	parms = mktemp("tmp$uparm")
	mkdir(parms);
	set uparm = parms;

# the destination directory should have trailing slash...
  	if (substr(t_dest,strlen(t_dest),strlen(t_dest)) != "/" ) 
		t_dest = t_dest//"/" ;
	if (substr(t_data,strlen(t_data),strlen(t_data)) != "/" )
		t_data = t_data//"/";

# put the image into an appropriate directory under the path destdir
	movesingle(timage,obsdir=t_data,reddir=t_dest,rn-,flipem+,overwrite+,shorten+,verbose=verbose) 

# movesingle reports the name of the the subdir it used
# via the return parameter movesingle.dest
	ndir = movesingle.dest

# set the number of images placed in the directory at 0
	wc = 0

	imgets(ndir//timage,"title")
	object = imgets.value

	# This is a recovery field if the first letter of the name is R
	if ( stridx("R",object) != 1 ) {
	   if (verbose) print("Creating PSF image\n");
	   makepsf.base=movesingle.dest   
	   makepsf(timage,"psf",fwhm=t_fwhm,apmin=3,apmax=15)
	   t_fwhm = makepsf.fwhm
	   plant = yes ;
	} else {
	   if (verbose) print("Setting up for RECOVERY only\n");
	   plant = no ;
	   n_fwhm = -1;
	   nstars = -1;
	   step0matt("-f",t_dest//timage,"-t",20,"-w",t_fwhm,"-m",30000)| scan(n_fwhm,nstars)
	   if ( n_fwhm > 0 && nstars > 10 ) {
		t_fwhm = n_fwhm;
	   } else {
	      step0matt("-f",t_dest//timage,"-t",10,"-w",t_fwhm,"-m",30000)| scan(n_fwhm,nstars)
	      if ( nstars < 10 || n_fwhm < 0 ) { 
		if (verbose) print("step0matt failed, using default fwhm");
	      } else {
		t_fwhm = n_fwhm;
	      }
	   }
	}
	# create/addto list of images available in this directory
	t_filelist = ndir//t_filelist
	if (!access(t_filelist) ) {
	   printf("# Files to be planted and searched\n",>t_filelist);
	   printf("#%18s %5s %s\n","image","fwhm","plant",>>t_filelist);
	   printf("%19s %5.1f %b\n",timage,t_fwhm,plant,>>t_filelist)
	} else {
	  printf("%19s %5.1f %b\n",timage,t_fwhm,plant,>>t_filelist)
	  newfile=mktemp("proc");
	  sort(t_filelist,column=1,>newfile);
	  delete(t_filelist,verify-);
	  rename(newfile,t_filelist);
	}


	# if there are three images in the proc-file then we 
	# can plant and search.	   
	count (t_filelist) | scan(wc)
	if ( wc == 5 ) {
	# ndir is the directory created to store the image
	  dirpipe = t_data//dirpipe
  	  print(ndir,>> dirpipe)
	}
	
	delete(parms//"/*",verify-);
	

end


