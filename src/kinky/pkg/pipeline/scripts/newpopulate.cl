# JJK July 2001.
# Rev. Nov 2001 (removed kluge with directory name)


procedure newpopulate (image,fwhm) 
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
	struct fline

	timage = image
	t_fwhm = fwhm
	t_data = datadir
	t_dest = destdir
	t_filelist = filelist

# put the image into an appropriate directory under the path destdir
	movem(timage,obsdir=t_data,reddir=t_dest,rn-,flipem+,overwrite-,shorten+,verbose=verbose) 

	pipescan(timage,t_fwhm,workdir=movesingle.dest,verbose=verbose)
	

end


