##
#
#  An entirely new PLANT script.  It was about time.
#
#


procedure pipeplant (filelist,outfile)
	string filelist {"",    prompt="images to plant objects in"}
	string psf      {"psf",    prompt="psf file for adding objects"}
	string shifts   {"shifts",    prompt="alignment shifts"}
	string outfile  {"", prompt="object list"}
	string mjdate   {"MJD-OBS", prompt="julian date keyword"}
	string exptime  {"EXPTIME", prompt="Exposure time header keyword"}
	string prefix   {"fk", prompt="planeted image prefix"}
	bool   verbose  {no, prompt="report progress"}
	string *pipeimages
	string *pipeobjects
	string *pipeshifts

begin 
## Matching the parameter variables.
	string t_filelist, t_mjd, t_exptime, t_outfile
	string t_psf, t_shifts, t_prefix 

## Internal varibles.

	string infile,addstar,fkimage ;
	int    naxis1, naxis2;
	string sx ;
	real start_time,current_time;
	real x,y,rate,angle,mag,arcrate;
	real dx,dxx,dxy,dy,dyx,dyy,dmag,emag;
	int  id, count, nsteps ;
	int  nmag ;
	real npix,fwhm;
	real dtime,dt,itime;
	bool plant

## Load the parameters in the local space
	t_filelist = filelist;
	t_psf = "."//psf;
	
	t_shifts=shifts;
	t_outfile = outfile;
	t_prefix = prefix;
	t_mjd = mjdate;
	t_exptime = exptime;


## Expected format of the input file is 
## filename fwhm plant?
	pipeimages = t_filelist;
	plant=yes
	while ( fscan(pipeimages,infile,fwhm,plant) != EOF ) {
	    if ( stridx("#",substr(infile,1,1)) == 0 ) 
		break;
	}
	if (!plant) { 
	  if (verbose) print ("planting turned off here");
	  bye;
	}

## Determine the size of area to plant into
	imgets(infile, "i_naxis1") ;
	naxis1 = int(imgets.value);
	imgets(infile, "i_naxis2") ;
	naxis2 = int(imgets.value);

## Get the start time for planting sequence
	imgets(infile, t_mjd);
	start_time = real(imgets.value);


#
#----- Plant the fake objects into the frames
	pipeimages = t_filelist;
	pipeshifts = t_shifts;
	while ( fscan(pipeimages,infile) != EOF ) {
	  if ( stridx("#",substr(infile,1,1)) != 0 ) 
	    next;

### Load the linear transformation + dmag
	  sx = "#";
	  while ( fscan(pipeshifts,sx,dxx,dxy,dy,dyx,dyy,dmag,emag,nmag)!=EOF ) {
	    if ( stridx("#",substr(sx,1,1)) == 0 )
		break;
	  }
	  if ( nmag < 4 ) { 
	  	!touch pipeplant.WARNING
	  	print (infile," mag shift based on just ",nmag," star(s)",>>'pipeplant.WARNING')
	  }
	  if ( emag > 0.05 ) {
	  	!touch pipeplant.WARNING
		print (infile," mag shift may be bad? error in mag shift= ",emag, >> 'pipeplant.WARNING')
	  }

	  if ( stridx("#",substr(sx,1,1)) != 0) {
	    error(2,"Too few shifts and images in input file ?\n");
	  }else{
	    dx = real(sx);
	  }

	  addstar = mktemp("addstar");
	  print("# xpos ypos mag id",>addstar);
# Read in the targets (one at a time) and create and addstar file
# with the shifted positions
	  count=0;
	  pipeobjects = t_outfile;
	  while(fscan(pipeobjects,sx,y,mag,rate,angle,arcrate,id) != EOF) {
	    if (stridx("#",substr(sx,1,1))!=0)
	       next;
	    x = real(sx);


# Apply the linear transformation
	    x =  dx + dxx*x + dxy*y;
	    y =  dy + dyx*x + dyy*y;
	    mag = mag + dmag;

# Determine the amount of shift since the refence time 
	    imgets(infile,t_mjd);
	    current_time = real(imgets.value);
	    x = x + rate*24.0*(current_time-start_time)*cos(angle/57.28)
	    y = y + rate*24.0*(current_time-start_time)*sin(angle/57.28)
	    
	    imgets(infile,exptime);
	    itime = real(imgets.value);
	    itime = itime/3600.0
	    npix = rate*itime;
	
	    nsteps = int(npix+1);
	    dtime  = itime/nsteps;
	    mag = mag + 2.5*log10(nsteps)

	    for (dt=0; dt<nsteps; dt+=1) {
	        count=count+1;
		x = x + dtime*rate*cos(angle/57.28);
		y = y + dtime*rate*sin(angle/57.28);
	        if ( x > 1 && x < naxis1 && y > 1 && y < naxis2) 
	        print(x,y,mag,count,>>addstar);
	    }
	    ;
          }
	  fkimage = t_prefix//infile;
          if ( imaccess(fkimage) ) {
	    imdelete (fkimage, verify-);
	    delete (fkimage//"*",verify-);
	  }
	  if ( !imaccess(infile//t_psf)) {
	    error(2,"There isn't a psf image for this frame?"//infile); 
	  }
          addstar(infile,addstar,infile//t_psf,fkimage,simple+,verify-,verbose-)
	  chpix(fkimage,fkimage,'ushort')
	  chpix(infile,infile,'ushort')
	  
        }
	
	if ( verbose ) {
	  !touch pipeplant.OK
          print(" ")
          print(" Done ")
          print(" ")
	}


end
