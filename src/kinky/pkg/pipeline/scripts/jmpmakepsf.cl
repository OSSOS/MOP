#
# jmpmakepsf produces a PSF from an input image, with seeing estimate and
#	     and aperture correction
#
# Last modified: Feb 7, 2003. BG.

procedure jmpmakepsf (image,psf,fwhm,thresh,maxlin) 

string	image {prompt="Image to produce PSF from."}
string  psf   {prompt="PSF output file EXTENSION."}
real    fwhm  {4.,prompt="Guess the seeing."}
real    thresh {4,prompt="Threshold for PSF stars."}
real	maxlin {20000,prompt="Maximum counts for linearity."}
real	apin {3,prompt="Small aperture as fraction of FWHM"}
real	apout {15,prompt="apout as fraction of FWHM"}
real	swidth {15,prompt="swidth as fraction of FWHM"}
string  base  {"./",prompt="Working directory (include trailing /)."}
int     order  {3, prompt="Order of the psf. 3 is vary in x and y"}
real    zeropt {26.0,prompt="Photometric zeropoint"}
bool    keep_files {yes, prompt="Do you want to keep the intermediate files?"}

string  *aplist

begin 	
  
# 	task parameters
	string t_image 
	string t_psf 
	string t_base  
	real t_zeropoint
	real t_fwhm
	real t_apin
	real t_apout
	real t_swidth
 	int t_order
	real t_maxlin
	real t_threshold
	bool t_keep_files
# procedure variables
	int failedflag, npsfstar
	real xsig
	real ysig
	real sig, ssig
	int nsamp
	real dum
	int	wc, ac
	string par1
	string par2
	string temp_mag
	real apmin
	real apmax
        real flux_aperture
	int naps
	int niters
	real apcor, aperr
	real p_fwhm, step0_fwhm
  

	# Set the internal variables from the called variables.
	# this ensures that any prompting happens at the start of the script
	t_image = image
	t_psf = psf
	t_fwhm = fwhm
	t_threshold = thresh
	t_maxlin = maxlin
	t_apin = apin
	t_apout = apout
	t_swidth = swidth
	t_base = base
	t_order = order
	t_zeropoint = zeropt
	t_keep_files = keep_files

	# Flow monitoring control
	touch (t_image//".jmpmakepsf.FAILED")
	print("invoked with : "//t_image//" , "//psf, >> t_image//".jmpmakepsf.FAILED")
	failedflag = 0

	
    	# ensure that the directory for writing has a traling '/'
	if (substr(t_base,strlen(t_base),strlen(t_base)) != "/" )
	    t_base = t_base//"/" 
	t_image = t_base//t_image
	t_psf = t_image//"."//t_psf

	apmin = t_fwhm*t_apin
	apmax = t_fwhm*t_apout
	naps = (apmax - apmin) +1
        apmax = apmin + naps
	naps = (apmax - apmin) +1

    # In case of failure, record paramaters
	print("FWHM  : ", t_fwhm, >> t_image//".jmpmakepsf.FAILED")
	print("apmin : ", apmin, >> t_image//".jmpmakepsf.FAILED")
	print("apmax : ", apmax, >> t_image//".jmpmakepsf.FAILED")
	print("naps  : ", naps, >> t_image//".jmpmakepsf.FAILED")
	print("thresh: ", t_threshold, >> t_image//".jmpmakepsf.FAILED")
	print("maxlin: ", t_maxlin, >> t_image//".jmpmakepsf.FAILED")
	print("base  : ", t_base, >> t_image//".jmpmakepsf.FAILED")
	print("ZP    : ", t_zeropoint, >> t_image//".jmpmakepsf.FAILED")

	kdelete(t_image//".zeropoint.used")
	print(t_zeropoint,> t_image//".zeropoint.used")
 
	# cache the variables so local usage of IRAF doesn't impact running script
	cache("daophot")
	cache("phot")  
	cache("datapars")  
	cache("findpars")
	cache("apcor")  
	cache("photpars")  
	cache("daopars")	
        cache("psf")	
	
	# Turn off verbosity and parameter checking in DAOPHOT
	daophot.verify=no
	daophot.verbose=no
	daophot.update=no
	psf.showplots=no
	psf.interactive=no

	# set the basic 'datapars' for the images.
	datapars.exposure=""
	# Note the itime is set to 1 as the SGwyn ZP includes the exposure time.
	datapars.itime=1
	datapars.gain="GAIN"
	datapars.airmass="AIRMASS"
	datapars.obstime=""
	datapars.ccdread=""
	datapars.readnoise=7
	datapars.datamax=t_maxlin
	datapars.datamin=-1000
	datapars.fwhmpsf=t_fwhm

	# Setup mopheader for 'OSSOS' pipeline. 
	stepZjmp("-f",t_image)	
	if ( access("stepZjmp.OK") ) {
	    # stepZ was succesful
	    kdelete ("stepZjmp.FAILED")
	    kdelete ("stepZjmp.OK")
	} else {
	   failedflag = 1
	   print ("stepZjmp FAILED")
	   print ("stepZjmp FAILED", >> t_image//".jmpmakepsf.FAILED")
	   goto finalproc
	}

	# find a set of bright stars for making a psf with.
	step0jmp("-f",t_image,"-w",t_fwhm,"-t",t_threshold,"-m",t_maxlin)
	if ( access("step0jmp.OK") ) {
	   kdelete ("step0jmp.FAILED")
	   kdelete ("step0jmp.OK")
	} else {   
	   print ("step0jmp FAILED", >> t_image//".jmpmakepsf.FAILED")
	   failedflag=1
	   goto finalproc
	}


	# Reject stars which have bad pixels in the fitting radius.
	# This is done by computing the aperture photometry on those stars
	photpars.apertures = apmin
	photpars.zmag = t_zeropoint
	fitskypars.dannulus = t_swidth*t_fwhm
	fitskypars.annulus =  apmax + 2

 	kdelete(t_image//".mag")
        flux_aperture = 18.0
	phot(t_image, 
	     t_image//".bright.psf",
	     t_image//".bright.mag",
	     photpars.apertures=flux_aperture,
   	     centerpars.calgori="centroid",
             centerpars.maxshift=3)
        print (apmin)

	# reject stars with any error in the calculation 
	# of their photometric magnitude
	kdelete(t_image//".coo.1")
	txdump (t_image//".bright.mag",
			"XCEN,YCEN",
			"(SIER==0)&&(CIER==0)&&(PIER==0)", > t_image//".coo.1");
	
	# Determine the sky threshold:
	# Used reject stars with larger sky background than average and with any error
	txdump(t_image//".bright.mag","MSKY","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)") | average | scan(sig,dum,nsamp);
	txdump(t_image//".bright.mag","STDEV","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)") | average | scan(ssig,dum,nsamp);

	print ("SKY: "//sig//" STDEV: "//ssig//" NPOINT: "//nsamp);

	datapars.datamin = (sig-5.0*ssig);

	## Now we build the PSF, twice.... correcting the FWHM as we go.
	p_fwhm = -1
	niters = 0
	while ( ( p_fwhm < 0 || abs(p_fwhm - t_fwhm)/t_fwhm > 0.01 ) && niters < 5 ) {
	    niters = niters + 1
	    p_fwhm = t_fwhm

	    # We should use a minimum aperture that is about 1.1 -1.2 the FWHM
	    # This is set at the start of the psf loop so the PSF small aperture
	    # matches that used in mkapfile after the PSF is built.
	    # We do this twice since the first time we build 
	    # the PSF with the input FWHM
	    # and the second time using the FWHM determined from the PSF stars

	    apmin = int(10*t_apin*t_fwhm + 0.5) / 10.0
            apmax = int(10*t_apout*t_fwhm + 0.5) / 10.0	
	    naps = (apmax - apmin) + 1
            apmax = apmin + naps 
	    naps = (apmax - apmin) + 1
  	    print("apmin : ", apmin)
	    print("apmax : ", apmax)
	    print("naps  : ", naps)

	    # using an aperture smaller than 1.5 pixels under samples
	    if ( apmin < 1.5 )  {
		apmin = 1.5
	    }

	    # Adjust the large aperture size too, and the sky fitting parameters
	    fitskypars.dannulus = t_swidth*t_fwhm
	    fitskypars.annulus =  apmax + 2

	    # set the PSF radius and fitting radius
	    daopars.fitrad = apmin
	    daopars.psfrad = apmax
	    datapars.fwhm = t_fwhm
	    print "Using fwhm of --> "//t_fwhm

	    ## using the good stars found outside the loop, reduce the aperture to the 
	    ## small aperture and do the photometry to get PSF onto scale of small ap photometry
	    photpars.apertures = apmin
	    kdelete(t_image//".mag")
	    phot(t_image,t_image//".coo.1",t_image//".mag",centerpars.calgori="centroid",photpars.apertures=flux_aperture)
            print (apmin)

	    ## select out those that have bad values of centroid and photometry issues.
	    ## the first step removes those stars that have INDEF values for MSKY since those 
	    ## stars cause math errors during the second step
	    temp_mag=mktemp(t_image)
	    pselect(t_image//".mag",temp_mag,"(CIER==0)&&(PIER==0)&&(SIER==0)&&(MSKY>0 && MSKY < 1e5 && MSKY!=INDEF )")

	    ## now the 'INDEF' values of MSKY are gone so remove stars with large MSKY deviations
	    kdelete(t_image//".mag")
	    pselect(temp_mag,t_image//".mag",\
		    "CIER == 0 && (abs(MSKY - "//sig//") < 3.0*"//ssig//")")
	    kdelete(temp_mag)

	    ## time to select upto 25 PSF stars, from the good stars we have left.
	    print "Selecting upto 25 PSF stars from "//t_image//".mag"
	    kdelete(t_image//".pst.1")
	    pstselect(t_image,t_image//".mag",t_image//".pst.1",25)

	    # use those stars to build an actual PSF
	    kdelete ( t_image//".pst.2")
	    kdelete ( t_image//".psg.1")

	    if ( imaccess(t_psf) ) {
	       imdelete(t_psf,verify-)
	    }

	    psf(t_image,t_image//".mag",t_image//".pst.1",
		    t_psf,
		    t_image//".pst.2",
		    t_image//".psg.1",showplots-,interactive-)
	
            # Estimate the FWHM using the gaussian fit to the PSF
	    hselect(t_psf,"PAR1",yes) | scan(xsig)
	    hselect(t_psf,"PAR2",yes) | scan(ysig)
	    hselect(t_psf,"NPSFSTAR",yes) | scan(npsfstar)
	    t_fwhm = sqrt((xsig*xsig + ysig*ysig)/2.0)
            print "Sigma -> "//t_fwhm
	    if ( abs((xsig - ysig)/t_fwhm) > 0.2 ) {
		#JJK Added a warning message here. March 2002
		touch (t_image//".psf.WARNING")
		print("ERROR: The PSF is too elliptical for planting. image"//t_image, >> t_image//".psf.WARNING" )
	    }
	    
	    ## we must have 10 or more stars to get a good psf...
	    if ( npsfstar < 9 ) {
	        print("Only "//npsfstar//" where used to build the image, terminating")
	        failedflag=1
	        goto finalproc
	    }

            # for MEGACAM the FWHM is about 84% of the
            # value you would get based on a guassian fit to the PSF
	    t_fwhm = 0.84*t_fwhm*sqrt(8*log(2))
	    print "Got FWHM of "//t_fwhm
	    if ( t_fwhm > 8 ) { 
		touch (t_image//".psf.WARNING")
		print ("WARNING: Seeing value large? Measured "//t_fwhm, 
                       >> t_image//".psf.WARNING" ) 
	    }
	    # keep going until change in t_fwhm between
	    # loops is less than 0.05 (5%)
	}

	## Check that the output fwhm is reasonably close to the input one.
	# and overwrite the input value with the computed one
	list = t_image//".fwhm"
	dum = fscan(list, p_fwhm)
	if ( (p_fwhm > t_fwhm*3.0) || (t_fwhm > p_fwhm*3.0) ) {
	   print("PSF change too  large")
	   failedflag = 1
	   goto finalproc
	}
	kdelete (t_image//".fwhm");
	print (t_fwhm, >> t_image//".fwhm")


	# compute the apcor using the PSF stars.
	kdelete (t_image//".nst.1");
	kdelete (t_image//".nrj.1");	  
	nstar(t_image,t_image//".psg.1",
		 psfimage=t_image//".psf.fits",
		 nstarfile=t_image//".nst.1",
		 rejfile=t_image//".nrj.1");

	kdelete(t_image//".phot")
	pselect(t_image//".nst.1",t_image//".phot","CHI < 2.5")

	#build a file for use as input into phot
	kdelete(t_image//".coo.2")
	txdump(t_image//".nst.1","XCENTER,YCENTER,MAG,SHARPNESS,ID","CHI < 2.5", \
	       headers+, > t_image//".coo.2")

	# Run PHOT with a range of apertures.
	kdelete(t_image//".mag.2")
    	photpars.apertures=apmin//":"//apmax+0.1//":1"
	naps = (apmax - apmin) +1
	print("apmin : ", apmin)
	print("apmax : ", apmax)
	print("naps  : ", naps)
	if (naps < 3) {
	    print("inner ("//apmin//") and outer ("//apmax//") aperatures must differ by more than 2 pixels\n")
	    failedflag=1
	    goto finalproc
	}	
	phot(t_image,t_image//".coo.2",t_image//".mag.2", photpars.apertures=apmin//":"//apmax+0.1//":1")
	

	# fit that range of apertures to a curve of growth.
	kdelete(t_image//".mkap")

	failedflag=1
	iferr { mkapfile(t_image//".mag.2",naps,t_image//".mkap",interactive-,verify-,nparams=t_order) }
	    goto finalproc

	failedflag=0

	### Read in the aperture correction file and convert to CFEPS Pipeline format
	list=t_image//".mkap"
	line=apmin//" "//apmax//" 30 30 "
	# the last line has the actual correction
	
	ac=0
	while( fscan(list,line) != EOF ) 
	    ac+=1
	
   	if ( ac != 3 )  {
	    print ("Invalid mkapfile output? (check "//t_image//".mkap)")
	    failedflag=1
	    goto finalproc
	} 
	    

	# Stick the apcor line in the way the pipeline wants it
	apcor=-30
	aperr=30
	wc=fscan(line,s1,apcor,aperr)
	kdelete(t_image//".apcor")
	apcor=-1*apcor
	printf("%.1f %.1f %6.2f %6.2f\n", apmin, apmax, apcor, aperr,
	                             >> t_image//".apcor" ) 
	   

	## spit out some warnings about apcor, if its a bit off.
	if  ( apmin < 2) { 
	  touch ( t_image//".apcor.WARNING" ) 
	  print ("WARNING: apmin small: "//apmin,>> t_image//".apcor.WARNING")
	}
	if  ( apmax > 35) { 
	  touch ( t_image//".apcor.WARNING" ) 
	  print ("WARNING: apmax huge: "//apmax,>> t_image//".apcor.WARNING")
	}
	if  ( apcor > 1.0 ) { 
	      touch ( t_image//".apcor.WARNING" ) 
	      print ("ERROR: apcor large ", >> t_image//".apcor.WARNING")
	}
	if  ( aperr > 0.2 ) { 
	      touch ( t_image//".apcor.WARNING" ) 
	      print ("ERROR: aperr large ", >> t_image//".apcor.WARNING")
	}


finalproc:

	if ( t_keep_files )  {
	    print("Keeping intermediate files")
        } else {
	    delete(t_image//".bright.mag", verify-)
	    delete(t_image//".mag*", verify-)
	    delete(t_image//".pst*", verify-)
	    delete(t_image//".psg*", verify-)
	    delete(t_image//".coo*", verify-)
	    delete(t_image//".nrj*", verify-)
	    delete(t_image//".nst*", verify-)
	}

	if ( failedflag == 0 ) {
		if ( imaccess(t_psf) ) {
			print("\n"//t_psf//" OK\n")
			touch (t_image//".jmpmakepsf.OK")
		} else {
			print("\n NO PSF BUILT \n")
			failedflag = 1
			pwd | scan(t_base)
	   }
	}
	if (failedflag != 0) {
		print("\n jmpmakepsf: FAILED in "//t_base//" for "//t_image//"\n", >> t_image//".jmpmakepsf.FAILED")
		print "failed to build psf" 
	}

end

