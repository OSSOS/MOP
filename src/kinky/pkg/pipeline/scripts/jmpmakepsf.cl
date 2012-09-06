#
# jmpmakepsf produces a PSF from an input image, with seeing estimate and
#	     and aperture correction
#
# Last modified: Feb 7, 2003. BG.

procedure jmpmakepsf (image,psf,fwhm,thresh,maxlin) ;
	string	image {prompt="Image to produce PSF from."}
	string  psf   {prompt="PSF output file EXTENSION."}
	real    fwhm  {4.,prompt="Guess the seeing."}
	real    thresh {4,prompt="Threshold for PSF stars."}
	real	maxlin {20000,prompt="Maximum counts for linearity."}
	string  coofile   {"default",prompt="output coordinate file."}
	int	apmin {3,prompt="Small aperture. (not used)"}
	int	apmax {15,prompt="Large aperture. (not used)"}
	string  base  {"./",prompt="Working directory (include trailing /)."}
	int     dimen {1000,prompt="Size of the working image to use."}
	real    zeropt {26.0,prompt="Photometric zeropoint"}

	string  *aplist

begin 	
  
# 	task parameters 
	string t_image 
	string t_coofile
	string t_psf 
	string t_base  
	string tmag
	int    t_dimen, failedflag, npsfstar
	real    t_fwhm, xsig, ysig, sig, ssig, nsamp, dum
	int    t_apmin, t_apmax, wc, ac
	real   t_zeropoint
	string par1,par2
#   static fields
	string DF,origin
    
#	procedure variables
	string 	working, groupfile, root, buff, newfile
	int   x_size,nstars  
	int   y_size,i
	int	x1,x2,y1,y2 , naps, npar
	real sky, apcor, aperr, n_fwhm, t_thre,t_maxlin
  
# Flow monitoring control
	touch (image//".jmpmakepsf.FAILED")
	print("invoked with : "//image//" , "//psf, >> image//".jmpmakepsf.FAILED")
	failedflag = 0


	t_image = image  
	t_psf = psf  
	t_coofile=coofile
	t_fwhm = fwhm
	t_apmin = apmin
	t_apmax = apmax
	t_base  = base  
	t_dimen = dimen 
	t_zeropoint = zeropt
	t_thre = thresh
	t_maxlin = maxlin
# in the interest of key strokes.
	DF = "default"
	if (substr(t_base,strlen(t_base),strlen(t_base)) != "/" )
	    t_base = t_base//"/" 
	t_image = t_base//t_image
	t_psf = t_image//"."//t_psf

# In case of failure, record paramaters
	print("FWHM  : ", t_fwhm, >> image//".jmpmakepsf.FAILED")
	print("apmin : ", t_apmin, >> image//".jmpmakepsf.FAILED")
	print("apmax : ", t_apmax, >> image//".jmpmakepsf.FAILED")
	print("thresh: ", t_thre, >> image//".jmpmakepsf.FAILED")
	print("maxlin: ", t_maxlin, >> image//".jmpmakepsf.FAILED")
	print("base  : ", t_base, >> image//".jmpmakepsf.FAILED")
	print("ZP    : ", t_zeropoint, >> image//".jmpmakepsf.FAILED")

	kdelete(t_image//".zeropoint.used")
	print(t_zeropoint,> t_image//".zeropoint.used")



# 	important to cache the variables so nothing gets messed up 
	cache("daophot")
	cache("phot")  
	cache("datapars")  
	cache("findpars")
	cache("apcor")  
	cache("photpars")  
	cache("daopars")	
    	cache("psf")	
	
#   this is a stealth script... go and do good... do not report progress
	
	daophot.verify=no
	daophot.verbose=no
	daophot.update=no

	psf.showplots=no
	psf.interactive=no
#
## set the basic 'datapars' for the images.
#
	datapars.exposure=""
        datapars.itime=1
	datapars.gain="GAIN"
	datapars.airmass="AIRMASS"
	datapars.obstime=""
	datapars.ccdread=""
	datapars.readnoise=7
	datapars.datamax=t_maxlin
	datapars.datamin=-1000
	datapars.fwhmpsf=t_fwhm


## Build the image header and a list of stars to use for PSF

	stepZjmp("-f",t_image)	
	if ( access("stepZjmp.OK") ) {
	    # stepZ was succesful
	    kdelete ("stepZjmp.FAILED")
	    kdelete ("stepZjmp.OK")
	} else {
	   failedflag = 1
	   print ("stepZjmp FAILED")
	   print ("stepZjmp FAILED", >> image//".jmpmakepsf.FAILED")
	   goto finalproc
	}

        step0jmp("-f",t_image,"-w",t_fwhm,"-t",t_thre,"-m",t_maxlin)
	if ( access("step0jmp.OK") ) {
	   kdelete ("step0jmp.FAILED")
	   kdelete ("step0jmp.OK")
	} else {   
	   print ("step0jmp FAILED", >> image//".jmpmakepsf.FAILED")
	   failedflag=1
	   goto finalproc
	}

#
# Reject stars which have bad pixels in the fitting radius.
# This is done by computing the aperture photometry on those stars
# 
	photpars.apertures = 5*t_fwhm
	photpars.zmag = t_zeropoint
	fitskypars.dannulus = t_fwhm
	fitskypars.annulus =  5*t_fwhm + 2

 	kdelete(t_image//".mag")
	phot(t_image,t_image//".bright.psf",t_image//".mag",centerpars.calgori="centroid")

# reject stars with any error in the calculation of their photometric magnitude
	kdelete(t_image//".coo.1")
	txdump (t_image//".mag","XCEN,YCEN","(SIER==0)&&(CIER==0)&&(PIER==0)", > t_image//".coo.1");
	
# Determine the sky threshold:
#      Used reject stars with larger sky background than average and with any error
	txdump(t_image//".mag","MSKY","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)") | average | scan(sig,dum,nsamp);
	txdump(t_image//".mag","STDEV","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)") | average | scan(ssig,dum,nsamp);

	print ("SKY: "//sig//" STDEV: "//ssig//" NPOINT: "//nsamp);

	datapars.datamin = (sig-5.0*ssig);

	## Now we build the PSF, twice.... correcting the FWHM as we go.
	for (i=1; i<3; i+=1) {
	    #  We should use a minimum aperture that is about 1.1 -1.2 the FWHM
	    #  This is set at the start of the psf loop so the PSF small aperture
	    #  matches that used in mkapfile after the PSF is built.
	    #  We do this twice since the first time we build the PSF with the input FWHM
	    #  and the second time using the FWHM determined from the PSF stars
	    t_apmin = int(1.2*t_fwhm +0.5)
	    # using an aperture smaller than 3 pixels cuases undersampling issues.
	    if (t_apmin < 3)  {
		t_apmin=3
	    }
	    # Adjust the large aperture size too, and the sky fitting parameters
	    t_apmax = int(5*t_fwhm+0.5)
	    fitskypars.dannulus = t_fwhm
	    fitskypars.annulus =  5*t_fwhm + 2
	    # set the PSF radius and fitting radius
	    daopars.fitrad = int(0.9*t_fwhm + 0.5)
	    daopars.psfrad = 5.0*t_fwhm
	    datapars.fwhm = t_fwhm
	    print "Using fwhm of --> "//t_fwhm
	    ## using the good stars found outside the loop, reduce the aperture to the 
	    ## small aperture and do the photometry to get PSF onto scale of small ap photometry

	    photpars.apertures=t_apmin
	    kdelete(t_image//".mag")
	    phot(t_image,t_image//".coo.1",t_image//".mag",centerpars.calgori="centroid")

	    ## select out those that have bad values of centroid and photometry issues.
	    ## the first step removes those stars that have INDEF values for MSKY since those 
	    ## stars cause math errors during the second step
	    tmag=mktemp(t_image)
	    pselect (t_image//".mag",tmag,"(CIER==0)&&(PIER==0)&&(SIER==0)&&(MSKY>0 && MSKY < 1e5 && MSKY!=INDEF )")
	    ## now the 'INDEF' values of MSKY are gone so remove stars with large MSKY deviations
	    kdelete(t_image//".phot")
	    pselect(tmag,t_image//".phot",\
		    "CIER == 0 && (abs(MSKY - "//sig//") < 3.0*"//ssig//")")
	    kdelete(tmag)
	

	    ## time to select upto 25 PSF stars, from the good stars we have left.
	    kdelete(t_image//".pst.1")
	    pstselect(t_image,t_image//".phot",t_image//".pst.1",25)

	    # use those stars to build an actual PSF
	    kdelete ( t_image//".pst.2")
	    kdelete ( t_image//".psg.1")

	    if ( imaccess(t_psf) ) {
	       imdelete(t_psf,verify-)
	    }

	    psf(t_image,t_image//".phot",t_image//".pst.1",
		    t_psf,
		    t_image//".pst.2",
		    t_image//".psg.1",showplots-,interactive-)
	
            # Estimate the FWHM using the gaussian fit to the PSF
	    hselect(t_psf,"PAR1",yes) | scan(xsig)
	    hselect(t_psf,"PAR2",yes) | scan(ysig)
	    hselect(t_psf,"NPSFSTAR",yes) | scan(npsfstar)
	    t_fwhm = sqrt(xsig*xsig + ysig*ysig)
	    if ( abs((xsig - ysig)/t_fwhm) > 0.2 ) {
		#JJK Added a warning message here. March 2002
		touch (t_image//".psf.WARNING")
		print("ERROR: The PSF is too elliptical for planting. image"//t_image, >> t_image//".psf.WARNING" )
	    }
	    
	    ## we must have 10 or more stars to get a good psf...
	    if ( npsfstar < 9 ) {
	        print("Too few psf stars")
	        failedflag=1
	        goto finalproc
	    }


	    t_fwhm = 1.05*t_fwhm*sqrt(8*log(2))/2.0
	    print "Got FWHM of "//t_fwhm
	    if ( t_fwhm > 8 ) { 
		touch (t_image//".psf.WARNING")
		print ("WARNING: Seeing value large? Measured "//t_fwhm, 
                       >> t_image//".psf.WARNING" ) 
	    }
	    ## Even though we recomputed the fwhm here, we don't reset t_apmin or t_apmax again.
	    ## nominally convergence occurs in just 2 loops.   We could check for convergence
	    ## ie: change in t_fwhm between loops is less than x% but we don't
	}

	## Check that the output fwhm is reasonablely close to the input one.
	list = t_image//".fwhm"
	npar = fscan(list, n_fwhm)
	if ( (n_fwhm > t_fwhm*3.0) || (t_fwhm > n_fwhm*3.0) ) {
	   print("PSF change too  large")
	   failedflag = 1
	   goto finalproc
	}


	kdelete (t_image//".nst.1");
	kdelete (t_image//".nrj.1");
	  
	nstar(t_image,t_image//".psg.1",
		 psfimage=t_image//".psf.fits",
		 nstarfile=t_image//".nst.1",
		 rejfile=t_image//".nrj.1");

        kdelete(t_image//".phot")
	pselect(t_image//".nst.1",t_image//".phot","CHI < 2.5")

	#build a file for use as input into mkapfile
	kdelete(t_image//".coo.2")
	txdump(t_image//".nst.1","XCENTER,YCENTER,MAG,SHARPNESS,ID","CHI < 2.5", \
	       headers+, > t_image//".coo.2")

        photpars.apertures=t_apmin//":"//t_apmax//":1"

	naps = t_apmax - t_apmin +1
	if (naps < 3) {
	    print("inner ("//t_apmin//") and outer ("//t_apmax//") aperatures must differ by more than 2 pixels\n")
	    failedflag=1
	    goto finalproc
	}

	kdelete(t_image//".mag.2")
	
	phot(t_image,t_image//".coo.2",t_image//".mag.2")
	
	npar = 3

	kdelete(t_image//".mkap")
        failedflag=1

	iferr { mkapfile(t_image//".mag.2",naps,t_image//".mkap",interactive-,verify-,nparams=npar) }
	     goto finalproc

        failedflag=0


	### Read in the aperture correction file and convert to CFEPS Pipeline format
	list=t_image//".mkap"
	line=t_apmin//" "//t_apmax//" 30 30 "
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
	printf("%d %d %6.2f %6.2f\n",t_apmin,t_apmax,apcor,aperr,
	                             >> t_image//".apcor" ) 
	   

	## spit out some warnings about apcor, if its a bit off.
	if  ( t_apmin < 2) { 
	  touch ( t_image//".apcor.WARNING" ) 
	  print ("WARNING: t_apmin small: "//t_apmin,>> t_image//".apcor.WARNING")
	}
	if  ( t_apmax > 35) { 
	  touch ( t_image//".apcor.WARNING" ) 
	  print ("WARNING: t_apmax huge: "//t_apmax,>> t_image//".apcor.WARNING")
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
        delete(t_image//".mag*",verify-)
        delete(t_image//".pst*",verify-)
        delete(t_image//".psg*",verify-)
        delete(t_image//".coo*",verify-)
	delete(t_image//".nrj*",verify-)
	delete(t_image//".nst*",verify-)

	#kdelete(t_psf//".fits")
	if ( failedflag == 0 ) {
	   if ( imaccess(t_psf) ) {
	      print("\n"//t_psf//" OK\n")
	   } else {
	      print("\n NO PSF BUILT \n")
	     failedflag = 1
	     pwd | scan(t_base)
	   }
	}
        if (failedflag == 0) {
	   touch (t_image//".jmpmakepsf.OK")
	} else {
	   print("\n jmpmakepsf: FAILED in "//t_base//" for "//t_image//"\n", >> t_image//".jmpmakepsf.FAILED")
	   print "failed to build psf" 
	}
	
end

