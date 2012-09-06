#
# makepsf produces a PSF from an input image
#
#



procedure makepsf (image,psf) ;
	string	image {prompt="Image to produce PSF from."}
	string  psf   {prompt="PSF output file EXTENSION."}
	string  coofile   {"default",prompt="output coordinate file."}
	int    fwhm  {3,prompt="Guess the seeing."}
	real    thresh {3,prompt="Threshold for PSF stars."}
	real	maxlin {20000,prompt="Maximum counts for linearity."}
	int	apmin {3,prompt="Small aperture. (not used)"}
	int	apmax {15,prompt="Large aperture. (not used)"}
	string  base  {"./",prompt="Working directory (include trailing /)."}
	int     dimen {1000,prompt="Size of the working image to use."}
	real    zeropt {26.0,prompt="Photometric zeropoint"}
	bool    plant {"yes",prompt="Actually build a psf? (NO=> fwhm only)"}

	string  *aplist

begin 	
  
# 	task parameters 
	string t_image 
	string t_coofile
	string t_psf 
	string t_base  
	string tmag
	int    t_dimen
	real    t_fwhm, xsig, ysig, sig, ssig, nsamp
	int    t_apmin
	int    t_apmax, wc, ac
	real   t_zeropoint
	bool   tplant
	string par1,par2
#   static fields
	string DF,origin
    
#	procedure variables
	string 	working, groupfile, root, buff
	int   x_size,nstars  
	int   y_size  
	int	x1,x2,y1,y2 , naps 
	real sky, apcor, aperr, n_fwhm, t_thre,t_maxlin
  
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
        tplant = plant
# in the interest of key strokes.
	DF = "default"
	if (substr(t_base,strlen(t_base),strlen(t_base)) != "/" )
		t_base = t_base//"/"
	t_image = t_base//t_image
	t_psf = t_image//"."//t_psf

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

	datapars.exposure="EXPTIME"
	datapars.gain="GAIN"
	datapars.airmass="AIRMASS"
	datapars.obstime="UTC-OBS"
	datapars.readnoise=7
	datapars.datamax=t_maxlin
	datapars.datamin=INDEF
	
#	get a CHUNK of the image, take a piece which is the middle dimen
#       in size. 
#	imgets(t_image,"i_naxis1")  
#	x_size=int(imgets.value)
#	imgets(t_image,"i_naxis2")  
#	y_size=int(imgets.value)  
#	
#	if (x_size > t_dimen && t_dimen > 0) {
#		x1 = x_size/2 - t_dimen/2 +1  
#		x2 = x1 + t_dimen - 1 
#	} else {
#		x1=1 
#		x2=x_size  
#	} 
#	
#	if ( y_size > t_dimen && t_dimen > 0 ) {
#		y1 = y_size/2 - t_dimen/2 +1
#		y2 = y1 + t_dimen - 1
#	} else {
#		y1 = 1 
#		y2 = y_size  
#	}
#	
# make a working copy of the image
# makepsf works on a subset of
# the image to speed up processing.
#    
#	root= mktemp("makepsf")
#	root= t_base//root
#	printf ("%s[%d:%d,%d:%d]\n",t_image,x1,x2,y1,y2) | scan(working)
	touch (t_image//".psf.FAILED")
	working = t_image;
	root = working;

	stepZjmp("-f",root)	
	if ( access(root//".stepZjmp.OK") ) {
	    # stepZ was succesful
	    kdelete ("stepZjmp.FAILED")
	    kdelete (root//".stepZjmp.OK")
	} else {
	   print ("stepZjmp FAILED")
	   print ("stepZjmp FAILED", >> t_image//".psf.FAILED")
	   bye;
	}
	
        step1jmp("-f",root,"-w",t_fwhm,"-t",t_thre,"-m",t_maxlin)
	if ( access(root//".step1jmp.OK") ) {
	    # step1 was succesful
	    kdelete ("step1jmp.FAILED")
	    kdelete (root//".step0jmp.OK")
	} else {
	   print ("step1jmp FAILED")
	   print ("step1jmp FAILED", >> t_image//".psf.FAILED")
	   bye;
	}

#
# Now fit gaussians to obj.jmp
#
	 n_fwhm = t_fwhm
#	if ( ! tplant ) { 
#	  kdelete(root//".coo.2")
#	  kdelete(root//".psffit")
#	  fitpsf(root,box=20,coords=root//".obj.jmp",function="radgauss", \
#		output=root//".psffit", interactive-,verify-,verbose-, \
#		mkbox-, nreject=3, kreject=3)
#          txdump(root//".psffit","rsigma", \
#		"IER==0")| average | scan(sig,ssig,nsamp)
#	  txdump(root//".psffit","xcenter,ycenter,amplitude,sky,id", \
#		"((IER==0) && (abs(rsigma - "//sig//") < 2.0*"//ssig//"))", \
#		> root//".coo.2")
#
#	  t_fwhm = sig*sqrt(4*log(2))
#	  n_fwhm = t_fwhm
#	  datapars.fwhm = n_fwhm
#	  kdelete(root//".psffit")
#	}else{

#  We should use a minimum aperture that is about 1.1 -1.2 the FWHM
# 
	  photpars.apertures = 1.2*t_fwhm
	  photpars.zmag = t_zeropoint
 	  kdelete(root//".mag.1")
 	  kdelete(root//".mag")
	  fitskypars.dannulus = 3*t_fwhm
	  fitskypars.annulus = 5*t_fwhm
	  phot(root,root//".obj.jmp",root//".mag",centerpars.calgori="gauss")
# reject stars with larger sky background than average and with any error
# in the calculation of their photometric magnitude
	  txdump(root//".mag","MSKY","CIER==0") | average | scan(sig,ssig,nsamp)

	  datapars.datamin = sig-3.0*ssig
	  tmag=mktemp(root)
	  pselect (root//".mag",tmag,"(MSKY>0 && MSKY < 1e5 && MSKY!=INDEF )")

	  pselect(tmag,root//".mag.1",\
		"CIER == 0 && (abs(MSKY - "//sig//") < 3.0*"//ssig//")")
	  kdelete(tmag)
	
# set the PSF radius and search for a maximum of 25 good PSF stars.
	  daopars.fitrad = 0.85*n_fwhm
	  daopars.psfrad = 5.0*n_fwhm

 	  kdelete(root//".pst.1")
	  pstselect(root,root//".mag.1",root//".pst.1",25)

# use those stars to build an actual PSF
	  if ( imaccess(root//".psf.1") ) imdelete(root//".psf.1")
          kdelete ( root//".pst.2")
          kdelete ( root//".psg.1")

	  psf(root,root//".mag.1",root//".pst.1",
		root//".psf.1",
		root//".pst.2",
		root//".psg.1",showplots-,interactive-)

	
# subtract the neighbour stars and rebuild the PSF

	  kdelete (root//".nst.1");
	  kdelete (root//".nrj.1");
	  
	  nstar(root,root//".psg.1",
		psfimage=root//".psf.1",
		nstarfile=root//".nst.1",
		rejfile=root//".nrj.1");

	  kdelete (root//".phot")

	  pselect(root//".nst.1",root//".phot","CHI < 2.5")

### I think this could be a subtraction of .psg stars.. need to check
### that is the case.
	  if ( imaccess(root//".sub.1") ) imdelete(root//".sub.1")
	  substar(root,root//".psg.1",root//".pst.1",root//".psf.1",
			root//".sub.1")
	
# Build the final PSF.  Neighbours have been removed and PSF
# stars that fit poorly are also removed.
	  if ( imaccess(t_psf) ) {
	     imdelete(t_psf,verify-)
	  }

	  psf(root//".sub.1",root//".phot",root//".pst.2",t_psf,
		root//".pst.3",
		root//".psg.2")

# Now we can get a real esitmate of the seeing
	  hselect(t_psf,"PAR1",yes) | scan(xsig)
	  hselect(t_psf,"PAR2",yes) | scan(ysig)
          t_fwhm = sqrt(xsig*xsig + ysig*ysig)
	  if ( abs((xsig - ysig)/t_fwhm) > 0.2 ) {
	   #JJK Added a warning message here. March 2002
	    touch (root//".psf.WARNING")
	    print("ERROR: The PSF is too elliptical for planting. image"//root, >> root//".psf.WARNING" )
	  }

	#
	#  Use the measured value even if it isn't good.
	#
	#    //t_fwhm = n_fwhm
	#  } else {
	#    t_fwhm = 1.05*t_fwhm*sqrt(8*log(2))/2.0
	#  }

	t_fwhm = 1.05*t_fwhm*sqrt(8*log(2))/2.0
	if ( t_fwhm > 8 ) { 
	   touch (root//".psf.WARNING")
	   print ("WARNING: Seeing value large? Measured "//t_fwhm, 
                       >> root//".psf.WARNING" ) 
	}
        kdelete(root//".coo.2")
	txdump(root//".phot","XCENTER,YCENTER,MAG,SHARPNESS,ID",yes, \
			headers+, > root//".coo.2")
#	}
	n_fwhm = t_fwhm
	datapars.fwhm = t_fwhm
        t_apmin = int(1.2*t_fwhm +0.5)
        t_apmax = int(5.0*t_fwhm +0.5)


	photpars.apertures=t_apmin//":"//t_apmax//":1"
	naps = t_apmax - t_apmin +1
	if (naps < 3) {
		error(1,"inner ("//t_apmin//") and outer ("//t_apmax//") aperatures must differ by more than 2pixels\n")
	}

        kdelete(root//".mag.2")
	phot(root,root//".coo.2",root//".mag.2")

        kdelete(root//".apcor")
	mkapfile(root//".mag.2",naps,root//".apcor",interactive-,verify-)

#-------BG tested number of PSF stars and that aperture corr worked ---------

#	Count the # of PSF stars used. 
        touch ( root//".countpsf" )
	pdump(root//".pst.2", "XCENTER,YCENTER", yes, >> root//".countpsf")
	count(root//".countpsf") | scan(wc)
        kdelete(root//".countpsf")
	# Lynne wants this to be minimum 10 stars....
	if ( wc > 9 ) {
	  print ("Number of PSF stars used was : ", wc)
	} else {
	  # Flag the operators with a .WARNING file
	  touch (root//".psf.WARNING")
	  print("ERROR: only "//wc//" psf stars for image "//root, 
                     >> root//".psf.WARNING" )
	}
# --------------------
#	Only do aperture correction if number of PSF stars was 1 or more
        if ( wc > 0 ) {
	 count(root//".apcor") | scan(ac)
	 # If any lines are in apcor file it was 'successful'
	 if ( ac > 0 ) {
		aplist = root//".apcor"
		i= fscan(aplist,s1)
		i= fscan(aplist,s1)
		i= fscan(aplist,s1,apcor,aperr)
		apcor = -1.0*apcor
	 } else {
	  # the aper calc failed.  so these are the default values
	  touch (root//".apcor.FAILED")
	  print("FAILED apcor for "//root, >> root//".apcor.FAILED" )
	  apcor=30.0
	  aperr=30.0
	 }
	}

	# Stick the apcor line in the way the pipeline wants it
	kdelete(root//".apcor")
	printf("%d %d %6.2f %6.2f\n",t_apmin,t_apmax,apcor,aperr,
	   >> root//".apcor" ) 
	if  ( t_apmin < 2) { 
	  touch ( root//".apcor.WARNING" ) 
	  print ("WARNING: t_apmin small: "//t_apmin,>> root//".apcor.WARNING")
	}
	if  ( t_apmax > 35) { 
	  touch ( root//".apcor.WARNING" ) 
	  print ("WARNING: t_apmax huge: "//t_apmax,>> root//".apcor.WARNING")
	}
	if  ( apcor > 1.0 ) { 
	      touch ( root//".apcor.WARNING" ) 
	      print ("ERROR: apcor large ", >> root//".apcor.WARNING")
	}
	if  ( aperr > 0.2 ) { 
	      touch ( root//".apcor.WARNING" ) 
	      print ("ERROR: aperr large ", >> root//".apcor.WARNING")
	}
	if ( !access(t_base//"zeropoint.used") ){
	   print(t_zeropoint,> t_base//"zeropoint.used");
	}
        delete(root//".mag*",verify-)
#        delete(root//".pst*",verify-)
        delete(root//".psg*",verify-)
        delete(root//".coo*",verify-)
	delete(root//".nrj*",verify-)
	delete(root//".nst*",verify-)
	imdelete(root//".psf.1",verify-)
	imdelete(root//".sub*",verify-)

	if ( imaccess(t_psf) ) {
	   print("\n"//t_psf//" OK\n")
	   delete(t_image//".psf.FAILED",verify-)
	} else {
	   pwd | scan(t_base)
	   print ("\n makepsf: FAILED in "//t_base//" for "//t_image//"\n")
	}
        

	
end

