#
# makepsf produces a PSF from an input image, with seeing estimate and
#	     and aperture correction
#
# Last modified: Apr. 16, 2004, JMP.

procedure makepsf (image,psf,fwhm,thresh,maxlin) ;
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

# BG removed; this is never used in this script
#	bool    plant {"yes",prompt="Actually build a psf? (NO=> fwhm only)"}

	string  *aplist

begin 	
  
# 	task parameters 
	string t_image 
	string t_coofile
	string t_psf 
	string t_base  
	string tmag
	int    t_dimen, failedflag
	real    t_fwhm, xsig, ysig, sig, ssig, nsamp, dum
	int    t_apmin, t_apmax, wc, ac
	real   t_zeropoint
	string par1,par2
#   static fields
	string DF,origin
    
#	procedure variables
	string 	working, groupfile, root, buff, newfile
	int   x_size,nstars  
	int   y_size  
	int	x1,x2,y1,y2 , naps, npar
	real sky, apcor, aperr, t_thre,t_maxlin
  
# Flow monitoring control
	touch (image//".makepsf.FAILED")
	print("invoked with : "//image//" , "//psf, >> image//".makepsf.FAILED")
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
#        tplant = plant
# in the interest of key strokes.
	DF = "default"
	if (substr(t_base,strlen(t_base),strlen(t_base)) != "/" )
		t_base = t_base//"/"
	t_image = t_base//t_image
	t_psf = t_image//"."//t_psf

# In case of failure, record paramaters
	print("FWHM  : ", t_fwhm, >> image//".makepsf.FAILED")
	print("apmin : ", t_apmin, >> image//".makepsf.FAILED")
	print("apmax : ", t_apmax, >> image//".makepsf.FAILED")
	print("thresh: ", t_thre, >> image//".makepsf.FAILED")
	print("maxlin: ", t_maxlin, >> image//".makepsf.FAILED")
	print("base  : ", t_base, >> image//".makepsf.FAILED")
	print("ZP    : ", t_zeropoint, >> image//".makepsf.FAILED")

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
	datapars.itime=1
	datapars.exposure=""
	datapars.gain="GAIN"
	datapars.airmass="AIRMASS"
	datapars.obstime=""
	datapars.ccdread=""
	datapars.readnoise=7
	datapars.datamax=t_maxlin
	datapars.datamin=-1000
	datapars.fwhmpsf=t_fwhm

	working = t_image;
	root = working;


#  We first reject stars which have bad pixels in the fitting radius.
# 

	t_apmin = int(1.2*t_fwhm + 0.5)
	if ( t_apmin  < 3 ) {
	   t_apmin = 3
        }
	t_apmax=5*t_fwhm

# set the PSF radius and search for a maximum of 25 good PSF stars.
	daopars.fitrad = t_apmin
	daopars.psfrad = t_apmax
	fitskypars.dannulus = int(t_fwhm +0.5)
	fitskypars.annulus =  int(5*t_fwhm+0.5) + 1


	print "Doing large aperture photometry on stars found in "//root//".obj.psf"
	photpars.apertures = t_apmax
	photpars.zmag = t_zeropoint
	kdelete(root//".mag.1")
 	kdelete(root//".mag")

	phot(root,root//".obj.psf",root//".mag",centerpars.calgori="centroid")

# reject stars with any error in the calculation of their photometric magnitude
	kdelete(root//".coo1")
	txdump (root//".mag","XCEN,YCEN","(SIER==0)&&(CIER==0)&&(PIER==0)", > root//".coo1")
	copy(root//".mag",root//".mag.check")
#  We should use a minimum aperture that is about 1.1 -1.2 the FWHM
# 

# reject stars with larger sky background than average and with any error
# in the calculation of their photometric magnitude

        print "Selecting out stars that have valid sky, centroid, photometry "
	txdump(root//".mag","MSKY","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)") | average | scan(sig,dum,nsamp)
	txdump(root//".mag","STDEV","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)") | average | scan(ssig,dum,nsamp)

	print ("SKY: "//sig//" STDEV: "//ssig//" NPOINT: "//nsamp)
	datapars.datamin = (sig-5.0*ssig)


	photpars.apertures = t_apmin
	kdelete(root//".mag")

        print "Redoing photoemtry using culled list, with a small aperture "//t_apmin//", to build PSF input file"

	phot(root,root//".coo1",root//".mag",centerpars.calgori="centroid")

	## reject stars that have centroid/photometry or sky problems.
	## do this in two steps since 'INDEF' causes error in math conditions
	tmag=mktemp(root)
	pselect (root//".mag",tmag,"(CIER==0)&&(PIER==0)&&(SIER==0)&&(MSKY>0 && MSKY < 1e5 && MSKY!=INDEF )")
	pselect(tmag,root//".mag.1",\
		"CIER == 0 && (abs(MSKY - "//sig//") < 3.0*"//ssig//")")
	kdelete(tmag)
	
 	## Select the PSF stars (up to 25)
	print "Selecting PSF stars"
	kdelete(root//".pst.1")
	pstselect(root,root//".mag.1",root//".pst.1",25)

# use those stars to build an actual PSF
        kdelete ( root//".pst.2")
        kdelete ( root//".psg.1")
	kdelete ( root//".psf.1.fits")

	print "building psf with stars from "//root//".pst.1"
	psf(root,root//".mag.1",root//".pst.1",
		root//".psf.1",
		root//".pst.2",
		root//".psg.1",showplots-,interactive-)

	kdelete (root//".nst.1");
	kdelete (root//".nrj.1");
	  
	nstar(root,root//".psg.1",
		psfimage=root//".psf.1",
		nstarfile=root//".nst.1",
		rejfile=root//".nrj.1");


	print "Subtracting the non-psf stars from the image"	
	if ( imaccess(root//".sub.1") ) imdelete(root//".sub.1")
	substar(root,root//".psg.1",root//".pst.2",root//".psf.1",
			root//".sub.1")
	
# Build the final PSF.  Neighbours have been removed and PSF
# stars that fit poorly are also removed.
	  if ( imaccess(t_psf) ) {
	     imdelete(t_psf,verify-)
	  }

	print "Seleting stars from "//root//".nst.1 that have CHI<2.5 on their PSF fits"

	kdelete (root//".phot")
	pselect(root//".nst.1",root//".phot","CHI < 2.5")
	  
 	kdelete(root//".pst")
	pstselect(root,root//".phot",root//".pst",25)

	print "Building the psf from the stars that fit the previous PSF best,"
	print "using the neighbour subtracted image"
        kdelete ( root//".pst.3")
	psf(root//".sub.1",root//".phot",root//".pst",t_psf,
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


	# Value from the header is actually sigma so convert to FWHM
	t_fwhm = 1.05*t_fwhm*sqrt(8*log(2))/2.0
	if ( t_fwhm > 8 ) { 
	    touch (root//".psf.WARNING")
	    print ("WARNING: Seeing value large? Measured "//t_fwhm, 
                      >> root//".psf.WARNING" ) 
	}
	print "Measured value of the FWHM (scaling off the psf) is "//t_fwhm

        kdelete(root//".coo.2")
	txdump(root//".phot","XCENTER,YCENTER,MAG,SHARPNESS,ID",yes, \
			headers+, > root//".coo.2")

	datapars.fwhm = t_fwhm

	t_apmin = int(1.2*t_fwhm + 0.5)
	if ( t_apmin  < 3 ) {
	     t_apmin = 3
        }

        t_apmax = int(5.0*t_fwhm +0.5)
	if ( t_apmax -1 < t_apmin ) { 
	       t_apmax = t_apmin + 2 
	}
	print "Rebuilding the PSF with the new apmin: "//t_apmin

	photpars.apertures=t_apmin
	fitskypars.annulus =  int(t_apmax+1.5)
	fitskypars.dannulus = int(t_fwhm +0.5)

	kdelete(root//".mag.2")
	phot(root,root//".coo.2",root//".mag.2")

 	kdelete(root//".pst")
	pstselect(root,root//".mag.2",root//".pst",25)


	print "Building the psf from the stars that fit the previous PSF best, using the neighbour subtracted image"
        kdelete ( root//".pst.3")
	kdelete ( root//".psg.2")
	kdelete ( t_psf//".fits")
	psf(root//".sub.1",root//".mag.2",root//".pst",t_psf,
		root//".pst.3",
		root//".psg.2")

	  
	print "using the following as the apcor boundaries"
	print "apmin "//t_apmin
	print "apmax "//t_apmax
	print "sky   "//int(t_apmax+1.5)
	print "width "//int(t_fwhm+0.5)


	photpars.apertures=t_apmin//":"//t_apmax//":1"
	naps = t_apmax - t_apmin +1
	if (naps < 3) {
		error(1,"inner ("//t_apmin//") and outer ("//t_apmax//") aperatures must differ by more than 2pixels\n")
	}

        kdelete(root//".mag.2")
	phot(root,root//".coo.2",root//".mag.2")

        kdelete(root//".apcor")

	npar = 3
	pwd | scan(working)

  mkapfileloop:

	mkapfile(root//".mag.2",naps,root//".apcor",interactive-,verify-,nparams=npar)

	if ( !access(root//".apcor") ) {
	   npar = npar + 1
	   if (npar <= 5) goto mkapfileloop
	   touch (root//".apcor")
	}
	#delete("correction_d_ouverture", verify-)
	#delete("execmymk.sh", verify-)

#-------BG tested number of PSF stars and that aperture corr worked ---------

#	Count the # of PSF stars used. 
        touch ( root//".countpsf" )
	pdump(root//".pst.3", "XCENTER,YCENTER", yes, >> root//".countpsf")
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
	  failedflag = 1
	  print("FAILED apcor for "//root, >> root//".makepsf.FAILED" )
	  apcor=30.0
	  aperr=30.0
	 }
	}

	# Stick the apcor line in the way the pipeline wants it
	kdelete(root//".apcor")
	printf("%d %d %6.2f %6.2f\n",t_apmin,t_apmax,apcor,aperr,
	                             >> root//".apcor" ) 
	if  ( t_apmin < 3) { 
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
	kdelete(root//".zeropoint.used")
	print(t_zeropoint,> root//".zeropoint.used");

	## an odd condition loop to allow one to turn deleting of intermediate files off/on
	if ( 1==1) { 
          delete(root//".mag.*",verify-)
	  kdelete(root//".psf.*.fits")
          delete(root//".pst.*",verify-)
          delete(root//".psg.*",verify-)
          delete(root//".coo.*",verify-)
	  delete(root//".nrj.*",verify-)
	  delete(root//".nst.*",verify-)
	  delete(root//".sub.*",verify-)
	}
	if ( imaccess(t_psf) ) {
	   print("\n"//t_psf//" OK\n")
	} else {
	   failedflag = 1
	   pwd | scan(t_base)
	   print("\n makepsf: FAILED in "//t_base//" for "//t_image//"\n", >> root//".makepsf.FAILED")
	}
        if (failedflag == 0) {
	   touch (root//".makepsf.OK")
	}
	
end

