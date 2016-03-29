"""Build an IRAF PSF for use in adding artificial sources to an image.

This process assumes we are working with a CFHT/MEGAPRIME image.
"""
import storage
from pyraf import iraf

AIRMASS='AIRMASS'
EXPTIME='EXPTIME'
PHOT_K='PHOT_K'
AP_OUT_SCALE=5
PSF_EXT='psf'
PHOT_EXT='phot'
ZERO_PT_USED='zeropoint.used'

class PSF(object):

    def init(self, expnum, ccd, prefix, version):
        """

        @param expnum: number of the exposure this PSF corresponds to
        @param ccd: the CCD on that exposure that this PSF is for.
        @param fwhm:
        @param thresh:
        @param maxlin:
        @return:
        """
        self.expnum = expnum
        self.ccd = ccd
        self.prefix = prefix
        self.version =version

    def build(self, fwhm=None, maxlin=40000, zeropoint=None):
        """

        @param fwhm:  initial guess of the FWHM, used for selecting sources that might be stars
        @param thresh: the detection threshold for stars (in sky sigma)
        @param maxlin: maximum linearity of the CCD, brighter sources are saturated.
        @param zeropoint: the cts/exptime mag zeropoint of the image
        @return:
        """
        if not zeropoint:
            zeropoint = storage.get_zeropoint(self.expnum,
                                              self.ccd,
                                              self.prefix,
                                              self.version)
        if not fwhm:
            fwhm = storage.get_fwhm(self.expnum,self.ccd,prefix=self.prefix, version=self.version)

        filename = storage.get_image(self.expnum,
                                     self.ccd,
                                     version=self.version,
                                     prefix=self.prefix)
        basename = os.path.splitext(filename)[0]

        iraf.set(writepars=0)

        iraf.kinky
        iraf.pipeline
        phot_filename = "{}.{}".format(basename, PHOT_EXT)
        psf_filename = "{}.{}".format(basename, PSF_EXT)
        zeropt_filename = "{}.{}".format(basename, ZERO_PT_USED)
        with open(zeropt_filename, 'w') as z:
            z.write("{}\n".format(zeropoint))

        apmax = AP_OUT_SCALE * fwhm
        swidth = fwhm

        # Turn off verbosity and parameter checking in DAOPHOT
        iraf.daophot.verify=iraf.no
        iraf.daophot.verbose=iraf.no
        iraf.daophot.update=iraf.no
        iraf.psf.showplots=iraf.no
        iraf.psf.interactive=iraf.no

        iraf.centerpars.calgori="centroid"
        iraf.centerpars.maxshift=3

        # set the basic 'datapars' for the images.
	    iraf.datapars.exposure=""
        # Note the itime is set to 1 as the SGwyn ZP includes the exposure time.
	    iraf.datapars.itime=1
        iraf.datapars.gain="GAIN"
        iraf.datapars.airmass="AIRMASS"
        iraf.datapars.obstime=""
        iraf.datapars.ccdread=""
        iraf.datapars.readnoise=7
        iraf.datapars.datamax=maxlin
        iraf.datapars.datamin=-1000
        iraf.datapars.fwhmpsf=fwhm


        # Reject stars which have bad pixels in the fitting radius.
        # This is done by computing the aperture photometry on those stars
        iraf.photpars.zmag=zeropoint
        iraf.fitskypars.dannulus=swidth
        iraf.fitskypars.annulus= apmax + 2

        iraf.phot(filename, t_image//".bright.psf",
             t_image//".bright.mag",
             photpars.apertures=apmax)

	# reject stars with any error in the calculation
	# of their photometric magnitude
	kdelete(t_image//".coo.1")
	txdump (t_image//".bright.mag", "XCEN,YCEN", "(SIER==0)&&(CIER==0)&&(PIER==0)", > t_image//".coo.1");
	txdump (t_image//".bright.mag", "XCEN,YCEN", "(SIER==0)&&(CIER==0)&&(PIER==0)", | average | scan(ssig, dum, nsamp);
        print "Kept "//nsamp//" PSF stars"
	print "Determining sky level using PSF stars"
	# Determine the sky threshold:
	# Used reject stars with larger sky background than average and with any error
	txdump(t_image//".bright.mag","MSKY","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)") | average | scan(sig,dum,nsamp);
	txdump(t_image//".bright.mag","STDEV","(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)") | average | scan(ssig,dum,nsamp);

	print ("SKY: "//sig//" STDEV: "//ssig//" NPOINT: "//nsamp);
	#datapars.datamin = (sig-20.0*ssig);

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
	    # using an aperture smaller than 3 pixels under samples
	    if ( apmin < 1.5 )  {
		 apmin = 1.5
	    }
            apmax = int(10*t_apout*t_fwhm + 0.5) / 10.0
            if ( apmax < apmin ) {
                 apmax = apmin + 1
            }
	    naps = (apmax - apmin) + 1
            apmax = apmin + naps
	    naps = (apmax - apmin) + 1

  	    print("apmin : ", apmin)
	    print("apmax : ", apmax)
	    print("naps  : ", naps)


	    # Adjust the large aperture size too, and the sky fitting parameters
	    fitskypars.dannulus = t_swidth*t_fwhm
	    fitskypars.annulus =  apmax + 2

	    # set the PSF radius and fitting radius
	    daopars.fitrad = apmin
	    daopars.psfrad = apmax + 1
	    datapars.fwhm = t_fwhm
	    print "Using fwhm of --> "//t_fwhm

	    ## Compute photometry of on selected PSF stars to set scale of PSF
	    kdelete(t_phot)
	    phot(t_image,t_image//".coo.1",t_phot, photpars.apertures=apmax)

	    ## select out those that have bad values of centroid and photometry issues.
	    ## the first step removes those stars that have INDEF values for MSKY since those
	    ## stars cause math errors during the second step
	    temp_mag=mktemp(t_image)
	    pselect(t_phot,temp_mag,"(CIER==0)&&(PIER==0)&&(SIER==0)&&(MSKY>0 && MSKY < 1e5 && MSKY!=INDEF )")

	    print "Removing stars with sky levels that are more than 10 sigma from the mean sky."
	    ## now the 'INDEF' values of MSKY are gone so remove stars with large MSKY deviations
	    kdelete(t_phot)
	    pselect(temp_mag, t_phot, "(abs(MSKY - "//sig//") < 10.0*"//ssig//")")
	    kdelete(temp_mag)
	    kdelete(t_image//".psg")
	    kdelete(t_image//".pst")
	    if ( imaccess(t_psf) ) {
	       imdelete(t_psf,verify-)
	    }
	    iferr { psf(t_image, t_phot, t_phot,
		    t_psf,
		    t_image//".pst",
		    t_image//".psg",
		    showplots-, interactive-, verbose-) }
		    goto finalproc

            print("Using nstar to find the good matches to the PSF...")
	    kdelete (t_image//".nst.1")
	    kdelete (t_image//".nrj.1")
	    nstar(t_image,t_image//".psg", psfimage=t_psf, nstarfile=t_image//".nst.1", rejfile=t_image//".nrj.1")

            # reject stars whose chi2 is large compared to the mean chi2 of stars in the PSF
	    txdump(t_image//".nst.1",
                   "CHI","(PIER==0)") | average | scan(achi,schi,nsamp);
            print("1st Average CHI2 "//achi//" +/- "//schi)
            x=achi+schi
            if ( x < 2.5 ) {
               x = 2.5
            }

	    kdelete(t_phot)
	    pselect(t_image//".nst.1", t_phot, "(CHI<"//x//")&&(PIER==0)")
	    psort(t_phot, "CHI", ascend+)

	    # use those stars to build an new PSF
	    kdelete ( t_image//".pst")
	    kdelete ( t_image//".psg")
	    if ( imaccess(t_psf) ) {
	       imdelete(t_psf,verify-)
	    }

	    iferr { psf(t_image, t_phot, t_phot,
		    t_psf,
		    t_image//".pst",
		    t_image//".psg", showplots-, interactive-, verbose-) }
	    goto finalproc

            # Use nstar to find the fluxes of psg stars
	    kdelete(t_image//".nst.1")
	    kdelete(t_image//".nrj.1")
	    nstar(t_image,t_image//".psg", psfimage=t_psf, nstarfile=t_image//".nst.1", rejfile=t_image//".nrj.1")

            print("Using the new PSF and FLUX to subtract the neighbouring stars.")
	    if ( imaccess(t_image//".sub.1") ) {
              imdelete(t_image//".sub.1")
            }

            substar(image=t_image, photfile=t_image//".nst.1",
                    exfile=t_image//".pst", psfimage=t_psf, subimage=t_image//".sub.1", verify-, update-, verbose-)

    	    # run PHOT on the subtracted image to get the flux zeropoint correct.
	    kdelete(t_phot)
	    kdelete(t_image//".coo.1")

	    txdump(t_image//".nst.1",
                   "CHI","(PIER==0)") | average | scan(achi,schi,nsamp);
            print("2nd Average CHI2 "//achi//" +/- "//schi)
            x=achi+schi
            if ( x < 1.5 ) {
               x = 1.5
            }

	    psort(t_image//".nst.1", "CHI", ascend+)
	    txdump(t_image//".nst.1","XCEN,YCEN,ID","(CHI < "//x//")&&(PIER==0)", > t_image//".coo.1")

	    phot(t_image,t_image//".coo.1",t_phot, photpars.apertures=apmax)

	    # build a PSF using the PSF neighbour subtracted image.
	    kdelete ( t_image//".pst")
	    kdelete ( t_image//".psg")
	    if ( imaccess(t_psf) ) {
	       imdelete(t_psf,verify-)
	    }

	    iferr { psf(t_image//".sub.1",t_phot, t_phot,
		    t_psf,
		    t_image//".pst",
		    t_image//".psg",showplots-,interactive-) }
	    goto finalproc

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
	    # compute the apcor using the PSF stars.

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
	phot(t_image,t_image//".coo.1",t_image//".mag.2", photpars.apertures=apmin//":"//apmax+0.1//":1")


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

