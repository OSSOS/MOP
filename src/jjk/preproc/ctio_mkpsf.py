#!/usr/cadc/misc/bin/python

#*
#************************************************************************
#*
#*  Script Name: mkpsf
#*  Purpose:
#*     Construct a PSF for a given CFH12K image use pyRAF/IRAF/DAOPHOT
#*  Method:
#*  Creation Date : March 03, 2004
#*
#*  CVS Info :
#*  Programmer     : $Author: observe $
#*  Module Name    : $Id: ctio_mkpsf.py,v 1.1 2005/01/16 04:56:55 observe Exp $
#*  Version Number : $Revision: 1.1 $
#*  Last Updated   : $Date: 2005/01/16 04:56:55 $
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/
"""Create a psf for the input image.

The PSF is created using the iraf/daophot tasks.  If the input file is 
in MEF format then an MEF psf file is produced, with a PSF for each
extension in the input image.  This program was designed to work
on CFHT 12K image and produces the PSF that are provided by the CADC
12k Archive.
"""



def build(f):
    
    ### is this an MEF file
    current_ext=0
    NEXTEND=0

    EXTEND=f[0].header['EXTEND']
    if (EXTEND=="T"):
        NEXTEND=f[0].header['NEXTEND']
        current_ext=1

    ### create the name of the output MEF psf file
    if not f[0].header.has_key('FILENAME'):
        os.unlink(opt.filename)
        sys.exit('The fits file '+opt.filename+' has no EXPNUM keyword\n')

    
    sexp = f[0].header['FILENAME']
    mef_psf=sexp+"p_psf_iraf.fits"
    ### create an MEF file that will contian the PSF(s)
    import pyfits
    fitsobj = pyfits.HDUList()
    prihdu = pyfits.PrimaryHDU()
    import re
    prihdu.header.update('FILENAME',sexp,comment='CFHT Exposure Numebr')
    prihdu.header.update('NEXTEND',NEXTEND,comment='number of extensions')
    version=re.match(r'\$Rev.*: (\d*.\d*) \$',__Version__).group(1)
    prihdu.header.update('MKPSF_V',float(version),
                         comment="Version number of mkpsf")
    fitsobj.append(prihdu)
    if os.access(mef_psf,os.F_OK):
        os.unlink(mef_psf)
    fitsobj.writeto(mef_psf)
    fitsobj.close()
    outfits = pyfits.open(mef_psf,"append")
    prihdr=outfits[0].header
    
    import jjkmode
    
    ### Get my python routines
    from pyraf import iraf
    from pyraf.irafpar import IrafParList

    ### keep all the parameters locally cached.
    iraf.set(uparm="./")


    ### Load the required IRAF packages
    iraf.digiphot()
    iraf.apphot()
    iraf.daophot()
    
    ### temp file name hash.
    tfile={}
    
    
    while ( current_ext <= NEXTEND ):

     ### this is a psf SCRIPT so the showplots and interactive are off by force

        print "Working on image section "+str(current_ext)
        
        
        iraf.findpars.sharplo=0
        iraf.findpars.sharphi=0.7
        iraf.findpars.roundlo=-0.7
        iraf.findpars.roundhi=0.7
        
        iraf.datapars.datamax=20000
        iraf.datapars.airmass='AIRMASS'
        iraf.datapars.filter='FILTER'
        iraf.datapars.obstime='TIME-OBS'
        iraf.datapars.exposure='EXPTIME'
        iraf.datapars.gain='GAIN'
        iraf.datapars.ccdread='RDNOISE'
        iraf.datapars.fwhmpsf=opt.fwhm
        
        iraf.daopars.nclean=2
        iraf.daopars.psfrad=5.0*opt.fwhm
        iraf.daopars.fitrad=0.85*opt.fwhm
        iraf.daopars.function="gauss"
        
        iraf.centerpars.calgorithm='centroid'
        
        zero_mag=26.19
        iraf.photpars.zmag=zero_mag
        iraf.photpars.apertures=int(0.85*opt.fwhm)
        iraf.fitskypars.annulus=2+int(opt.fwhm*4.00)
        iraf.fitskypars.dannulus=int(opt.fwhm*2.0)
        iraf.daophot.verbose=no
        iraf.daophot.verify=no
        iraf.daophot.update=no
        
        iraf.psf.interactive=no
        iraf.pstselect.interactive=no
        
        iraf.datapars.saveParList()
        iraf.fitskypars.saveParList()
        iraf.centerpars.saveParList()
        iraf.findpars.saveParList()
        iraf.photpars.saveParList()
        
        tfiles = ['coo_bright','coo_ok','coo_faint', 'mag_all',
                  'mag_bright', 'mag_ok', 'mag_good', 'mag_best',
                  'pst_in', 'pst_out', 'pst_out2', 'prf', 'psg_org',
                  'psg', 'psf_1.fits', 'psf_2.fits', 'psf_final.fits',
                  'psf_3.fits', 'psf_4.fits', 'mag_pst', 'coo_pst',
                  'nst', 'nrj', 'seepsf.fits', 'sub.fits', 'fwhm', 'apcor']
        
        for file in tfiles:
            extname="chip"+str(f[current_ext].header.get('IMAGEID',str(current_ext))).zfill(2)
            tfile[file]=sexp+"_"+extname+"."+file
            if ( os.access(tfile[file],os.F_OK) ):
                os.unlink(tfile[file])

	if (EXTEND=="T"):
            this_image=opt.filename+"["+extname+"]"
        else:
            this_image=opt.filename
        gain = f[current_ext].header.get('GAIN',1.0)
        #### set sky/sigma parameters specific to this frame.
        (skyvalue, sigma) = jjkmode.stats(f[current_ext].data)
        import math
        sigma=math.sqrt(skyvalue/gain)
        datamin = float(skyvalue)-8.0*float(sigma)
        print "Determined sky level to be "+str(skyvalue)+" +/-"+str(sigma)
        
        iraf.datapars.datamin=datamin
        iraf.datapars.sigma=float(sigma)
        iraf.datapars.saveParList()

        iraf.fitskypars.skyvalue=skyvalue
        iraf.fitskypars.saveParList()
        ### find the bright stars in the image.
        print "sextracting for stars in "+this_image
         
        ###iraf.daophot.daofind(image=this_image,
        ###                     output=tfile['coo_bright'],threshold=4.0)
	
	os.system("sex -c /home/cadc/kavelaar/12kproc/config/default.sex -SATUR_LEVEL 25000 -CATALOG_NAME "+tfile['coo_bright']+" "+this_image)
	
        
        ### print "finding stellar locus in sround/ground space"
        print "clipping using star_class > 0.85 " 
        fcoo = open(tfile['coo_bright'],'r')
        lines=fcoo.readlines()
        fcoo.close()
        import numarray, math
        fout = open(tfile['coo_ok'],'w')
        for line in lines:
            if re.match(r'^#',line) or re.search(r'INDEF',line):
                continue
            values=line.split()
            star_class=float(values[2])
            if star_class > 0.75 :
                fout.write(line)
	fout.close()


        print "Measuring photometry for psf candidate stars in "+tfile['coo_ok']
        iraf.daophot.phot(image=this_image,
                  coords=tfile['coo_ok'],
                  output=tfile['mag_bright'])

        ### do this selection in 2 steps because of the way IRAF handles INDEFs
        print "Selecting stars that have good centroids and magnitudes "
        iraf.pselect(tfile['mag_bright'],tfile['mag_ok'],
                     "(CIER==0)&&(PIER==0)&&(SIER==0)&&(MSKY>0)&&(MSKY<2e5)&&(MSKY!=INDEF)")
        print "Selecting stars that have normal sky levels"
        condition="(abs(MSKY -"+str(skyvalue)+") < 5.0*"+str(sigma)+")"
        iraf.pselect(tfile['mag_ok'],tfile['mag_good'],condition)

        a=iraf.txdump(tfile['mag_good'],"SSKEW",iraf.yes,Stdout=1)
        aa=[]
        for v in a:
            aa.append(float(v))

        a=numarray.array(aa)
        mean=a.mean()
        aa=a*a
        
        stddev = math.sqrt(aa.sum()/len(aa) - mean**2 )
        limit = mean+2*stddev

        os.unlink(tfile['mag_good'])
        condition=condition+" && SSKEW < "+str(limit)
        iraf.pselect(tfile['mag_ok'],tfile['mag_good'],condition)
        
        print "Choosing the psf stars"
        iraf.pstselect(image=this_image,
                       photfile=tfile['mag_good'],
                       pstfile=tfile['pst_in'],
                       maxnpsf=25)


        ## construct an initial PSF image
        print "computing psf with neighbor stars based on complete star list"
        iraf.psf.mode='a'
        iraf.psf(image=this_image,photfile=tfile['mag_bright'],
                 pstfile=tfile['pst_in'],psfimage=tfile['psf_1.fits'],
                 opstfile=tfile['pst_out'],
                 groupfile=tfile['psg_org'],
                 varorder=0)


        try:
            print "subtracting the psf neighbors and placing the results in "+tfile['sub.fits']
            iraf.daophot.nstar(image=this_image,
                               groupfile=tfile['psg_org'],
                               psfimage=tfile['psf_1.fits'],
                               nstarfile=tfile['nst'],
                               rejfile=tfile['nrj'])


            iraf.daophot.substar(image=this_image,photfile=tfile['nst'],
                                 exfile=tfile['pst_in'],
                                 psfimage=tfile['psf_1.fits'],
                                 subimage=tfile['sub.fits'])

            a=iraf.daophot.txdump(tfile['nst'],'chi','yes',Stdout=1)
            aa=[]
            for v in a:
                aa.append(float(v))

            a=numarray.array(aa)
            mean=a.mean()
            aa=a*a

            stddev = math.sqrt(aa.sum()/len(aa) - mean**2 )
            limit = mean+2.5*stddev

            print "Selecting those psf stars with CHI^2 <"+str(limit)+" after fitting with trial psf" 
            iraf.pselect(tfile['nst'],tfile['mag_best'],"CHI < "+str(limit))

            os.unlink(tfile['pst_out'])
        ##    os.unlink(tfile['psg'])
        ## rebuild the PSF file with the psf stars that fit well..
        ## using the neighbor subtracted image

            print "Rebuilding the PSF"

            iraf.daophot.psf(image=tfile['sub.fits'],
                             photfile=tfile['mag_best'],
                             pstfile=tfile['pst_in'],
                             psfimage=tfile['psf_2.fits'],
                             opstfile=tfile['pst_out'],
                             groupfile=tfile['psg'],
                             varorder=0)

            print "re-subtracting with rebuilt psf"

            os.unlink(tfile['nst'])
            os.unlink(tfile['nrj'])
            iraf.daophot.nstar(image=this_image,
                               groupfile=tfile['psg'],
                               psfimage=tfile['psf_2.fits'],
                               nstarfile=tfile['nst'],
                               rejfile=tfile['nrj'])

            os.unlink(tfile['sub.fits'])
            iraf.daophot.substar(image=this_image,photfile=tfile['nst'],
                                 exfile=tfile['pst_in'],
                                 psfimage=tfile['psf_2.fits'],
                                 subimage=tfile['sub.fits'])

            os.unlink(tfile['psg'])
            os.unlink(tfile['pst_out'])
            iraf.daophot.psf(image=tfile['sub.fits'],
                             photfile=tfile['mag_best'],
                             pstfile=tfile['pst_in'],
                             psfimage=tfile['psf_3.fits'],
                             opstfile=tfile['pst_out'],
                             groupfile=tfile['psg'],
                             varorder=0)

            os.unlink(tfile['nrj'])
            os.unlink(tfile['nst'])
            iraf.daophot.nstar(image=this_image,
                               groupfile=tfile['psg'],
                               psfimage=tfile['psf_3.fits'],
                               nstarfile=tfile['nst'],
                               rejfile=tfile['nrj'])

            a=iraf.daophot.txdump(tfile['nst'],'chi','yes',Stdout=1)
            aa=[]
            for v in a:
                aa.append(float(v))

            a=numarray.array(aa)
            mean=a.mean()
            aa=a*a

            stddev = math.sqrt(aa.sum()/len(aa) - mean**2 )
            limit = mean+2*stddev
	    limit = 2.0


            #print "Selecting those psf stars with CHI^2 < "+str(limit)+" after fit with GOOD psf"

            os.unlink(tfile['mag_best'])
            iraf.pselect(tfile['nst'],tfile['mag_best'],"CHI < "+str(limit))

            print "Building final PSF.... "
            os.unlink(tfile['sub.fits'])
            iraf.daophot.substar(image=this_image,photfile=tfile['nst'],
                                 exfile=tfile['pst_in'],
                                 psfimage=tfile['psf_3.fits'],
                                 subimage=tfile['sub.fits'])

            os.unlink(tfile['psg'])
            os.unlink(tfile['pst_out'])
            iraf.daophot.psf(image=tfile['sub.fits'],
                             photfile=tfile['mag_best'],
                             pstfile=tfile['pst_in'],
                             psfimage=tfile['psf_final.fits'],
                             opstfile=tfile['pst_out'],
                             groupfile=tfile['psg'],
                             varorder=0)

            print "building an analytic psf for the FWHM calculations"
            os.unlink(tfile['pst_out'])
            os.unlink(tfile['psg'])
            iraf.daophot.psf(image=tfile['sub.fits'],
                             photfile=tfile['mag_best'],
                             pstfile=tfile['pst_in'],
                             psfimage=tfile['psf_4.fits'],
                             opstfile=tfile['pst_out'],
                             groupfile=tfile['psg'],
                             varorder=-1)
        except:
            print sys.exc_info()[1]
            print "ERROR: Reverting to first pass psf"
            tfile['psf_final.fits']=tfile['psf_1.fits']

            iraf.daophot.psf(image=this_image,
                         photfile=tfile['mag_best'],
                         pstfile=tfile['pst_in'],
                         psfimage=tfile['psf_4.fits'],
                         opstfile=tfile['pst_out2'],
                         groupfile=tfile['psg'],
                         varorder=-1)



        psf_ap=iraf.photpars.apertures
	ap1=int(psf_ap)
        ap2=int(4.0*opt.fwhm)
        apcor="INDEF"
        aperr="INDEF"
        if (0) :
        #try
        ### now that we have the psf use the output list of psf stars
        ### to compute the aperature correction
            lines=iraf.txdump(tfile['pst_out'],
                              'xcen,ycen,mag,id',
                              iraf.yes,Stdout=tfile['coo_pst'])
            

            ## set the lower ap value for the COG (normally set to 2)
            if ( ap1 < 3 ) :
                smallap=1
            else:
                smallap=2-ap1+1
                ap1=2
            
            ap2=int(math.floor(4.0*opt.fwhm))
            naperts=ap2-ap1+1

            iraf.photpars.apertures=str(ap1)+":"+str(ap2)+":1"
            iraf.photpars.saveParList()
            iraf.daophot.phot(image=this_image,
                              coords=tfile['coo_pst'],
                              output=tfile['mag_pst'])
	    iraf.photcal()
            iraf.photcal.mkapfile(tfile['mag_pst'],
                                  naperts=naperts,
                                  apercors=tfile['apcor'],
                                  smallap=smallap,
                                  verify='no',
                                  gcommands='',
                                  interactive=0)
            fin=open(tfile['apcor'],'r')
            lines=fin.readlines()
            values=lines[2].split()
            apcor=values[1]
            aperr=values[2]
        #except:
            
        



        ## compute the FWHM of the PSF image using the analytic PSF (VarOrd=-1)
        psf_file=pyfits.open(tfile['psf_4.fits'])

        fwhm = (psf_file[0].header.get('PAR1',99.0)+
                              psf_file[0].header.get('PAR2',99.0))
        psf_file.close()
    #   ## Open the psf.fits
        infits=pyfits.open(tfile['psf_final.fits'])
        hdu = infits[0]
        inhdu=hdu.header    
        inhdu.update('XTENSION','IMAGE',before='SIMPLE')
        inhdu.update('PCOUNT',0,after='NAXIS2')
        inhdu.update('GCOUNT',1,after='PCOUNT')
        del hdu.header['SIMPLE']
        del hdu.header['EXTEND']
        inhdu.update("EXTNAME",extname,comment="image extension identifier")
        #inhdu.update("SLOW",slow,comment="SROUND low cutoff")
        #inhdu.update("SIGH",sigh,comment="SROUND high cutoff")
        inhdu.update("PFWHM",fwhm,comment="FWHM of stars based on PSF fitting")
        inhdu.update("ZMAG",zero_mag,comment="ZMAG of PSF ")
        inhdu.update("BCKG",skyvalue,comment="Mean sky level in counts")
        inhdu.update("BCKG_STD",sigma,comment="standard deviation of sky in counts")
        inhdu.update("AP1",psf_ap,comment="Apperture used for PSF flux")
        inhdu.update("AP2",ap2,comment="Full Flux aperture")
        inhdu.update("APCOR",apcor,comment="Apperture correction (ap1->ap2)")
        inhdu.update("APERR",apcor,comment="Uncertainty in APCOR")

    #    ### append this psf to the output images....
        print "Sticking this PSF onto the output file"
        f[current_ext].header.update("PFWHM",fwhm,comment="FWHM of stars based on PSF fitting")
        f[current_ext].header.update("BCKG",skyvalue,comment="Mean sky level in counts")
        f[current_ext].header.update("BCKG_STD",sigma,comment="Standard deviation of sky in counts")
        f.flush()
        outfits.append(hdu)
        outfits.flush()
        infits.close()

        ### remove the temp file we used for this computation.
        for tf in tfile.keys():
            if os.access(tfile[tf],os.F_OK):
                os.unlink(tfile[tf])

        current_ext = current_ext+1



    outfits.close()
    return mef_psf

### set the version flag for this program
__Version__ = "$Revision: 1.1 $"
__Id__ = "$Id: ctio_mkpsf.py,v 1.1 2005/01/16 04:56:55 observe Exp $"

if __name__ == '__main__':

    ### Import some standard python.
    import os, sys, string, tempfile, shutil
    import optik
    from optik import OptionParser

    ### parse the commandline arguements
    parser = OptionParser()
    parser.add_option("-f","--file",
                      action="store", type="string", dest="filename",
                      help="create PSF for FILE", metavar="FILE")
    parser.add_option("-e","--expnum",
                      action="store", type="string", dest="expnum",
                      help="exposure to get from AD",
                      metavar="EXPNUM")
    parser.add_option("-u","--update",
                      action="store_true", dest="update",
                      default=0,
                      help="Update AD with new file? (before possible removal)")
    parser.add_option("-r","--remove",
                      action="store_true", dest="remove",
                      default=0,
                      help="Remove file when done updating?")
    parser.add_option("-s","--silent",
                      action="store_false", dest="silent",
                      default=1,
                      help="Run tasks silently, no output except errors")
    parser.add_option("-w","--fwhm",
                      dest="fwhm", default=4,type="float",
                      help="estimate of FWHM of stellar profiles in FILE [pixels]")
    parser.add_option("-z","--zeropt",
                      dest="zpt", default=25,
                      help="photometric zeropoint of FILE [mag]")
    parser.add_option("-q","--quiet",
                      action="store_false", dest="verbose", default=1,
                      help="run makepsf/iraf quietly?")
    parser.add_option("-i","--interactive",
                      action="store_false", dest="inter", default=0,
                      help="prompt for important parameters?")

    (opt,args)=parser.parse_args()


    ### Get the IRAF/FITS stuff for python
    import cfh12kFits

    no=0
    yes=1
    ### Get the name of the image (from the commandline)
    ### and exit if it doesn't exist

    if opt.expnum:
        opt.expnum=str(opt.expnum)+"p"
        opt.filename=opt.expnum+".fits"
        status=os.system("acp -s -a CFHT -curl -d -o "+opt.filename+" "+opt.expnum)
        if status!=0:
            sys.exit("Failed to get "+opt.expnum+" from AD")
    elif opt.filename:
        import re
        mm=re.match(r'(^[\w]+?)\..*',opt.filename)
        if mm:
            opt.expnum=mm.group(1)
        else:
            sys.exit("Bad filename: Expected CFHT style filename. \n Cann't determine expnum from string "+opt.filename)
    else:
        parser.print_help()
        sys.exit("\n \n \n You must give either a filename or exposure number");


    ### get a handle for the image
    f = cfh12kFits._open(opt.filename,'update')


    import os
    os.environ['SEX_CONFIG']="/home/cadc/kavelaar/12kproc/config/"

    mef_psf=build(f)

    ### update the IQ keywords
    #import iq_calc
    #iq_calc.calc_iq_ratio(f)
    #f.close()

    if opt.update:
        import cfh12kFits
        if opt.silent:
            print "Updating AD"
        cfh12kFits.adPut(opt.filename,'12K-processed')
        cfh12kFits.adPut(mef_psf,'12K-processed')
    else:
        if opt.silent:
            print "Not updating AD"

    if opt.remove:
        os.unlink(mef_psf)
        os.unlink(opt.filename)
