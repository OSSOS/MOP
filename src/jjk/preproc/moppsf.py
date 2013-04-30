#!/usr/bin/env python
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
#* (c) 2004.				(c) 2004.
#* National Research Council		Conseil national de recherches
#* Ottawa, Canada, K1A 0R6 		Ottawa, Canada, K1A 0R6
#* All rights reserved			Tous droits reserves
#* 					
#* NRC disclaims any warranties,	Le CNRC denie toute garantie
#* expressed, implied, or statu-	enoncee, implicite ou legale,
#* tory, of any kind with respect	de quelque nature que se soit,
#* to the software, including		concernant le logiciel, y com-
#* without limitation any war-		pris sans restriction toute
#* ranty of merchantability or		garantie de valeur marchande
#* fitness for a particular pur-	ou de pertinence pour un usage
#* pose.  NRC shall not be liable	particulier.  Le CNRC ne
#* in any event for any damages,	pourra en aucun cas etre tenu
#* whether direct or indirect,		responsable de tout dommage,
#* special or general, consequen-	direct ou indirect, particul-
#* tial or incidental, arising		ier ou general, accessoire ou
#* from the use of the software.	fortuit, resultant de l'utili-
#* 					sation du logiciel.
#*
#************************************************************************
#*
#*   Script Name:	moppsf.py
#*
#*   Purpose:
#* ### a quick approach to creating a PSF for bright stars.
#* ### a) get stamps near bright stars
#* ### b) create medians of the stack
#* ### c) do variance rejection and rebuild the median
#* ### done.
#************************************************************************
#*
#*   Functions:
#*
#*
#*   CVS data:
#*	$Header: /home/observe/cvsroot/MOP/src/jjk/preproc/MOPpsf.py,v 1.1 2005/05/10 21:03:12 observe Exp $
#*
#*   Initiated by       : JJ Kavelaars
#*   Date		: <Dec 14 2004>
#*
#*   Modification History:
#*   $Log $
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/
"""Create a fits file that contains a representation of a stellar PSF"""


__Version__ = "$Revision: 1.1 $"
import re
version=re.match(r'\$Rev.*: (\d*.\d*) \$',__Version__).group(1)


def iraf_phot(f,x,y,zmag=26.5,apin=10,skyin=15,skywidth=10):
    """Compute the magnitude of the star at location x/y"""
    import pyfits
    import re
    infits=pyfits.open(f,'update')
    f=re.sub(r'.fits$','',f)
    
    ### Get my python routines
    from pyraf import iraf
    from pyraf.irafpar import IrafParList

    ### keep all the parameters locally cached.
    iraf.set(uparm="./")
    iraf.set(imtype="fits")


    ### Load the required IRAF packages
    iraf.digiphot()
    iraf.apphot()
    iraf.daophot()
    
    ### temp file name hash.
    tfile={}
    
    iraf.datapars.datamax=60000
    iraf.datapars.datamin=-1000
    iraf.datapars.airmass='AIRMASS'
    iraf.datapars.filter='FILTER'
    iraf.datapars.obstime='TIME-OBS'
    iraf.datapars.exposure='EXPTIME'
    iraf.datapars.gain='GAIN'
    iraf.datapars.ccdread='RDNOISE'
    iraf.datapars.fwhmpsf=5.0
    
    iraf.centerpars.calgorithm='centroid'
    iraf.photpars.zmag=zmag
    iraf.photpars.apertures=apin
    iraf.fitskypars.annulus=skyin
    iraf.fitskypars.dannulus=skywidth
    iraf.daophot.verbose=iraf.no
    iraf.daophot.verify=iraf.no
    iraf.daophot.update=iraf.no
    
    iraf.psf.interactive=iraf.no
    iraf.pstselect.interactive=iraf.no
    
    iraf.datapars.saveParList()
    iraf.fitskypars.saveParList()
    iraf.centerpars.saveParList()
    iraf.findpars.saveParList()
    iraf.photpars.saveParList()
    
    tfiles = ['coo','mag']
    

    
    for file in tfiles:
        extname=f
        tfile[file]=extname+"."+file
        if ( os.access(tfile[file],os.F_OK) ):
            os.unlink(tfile[file])
            
    this_image=f
        
    fd = open(tfile['coo'],'w')
    fd.write('%f %f\n' % ( x, y) )
    fd.close()
    
    print "Measuring photometry psf star in "+tfile['coo']
    iraf.daophot.phot(image=this_image,
                      coords=tfile['coo'],
                      output=tfile['mag'])

    import string
    a=iraf.txdump(tfile['mag'],"MAG,XCEN,YCEN",iraf.yes,Stdout=1)
    (mag,x,y)=string.split(a[0])

    inhdu=infits[0].header
    
    inhdu.update("PSFMAG",float(mag),comment="PSF Magnitude")
    inhdu.update("PSF_X",float(x),comment="PSF Magnitude")
    inhdu.update("PSF_Y",float(y),comment="PSF Magnitude")
    inhdu.update("ZMAG",zmag,comment="ZMAG of PSF ")

    ## now measure using a smaller aperture to get aper correction
    iraf.photpars.apertures=apin*3.0
    os.unlink(tfile['mag'])
    iraf.daophot.phot(image=this_image,
                      coords=tfile['coo'],
                      output=tfile['mag'])
    a=iraf.txdump(tfile['mag'],"MAG,XCEN,YCEN",iraf.yes,Stdout=1)
    (magout,x,y)=string.split(a[0])
    inhdu.update("APCOR",float(magout)-float(mag),comment="AP_OUT - AP_IN")
    inhdu.update("AP_IN",apin*3.0,comment="Small aperature")
    inhdu.update("AP_OUT",apin,comment="Large aperture")

    
    #    ### append this psf to the output images....
    
    infits.close()
    
    ### remove the temp file we used for this computation.
    #for tf in tfile.keys():
    #    if os.access(tfile[tf],os.F_OK):
    #        os.unlink(tfile[tf])
    return 0



if __name__=='__main__':
    import MOPfits
    import pyfits

    ### Must be running as a script
    import optparse, sys
    from optparse import OptionParser
    
    parser=OptionParser()
    parser.add_option("--verbose","-v",
                      action="store_true",
                      dest="verbose",
                      help="Provide feedback on what I'm doing")
    parser.add_option("--outfile",
                      action="store",
                      type="string",
                      dest="outfile",
                      help="name for output psf file")
    parser.add_option("--iq",
                      action="store",
                      type="float",
                      help="Image Quality in pixels")
    parser.add_option("--image",
                      action="store", type="string", dest="image",
                      help="input image to build PSF for")
    parser.add_option("--xbox",
                      action="store",
                      type="int",
                      dest="xbox",
                      default=40,
                      help="Size of stellar profile in x direction")
    parser.add_option("--ybox",
                      action="store",
                      type="int",
                      dest="ybox",
                      default=40,
                      help="Size of stellar profiel in y direction")
    parser.add_option("--stars",
                      action="store",
                      dest="stars",
                      help="file with x/y locations of bright isolated stars")
                      
    (opt, file_ids)=parser.parse_args()



    ### import import import
    import os,re, string
    import numarray as N
    from numarray.ma  import *
    import numarray.image.combine as nic
    from string import lstrip
    from numarray.nd_image.interpolation import shift as shift
    from numarray.nd_image import label
    from math import floor

    ### get the list of psf stars.
    ### expected to be step0jmp bright.psf format.
    if not os.access(opt.stars,os.R_OK):
        sys.exit("failed trying to open input star list: "+opt.stars)
    import mop_files
    psf_stars=mop_files.read(opt.stars)



    ### open the output psf file... must not exist at start
    outfile=opt.outfile
    if os.access(outfile,os.W_OK):
        sys.exit("Output file "+outfile+" already exists")

    if opt.verbose:
        print "Creating output image "+outfile

    ## fitsobj is a list of fits HDUs.
    fitsobj = pyfits.HDUList()

    ### s is the 'structure' what we call linked objects.
    ### this is used in the label routine.
    s=[[1,1,1],[1,1,1],[1,1,1]]

    ### stack will become an array of image data chunks cut out
    ### around the x/y locations listed in opt.stars file
    stack=[]

    ### load the input image.
    hdu=pyfits.open(opt.image)
    rdnoise=hdu[0].header.get('RDNOISE',5)
    gain=hdu[0].header.get('GAIN',1.5)
    filter=hdu[0].header.get('FILTER','r')

    timeobs=hdu[0].header.get('TIME-OBS','00:00:00')
    
    data=hdu[0].data
    xbox=opt.xbox
    ybox=opt.ybox

    flux=[]
    slices=[]
    for index in range(len(psf_stars['data']['X'])):
        x=float(psf_stars['data']['X'][index])
        y=float(psf_stars['data']['Y'][index])

        if x+xbox > data.getshape()[1] or x-xbox < 0 or y+ybox > data.getshape()[0] or y-ybox < 0:
            continue
        l=int(x-xbox)
        r=int(x+xbox)
        t=int(y-ybox)
        b=int(y+ybox)
        sec = data[t:b,l:r].copy()
        sec = shift(sec,(x-int(x),y-int(y)),order=3)
        obj =  N.where(sec > 2.0*average(average(sec)),1,0)
        sky2 = N.where(sec < 2.0*average(average(sec)),1,0)
        sky2 = N.sum(N.sum(sky2*sec))/N.sum(N.sum(sky2))

        (lab, nobj) = label(obj,structure=s)
        f = N.nd_image.find_objects(lab)
        skip=0
        
        msec = masked_outside(sec,sky2-5.0*sqrt(sky2),40000.)
        for i in range(1,nobj+1):
            (a,b)=shape(obj[f[i-1]])
            a*=1.
            b*=1.
            if a*b < 10:
                continue
            if a/b < 0.5 or b/a < 0.5 or a>25 or b> 25:
                ### some part of chunk of the image has a wonky shape
                ### better skip this one.
                skip=1
                break
        if skip:
            continue
        ### subtract the sky level to create a scalable data section
        sec = msec - sky2
        ### scale by the flux to combine the different stellar profiles
        flux.append(sum(sum(sec)))
        sec /= flux[len(flux)-1]
        slices.append(sec.filled(fill_value=-1000))

    median=nic.median(slices,nlow=3,nhigh=2)

    new_slices=[]
    for data in slices:
        if fabs(sum(sum(data-median))) < 1:
            new_slices.append(data)
            
    median=nic.median(new_slices)

    hdu=pyfits.PrimaryHDU()
    hdu.header.update('EXPTIME',1,comment="Arbitrary")
    hdu.header.update('TIME-OBS',timeobs,comment='UT Time of observations')
    hdu.header.update('AIRMASS',1,comment="Arbitrary")
    hdu.header.update('RDNOISE',rdnoise,comment="Average of [A,B] amps")
    hdu.header.update('GAIN',gain,comment='Amplifier Gain')
    hdu.header.update('FILTER',filter,comment='filter')
    
    hdu.data=median[2:-2,2:-2]*10000.0
    
    fitsobj.append(hdu)
    fitsobj.writeto(outfile)
    fitsobj.close()
    iraf_phot(outfile,(xbox+1/2.0),(ybox+1/2.0),apin=int(opt.iq*0.9))

            
