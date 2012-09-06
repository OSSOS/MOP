#!/usr/bin/env python
""" Plant artificial profiles into an image given a list of x/y coordinates
    and rates of motion"""

def plant(image,psf,outfile,list,dtime):
    import pyfits,os 
    import numarray as N
    psf_f=pyfits.open(psf)
    psf_flux=psf_f[0].data.sum()
    psf_x_size=psf_f[0].header.get('NAXIS1',0)
    psf_y_size=psf_f[0].header.get('NAXIS2',0)
    psf_x=psf_f[0].header.get('PSF_X',0)
    psf_y=psf_f[0].header.get('PSF_Y',0)
    psf_mag=psf_f[0].header.get('PSFMAG',26.0)
    image_f=pyfits.open(image)
    xmax=image_f[0].header.get('NAXIS1',0)
    ymax=image_f[0].header.get('NAXIS2',0)
    exptime=image_f[0].header.get('EXPTIME',1)
    zeropoint=image_f[0].header.get('PHOT_C',26.5)

    import mop_files
    ahdu=mop_files.read(list)
    import string,math,re
    from numarray.nd_image.interpolation import shift as shift

    from string import atof
    for i in range(len(ahdu['data']['x'])):
        x=float(ahdu['data']['x'][i])
        y=float(ahdu['data']['y'][i])
        mag=float(ahdu['data']['mag'][i])
        rate=float(ahdu['data']['pix_rate'][i])/3600.0
        angle=float(ahdu['data']['angle'][i])
        x_shift_rate=rate*math.cos(angle/57.3)
        y_shift_rate=rate*math.sin(angle/57.3)
        #flux=exptime*10**((zeropoint-mag)/2.5)
        #scale=flux/psf_flux
        scale=10**((psf_mag-mag)/2.5)*exptime
        #print scale
        niter=int(rate*exptime)+1
        scale=scale/niter
        dt = exptime/niter
        #print x,y,mag,niter
        for i in range(niter):
            curtime = dtime+dt*i
            x=x+x_shift_rate*curtime
            y=y+y_shift_rate*curtime
            x1=int(max(0,x-psf_x))
            x2=int(min(xmax,x+psf_x_size-psf_x))
            y1=int(max(0,y-psf_y))
            y2=int(min(ymax,y+psf_y_size-psf_y))
            #print x2,x1,y2,y1
            px1=int((psf_x-(x-x1)))
            px2=int(px1+(x2-x1))
            py1=int(psf_y-(y-y1))
            py2=int(py1+(y2-y1))
            sec = psf_f[0].data[py1:py2,px1:px2].copy()
            sec = shift(sec,(y-int(y),x-int(x)),order=3)
            #print sec.shape,y2-y1,x2-x1
            #print "Adding @ ",x,y,mag,scale," data=> ",y1,y2,x1,x2," PSF=> ",py1,py2,px1,px2
            
            image_f[0].data[y1:y2,x1:x2]+=scale*sec

    image_f.writeto(outfile)
    image_f.close()
    
import optparse, sys
from optparse import OptionParser

parser=OptionParser()
parser.add_option("--verbose","-v",
                  action="store_true",
                  dest="verbose",
                  help="Provide feedback on what I'm doing")
parser.add_option("--suffix",
                  action="store",
                  dest="suffix",
                  default="art",
                  help="Suffix for images with stars added")
parser.add_option("--art",
                  default="Object.planted",
                  help="x/y/mag/rate list of artificial objects")
parser.add_option("--psf",
                  default="psf",
                  help="Suffix of MOPpsf format file")

(opt,argv)=parser.parse_args()

import os,pyfits
images=[]
start_date=None;
for file in argv:
    if not os.access(file,os.R_OK):
        sys.exit("couldn't open "+file)
    image={'file': file}
    base=os.path.splitext(file)
    image['base']=base[0]
    f=pyfits.open(image['base']+".mopheader")
    image['mjdate']=f[0].header.get('MJD-OBSC',0)
    if not start_date or image['mjdate']<start_date:
        start_date=image['mjdate']
    images.append(image)
    f.close()
                   
for image in images:
    psf = image['base']+'.'+opt.psf+'.fits'
    outfile=image['base']+'.'+opt.suffix+'.fits'
    plant(image['file'],psf,outfile,opt.art,(image['mjdate']-start_date)*3600.0*24.0)
          
                  
                  
