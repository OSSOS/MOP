#!/usr/bin/python

"""Compute the centroids and magnitudes of an input
list of stars and a fits image.

This script is designed to work with MEGAPRIME images from CFHT"""

author="JJ Kavelaars"
version=1.0

import sys
import optik
from optik import OptionParser


if __name__ =='__main__':
	
	parser=OptionParser()
	
	parser.add_option('--image',action='store',
			  help='image to compute centroids of stars')
	parser.add_option('--input',action='store',
			  help='coo file [in format approriate for iraf.digiphot.daophot.phot coo] ')
	parser.add_option('--output',action='store',
			  help='file with output star coordinates [X,Y,MAG,MERR,ID]')
	parser.add_option('--aperture',action='store',
			  help='phot aperture to use', default=15)
	
	(opt, args)=parser.parse_args()
	
	if not (opt.image and opt.input and opt.output ):
		parser.print_help()
		sys.exit(0)

def centroid(image,input,output):
	"""Compute the centroids and magnitudes of a bunch sources detected on CFHT-MEGAPRIME images"""
print "Centroiding stars"
print "IMAGE: %s " % (opt.image,)
print "COORDINATES: %s " % ( opt.input,)
print "RESULTS:  %s" % ( opt.output, ) 

from pyraf import iraf
from pyraf.irafpar import IrafParList
import pyfits

### make sure we have a chance of success at least.
import os
if not os.access(opt.input,os.R_OK):
    sys.exit("Cann't access in the input star list "+opt.input)
try:
	f=pyfits.open(opt.image)
except:
	sys.exit("Failed to open input image\n")

### overide the default malin if header keyword exists
maxlin=f[0].header.get('MAXLIN',64000)

## get the filter for this image
filter=f[0].header.get('FILTER','DEFAULT')

### Some CFHT zeropoints that might be useful
zeropoints={"I": 25.77,
	    "R": 26.07,
	    "V": 26.07,
	    "B": 25.92,
	    "DEFAULT": 26.0,
	    "g.MP9401": 26.4
	    }

### load the 
if not zeropoints.has_key(filter):
    filter="DEFAULT"
zmag=f[0].header.get('PHOT_C',zeropoints[filter])
f.close()

### setup IRAF to do the magnitude/centroid measurements
iraf.set(uparm="./")
iraf.digiphot()
iraf.apphot()
iraf.daophot()

### check for the majical 'zeropoint.used' file
import os
if os.access('zeropoint.used',os.R_OK):
	f=file('zeropoint.used')
	zmag=float(f.read())

if os.access(opt.output,os.R_OK):
    os.unlink(opt.output)

iraf.photpars.apertures=int(opt.aperture)
iraf.photpars.zmag=zmag
iraf.datapars.datamin=0
iraf.datapars.datamax=maxlin
iraf.datapars.exposur="EXPTIME"
iraf.fitskypars.annulus=int(opt.aperture)+5
iraf.fitskypars.dannulus=5
iraf.centerpars.calgori="centroid"
iraf.centerpars.cbox=5.
iraf.centerpars.cthreshold=0.
iraf.centerpars.maxshift=2.
iraf.centerpars.clean='no'
iraf.phot.update='no'
iraf.phot.verbose='no'
iraf.phot.verify='no'
iraf.phot.interactive='no'
import tempfile
magfile=tempfile.mktemp(suffix='mag')
print(magfile)
iraf.phot(opt.image, opt.input, magfile)
iraf.pdump(magfile,"XCENTER,YCENTER,MAG,MERR,ID","MERR < 0.1 && MAG != INDEF && PIER==0", header='no', parameters='yes',  Stdout=opt.output)

os.unlink(magfile)
