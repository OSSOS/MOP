#!/usr/cadc/misc/bin/python
#/*+
#************************************************************************
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#*
#* (c) <year>.				(c) <year>.
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
#*   Script Name:	daophot.py
#*
#*   Purpose:
#*	run daophot on an image.
#*
#*
#*   Date		: May 17, 2006
#*
#*   RCS data:
#*	$RCSfile$
#*	$Revision$
#*	$Date$
#*
#*   Programmer		: JJ Kavelaars
#*
#*   Modification History:
#*
#****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
#************************************************************************
#-*/
"""Run DAOPHOT [using pyIRAF] using the image and source list provided.

This script is designed to work with MEGAPRIME images from CFHT"""

author = "JJ Kavelaars"
version = "$Revision$"

import sys, os
from myTaskError import TaskError
from optparse import OptionParser


def phot(image, input='DEFAULT', aperture=15, sky=20, swidth=10, apcor=0.3):
    """Compute the centroids and magnitudes of a bunch sources detected on CFHT-MEGAPRIME images.

    Returns a MOPfiles data structure."""

    from pyraf import iraf
    from pyraf.irafpar import IrafParList
    import pyfits
    from myTaskError import TaskError

    ### make sure we have a chance of success at least.
    import os

    fits_file = image
    if not os.access(fits_file, os.R_OK):
        fits_file += '.fits'
    try:
        f = pyfits.open(fits_file)
    except:
        raise TaskError, 'Failed to open input image'
        ### overide the default malin if header keyword exists
    maxlin = f[0].header.get('MAXLIN', 64000)

    ## get the filter for this image
    filter = f[0].header.get('FILTER', 'DEFAULT')

    ### Some CFHT zeropoints that might be useful
    zeropoints = {"I": 25.77,
                  "R": 26.07,
                  "V": 26.07,
                  "B": 25.92,
                  "DEFAULT": 26.0,
                  "g.MP9401": 26.4
    }

    ### load the
    if not zeropoints.has_key(filter):
        filter = "DEFAULT"
    zmag = f[0].header.get('PHOTZP', zeropoints[filter])
    f.close()

    ### setup IRAF to do the magnitude/centroid measurements
    iraf.set(uparm="./")
    iraf.digiphot()
    iraf.apphot()
    iraf.daophot()

    ### check for the magical 'zeropoint.used' file
    import os

    if os.access('zeropoint.used', os.R_OK):
        f = file('zeropoint.used')
        zmag = float(f.read())

    iraf.photpars.apertures = int(aperture)
    iraf.photpars.zmag = zmag
    iraf.datapars.datamin = 0
    iraf.datapars.datamax = maxlin
    #iraf.datapars.exposur="EXPTIME"
    iraf.datapars.exposur = ""
    iraf.datapars.itime = 1
    iraf.fitskypars.annulus = sky
    iraf.fitskypars.dannulus = swidth
    iraf.centerpars.calgori = "centroid"
    iraf.centerpars.cbox = 5.
    iraf.centerpars.cthreshold = 0.
    iraf.centerpars.maxshift = 2.
    iraf.centerpars.clean = 'no'
    iraf.phot.update = 'no'
    iraf.phot.verbose = 'no'
    iraf.phot.verify = 'no'
    iraf.phot.interactive = 'no'
    import tempfile

    magfile = tempfile.mktemp(suffix='mag')
    iraf.phot(image, input, magfile)
    pdump_out = iraf.pdump(magfile, "XCENTER,YCENTER,MAG,MERR,ID,XSHIFT,YSHIFT,LID",
                           "MERR < 0.4 && MERR != INDEF && MAG != INDEF && PIER==0", header='no', parameters='yes',
                           Stdout=1)
    os.unlink(magfile)

    ### setup the mop output file structure
    hdu = {}
    hdu['header'] = {'image': image,
                     'aper': aperture,
                     's_aper': sky,
                     'd_s_aper': swidth,
                     'aper_cor': apcor,
                     'zeropoint': zmag}
    hdu['order'] = ['X', 'Y', 'MAG', 'MERR', 'ID', 'XSHIFT', 'YSHIFT', 'LID']
    hdu['format'] = {'X': '%10.2f',
                     'Y': '%10.2f',
                     'MAG': '%10.2f',
                     'MERR': '%10.2f',
                     'ID': '%8d',
                     'XSHIFT': '%10.2f',
                     'YSHIFT': '%10.2f',
                     'LID': '%8d'}
    hdu['data'] = {}
    for col in hdu['order']:
        hdu['data'][col] = []

    import re

    for line in pdump_out:
        values = line.split()
        for col in hdu['order']:
            if re.match('\%.*f', hdu['format'][col]):
                if col == 'MAG':
                    values[0] = float(values[0]) - float(apcor)
                hdu['data'][col].append(float(values.pop(0)))
            elif re.match('\%.*d', hdu['format'][col]):
                hdu['data'][col].append(int(values.pop(0)))
            else:
                hdu['data'][col].append(values.pop(0))

    return hdu


if __name__ == '__main__':

    parser = OptionParser()

    #parser.add_option('--image',action='store',
    #		  help='image to compute centroids of stars')
    parser.add_option('--input', action='store', default='DEFAULT',
                      help='coo file [in format approriate for iraf.digiphot.daophot.phot coo] ')
    parser.add_option('--output', action='store', default='DEFAULT',
                      help='file with output star coordinates [X,Y,MAG,MERR,ID]')
    parser.add_option('--aperture', action='store', default=15,
                      help='phot aperture to use')
    parser.add_option('--sky', action='store', default=20, help='Inner sky annulus')
    parser.add_option('--swidth', action='store', default=10, help='Width of sky annulus')
    parser.add_option('--apcor', action='store', default=0.3,
                      help='Aperture correction to get from input aperture to reference system')
    parser.set_usage(usage='%prog [options] image[.fits]')
    (opt, args) = parser.parse_args()

    if len(args) != 1:
        parser.error("No input image specified.")
    opt.image = args[0]
    if not os.access(opt.image, os.R_OK) and not os.access(opt.image + '.fits', os.R_OK):
        parser.error("Failed trying to access: %s[.fits]" % (opt.image))

    import MOPfiles


    obj = MOPfiles.read(opt.input)
    for i in range(len(obj['data']['X'])):
        print obj['data']['X'][i]
    print "NEXT"

    hdu = phot(opt.image, opt.input, opt.aperture, opt.sky, opt.swidth, opt.apcor)

    obj['data']['XSHIFT'] = range(len(obj['data']['X']))
    obj['data']['YSHIFT'] = range(len(obj['data']['X']))
    obj['data']['MAG'] = range(len(obj['data']['X']))
    obj['data']['MERR'] = range(len(obj['data']['X']))
    obj['data']['DMAG'] = range(len(obj['data']['X']))
    obj['order'].append('XSHIFT')
    obj['order'].append('YSHIFT')
    obj['order'].append('MAG')
    obj['order'].append('MERR')
    obj['order'].append('DMAG')
    obj['format']['XSHIFT'] = hdu['format']['X']
    obj['format']['YSHIFT'] = hdu['format']['X']
    obj['format']['MAG'] = hdu['format']['MAG']
    obj['format']['MERR'] = hdu['format']['MERR']
    obj['format']['DMAG'] = hdu['format']['MERR']
    for keyword in hdu['header'].keys():
        obj['header'][keyword] = hdu['header'][keyword]



    ### add the MAG/MERR columns for the input catalog, use the DAOPHOT centroids.
    ### remove those sources with large centroid shifts
    ### remove those sources with that DAOPHOT returned INDEF for
    ## #del_list is an list of array indexies to remove from the obj data
    del_list = []
    for i in range(len(obj['data']['X'])):
        found = False
        for j in range(len(hdu['data']['X'])):
            if ((float(obj['data']['X'][i]) + float(hdu['data']['XSHIFT'][j]) - float(hdu['data']['X'][j])) ** 2 + (
                        float(obj['data']['Y'][i]) + float(hdu['data']['YSHIFT'][j]) - float(
                        hdu['data']['Y'][j])) ** 2) < 1:
                if found:
                    del_list.append(i)
                    continue
                found = True
                if float(hdu['data']['XSHIFT'][j]) ** 2 + float(hdu['data']['YSHIFT'][j]) ** 2 < 9.0:
                    for col in ['X', 'Y', 'MAG', 'MERR', 'XSHIFT', 'YSHIFT']:
                        obj['data'][col][i] = float(hdu['data'][col][j])
                    obj['data']['DMAG'][i] = float(obj['data']['PMAG'][i]) - obj['data']['MAG'][i]
                else:
                    del_list.append(i)
        if not found:
            del_list.append(i)
    del_list.sort()
    del_list.reverse()
    for i in del_list:
        print obj['data']['X'][i]
        for col in obj['data'].keys():
            del obj['data'][col][i]

    if opt.output == 'DEFAULT':
        n = 1
        while (os.access(opt.image + '.mag.' + str(n), os.R_OK)):
            n += 1
        opt.output = opt.image + '.mag.' + str(n)

    MOPfiles.write(opt.output, obj)

