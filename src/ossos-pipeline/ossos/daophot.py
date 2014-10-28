from ossos.gui import logger

__author__ = "David Rusk <drusk@uvic.ca>"

import os
import re
import tempfile

from pyraf import iraf
from astropy.io import fits


class TaskError(Exception):
    """Base task error"""


def phot(fits_filename, x_in, y_in, aperture=15, sky=20, swidth=10, apcor=0.3,
         maxcount=30000.0, exptime=1.0, zmag=None):
    """
    Compute the centroids and magnitudes of a bunch sources detected on
    CFHT-MEGAPRIME images.

    Args:
      fits_filename: str
        The name of the file containing the image to be processed.

    Returns a MOPfiles data structure.
    """

    if (not os.path.exists(fits_filename) and
            not fits_filename.endswith(".fits")):
        # For convenience, see if we just forgot to provide the extension
        fits_filename += ".fits"

    try:
        input_hdulist = fits.open(fits_filename)
    except Exception as err:
        logger.debug(str(err))
        raise TaskError("Failed to open input image: %s" % err.message)

    ## get the filter for this image
    filter = input_hdulist[0].header.get('FILTER', 'DEFAULT')

    ### Some nominal CFHT zeropoints that might be useful
    zeropoints = {"I": 25.77,
                  "R": 26.07,
                  "V": 26.07,
                  "B": 25.92,
                  "DEFAULT": 26.0,
                  "g.MP9401": 26.4
    }

    photzp = input_hdulist[0].header.get('PHOTZP', zeropoints.get(filter, zeropoints["DEFAULT"]))
    if zmag is None:
        zmag = input_hdulist[0].header.get('PHOTZP', zeropoints[filter])

        ### check for magic 'zeropoint.used' files
        zpu_file = "zeropoint.used"
        if os.access(zpu_file, os.R_OK):
            with open(zpu_file) as zpu_fh:
                zmag = float(zpu_fh.read())
        else:
            zpu_file = "%s.zeropoint.used" % ( fits_filename[0:-5])
            if os.access(zpu_file, os.R_OK):
                with open(zpu_file) as zpu_fh:
                    zmag = float(zpu_fh.read())

    if zmag != photzp:
        logger.warning("ZEROPOINT {} used in DAOPHOT doesn't match PHOTZP {} in header".format(zmag, photzp))

    ### setup IRAF to do the magnitude/centroid measurements
    iraf.set(uparm="./")
    iraf.digiphot()
    iraf.apphot()
    iraf.daophot(_doprint=0)



    iraf.photpars.apertures = aperture
    iraf.photpars.zmag = zmag
    iraf.datapars.datamin = 0
    iraf.datapars.datamax = maxcount
    iraf.datapars.exposur = ""
    iraf.datapars.itime = exptime
    iraf.fitskypars.annulus = sky
    iraf.fitskypars.dannulus = swidth
    iraf.fitskypars.salgorithm = "mode"
    iraf.fitskypars.sloclip = 5.0
    iraf.fitskypars.shiclip = 5.0
    iraf.centerpars.calgori = "centroid"
    iraf.centerpars.cbox = 5.
    iraf.centerpars.cthreshold = 0.
    iraf.centerpars.maxshift = 2.
    iraf.centerpars.clean = 'no'
    iraf.phot.update = 'no'
    iraf.phot.verbose = 'no'
    iraf.phot.verify = 'no'
    iraf.phot.interactive = 'no'

    # Used for passing the input coordinates
    coofile = tempfile.NamedTemporaryFile(suffix=".coo", delete=False)
    coofile.write("%f %f \n" % (x_in, y_in))

    # Used for receiving the results of the task
    # mag_fd, mag_path = tempfile.mkstemp(suffix=".mag")
    magfile = tempfile.NamedTemporaryFile(suffix=".mag", delete=False)

    # Close the temp files before sending to IRAF due to docstring:
    # "Whether the name can be used to open the file a second time, while
    # the named temporary file is still open, varies across platforms"
    coofile.close()
    magfile.close()
    os.remove(magfile.name)

    iraf.phot(fits_filename, coofile.name, magfile.name)

    # TODO: Move this filtering downstream to the user.
    phot_filter = "PIER==0 && CIER==0 && SIER==0"

    pdump_out = iraf.pdump(magfile.name, "XCENTER,YCENTER,MAG,MERR,ID,XSHIFT,YSHIFT,LID",
                           phot_filter, header='no', parameters='yes',
                           Stdout=1)

    if not len(pdump_out) > 0:
        mag_content = open(magfile.name).read()
        raise TaskError("photometry failed. {}".format(mag_content))

    os.remove(coofile.name)
    os.remove(magfile.name)

    ### setup the mop output file structure
    hdu = {}
    hdu['header'] = {'image': input_hdulist,
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

    # Clean up temporary files generated by IRAF
    os.remove("datistabe.par")
    os.remove("datpdump.par")

    return hdu


def phot_mag(*args, **kwargs):
    """Wrapper around phot which only returns the computed magnitude directly."""
    hdu = phot(*args, **kwargs)

    try:
        return hdu["data"]["X"][0], hdu["data"]["Y"][0], hdu["data"]["MAG"][0], hdu["data"]["MERR"][0],
    except IndexError:
        raise TaskError("Photometry cannot be performed.  "+str(hdu))

