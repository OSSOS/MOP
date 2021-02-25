__author__ = "David Rusk <drusk@uvic.ca>"
import logging
import os
import tempfile
import warnings

from .gui import logger

warnings.simplefilter("ignore")
from pyraf import iraf
from astropy.io import fits
from astropy.io import ascii


class TaskError(Exception):
    """Base task error"""


def phot(fits_filename, x_in, y_in, aperture=15, sky=20, swidth=10, apcor=0.3,
         maxcount=30000.0, exptime=1.0, zmag=None, extno=0, centroid=True):
    """
    Compute the centroids and magnitudes of a bunch sources  on fits image.

    :rtype : astropy.table.Table
    :param fits_filename: Name of fits image to measure source photometry on.
    :type fits_filename: str
    :param x_in: x location of source to measure
    :type x_in: float, numpy.array
    :param y_in: y location of source to measure
    :type y_in: float, numpy.array
    :param aperture: radius of circular aperture to use.
    :type aperture: float
    :param sky: radius of inner sky annulus
    :type sky: float
    :param swidth: width of the sky annulus
    :type swidth: float
    :param apcor: Aperture correction to take aperture flux to full flux.
    :type apcor: float
    :param maxcount: maximum linearity in the image.
    :type maxcount: float
    :param exptime: exposure time, relative to zmag supplied
    :type exptime: float
    :param zmag: zeropoint magnitude
    :param extno: extension of fits_filename the x/y location refers to.
    """
    if not hasattr(x_in, '__iter__'):
        x_in = [x_in, ]
    if not hasattr(y_in, '__iter__'):
        y_in = [y_in, ]

    if (not os.path.exists(fits_filename) and
            not fits_filename.endswith(".fits")):
        # For convenience, see if we just forgot to provide the extension
        fits_filename += ".fits"

    try:
        input_hdulist = fits.open(fits_filename)
    except Exception as err:
        logger.error(f'Failed trying to open {fits_filename}')
        logger.error(str(err))
        raise FileNotFoundError(fits_filename)

    # get the filter for this image
    filter_name = input_hdulist[extno].header.get('FILTER',
                                                  input_hdulist[0].header.get('FILTER',
                                                                              'DEFAULT'))

    # Some nominal CFHT zeropoints that might be useful
    zeropoints = {"I": 25.77,
                  "R": 26.07,
                  "V": 26.07,
                  "B": 25.92,
                  "r2": 27.3,
                  "DEFAULT": 26.0,
                  "g.MP9401": 32.0,
                  'r.MP9601': 31.9,
                  'gri.MP9603': 33.520}
    if zmag is None:
        logger.warning("No zmag supplied to daophot, looking for header or default values.")
        zmag = input_hdulist[extno].header.get('PHOTZP', zeropoints[filter_name])
        logger.warning("Setting zmag to: {}".format(zmag))
        # check for magic 'zeropoint.used' files
        for zpu_file in ["{}.zeropoint.used".format(os.path.splitext(fits_filename)[0]), "zeropoint.used"]:
            if os.access(zpu_file, os.R_OK):
                with open(zpu_file) as zpu_fh:
                    zmag = float(zpu_fh.read())
                    logger.warning("Using file {} to set zmag to: {}".format(zpu_file, zmag))
                    break
    photzp = input_hdulist[extno].header.get('PHOTZP', zeropoints.get(filter_name, zeropoints["DEFAULT"]))
    if zmag != photzp:
        logger.warning(("zmag sent to daophot: ({}) "
                        "doesn't match PHOTZP value in image header: ({})".format(zmag, photzp)))

    # setup IRAF to do the magnitude/centroid measurements
    iraf.set(uparm="./")
    iraf.digiphot()
    iraf.apphot()
    iraf.daophot(_doprint=0)

    iraf.photpars.apertures = aperture
    iraf.photpars.zmag = zmag
    iraf.datapars.datamin = -100
    iraf.datapars.datamax = maxcount
    iraf.datapars.exposur = ""
    iraf.datapars.itime = exptime
    iraf.fitskypars.annulus = sky
    iraf.fitskypars.dannulus = swidth
    iraf.fitskypars.salgorithm = "mode"
    iraf.fitskypars.sloclip = 5.0
    iraf.fitskypars.shiclip = 5.0
    if centroid:
       iraf.centerpars.calgori = "centroid"
       iraf.centerpars.cbox = 5.
       iraf.centerpars.cthreshold = 0.
       iraf.centerpars.maxshift = 2.
       iraf.centerpars.clean = 'no'
    else:
       iraf.centerpars.calgori = "none"
    iraf.phot.update = 'no'
    iraf.phot.verbose = 'no'
    iraf.phot.verify = 'no'
    iraf.phot.interactive = 'no'

    # Used for passing the input coordinates
    coofile = tempfile.NamedTemporaryFile(suffix=".coo", delete=False, mode='w')

    for i in range(len(x_in)):
        coofile.write("%f %f \n" % (x_in[i], y_in[i]))
    coofile.flush()
    # Used for receiving the results of the task
    # mag_fd, mag_path = tempfile.mkstemp(suffix=".mag")
    magfile = tempfile.NamedTemporaryFile(suffix=".mag", delete=False, mode='w')

    # Close the temp files before sending to IRAF due to docstring:
    # "Whether the name can be used to open the file a second time, while
    # the named temporary file is still open, varies across platforms"
    coofile.close()
    magfile.close()
    os.remove(magfile.name)

    iraf.phot(fits_filename+"[{}]".format(extno), coofile.name, magfile.name)
    pdump_out = ascii.read(magfile.name, format='daophot')
    logging.debug("PHOT FILE:\n"+str(pdump_out))
    if not len(pdump_out) > 0:
        mag_content = open(magfile.name).read()
        raise TaskError("photometry failed. {}".format(mag_content))

    # apply the aperture correction
    pdump_out['MAG'] -= apcor

    # if pdump_out['PIER'][0] != 0 or pdump_out['SIER'][0] != 0 or pdump_out['CIER'][0] != 0:
    #    raise ValueError("Photometry failed:\n {}".format(pdump_out))

    # Clean up temporary files generated by IRAF
    os.remove(coofile.name)
    os.remove(magfile.name)
    logger.debug("Computed aperture photometry on {} objects in {}".format(len(pdump_out), fits_filename))

    del input_hdulist
    return pdump_out


def phot_mag(*args, **kwargs):
    """Wrapper around phot which only returns the computed magnitude directly."""
    try:
        return phot(*args, **kwargs)
    except IndexError:
        raise TaskError("No photometric records returned for {0}".format(kwargs))
