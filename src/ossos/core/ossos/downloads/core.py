import os

__author__ = "David Rusk <drusk@uvic.ca>"

from astropy.io import fits
import io
from ossos.gui import logger
from .. import storage
import sys


class Downloader(object):
    """
    Downloads data from VOSpace.
    """

    def download_hdulist(self, uri, **kwargs):
        """
        Downloads a FITS image as a HDUList.

        Args:
          uri: The URI of the FITS image to download.
          kwargs: optional arguments to pass to the vos client.
            For example, passing view="cutout" and cutout=[1] will result
            in a cutout of extension 1 from the FITS image specified by the
            URI.

        Returns:
          hdulist: astropy.io.fits.hdu.hdulist.HDUList
            The requests FITS image as an Astropy HDUList object
            (http://docs.astropy.org/en/latest/io/fits/api/hdulists.html).
        """

        logger.debug(str(kwargs))
        hdulist = None
        try:
            vobj = storage.vofile(uri, **kwargs)
            try:
                fobj = io.StringIO(vobj.read())
                fobj.seek(0)
                hdulist = fits.open(fobj)
            except Exception as e:
                sys.stderr.write("ERROR: {}\n".format(str(e)))
                sys.stderr.write("While loading {} {}\n".format(uri, kwargs))
                pass
            finally:
                vobj.close()
        except Exception as e:
            sys.stderr.write(str(e)+"\n")
            sys.stderr.write("While opening connection to {}.\n".format(uri))
            sys.stderr.write("Sending back FLAT instead, too keep display happy.")
            hdulist = self.download_hdulist('vos:OSSOS/dbimages/calibrators/13AQ05_r_flat.fits', **kwargs)
        return hdulist

    def download_apcor(self, uri):
        """
        Downloads apcor data.

        Args:
          uri: The URI of the apcor data file.

        Returns:
          apcor: ossos.downloads.core.ApcorData
        """

        local_file = os.path.basename(uri)
        if os.access(local_file, os.F_OK):
            fobj = open(local_file)
        else:
            fobj = storage.vofile(uri, view='data')
            fobj.seek(0)
        str = fobj.read()
        fobj.close()
        apcor_str = str
        return ApcorData.from_string(apcor_str)

    def download_zmag(self, uri):
        local_file = os.path.basename(uri)
        if os.access(local_file, os.F_OK):
            return float(open(local_file).read())
        fobj = storage.vofile(uri, view="data")
        fobj.seek(0)
        str = fobj.read()
        return float(str)


class ApcorData(object):
    def __init__(self, ap_in, ap_out, apcor, apcor_err):
        self.ap_in = ap_in
        self.ap_out = ap_out
        self.apcor = apcor
        self.apcor_err = apcor_err

    @classmethod
    def from_string(cls, rawstr):
        """
        Creates an ApcorData record from the raw string format.

        Expected string format:
        ap_in ap_out   ap_cor  apcor_err
        """
        try:
            args = list(map(float, rawstr.split()))
        except Exception as ex:
            import sys
            logger.error("Failed to convert aperture correction: {}".format(ex))
            raise ex
        return cls(*args)

    @property
    def valid(self):
        return self.apcor_err < 1.0

    @property
    def aperture(self):
        return self.ap_in

    @property
    def sky(self):
        return self.ap_out + 1

    @property
    def swidth(self):
        return 2*self.ap_in
