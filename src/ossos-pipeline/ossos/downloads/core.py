__author__ = "David Rusk <drusk@uvic.ca>"

import cStringIO

from astropy.io import fits
import vos

from ossos.downloads.data import ApcorData
from ossos.gui import logger


class Downloader(object):
    """
    Downloads data from VOSpace.
    """

    def __init__(self, vosclient=None):
        if vosclient is None:
            self.vosclient = self._create_default_vosclient()
        else:
            self.vosclient = vosclient

    def download_raw(self, uri, **kwargs):
        """
        Downloads raw data from VOSpace.

        Args:
          uri: The URI of the resource to download.
          kwargs: optional arguments to pass to the vos client.

        Returns:
          raw_string: str
            The data downloaded as a string.
        """
        logger.debug("Starting download: %s" % uri)
        return self.vosclient.open(uri, **kwargs).read()

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
        return fits.open(cStringIO.StringIO(self.download_raw(uri, **kwargs)))

    def download_apcor(self, uri):
        """
        Downloads apcor data.

        Args:
          uri: The URI of the apcor data file.

        Returns:
          apcor: ossos.downloads.data.ApcorData
        """
        return ApcorData.from_string(self.download_raw(uri, view="data"))

    def download_object_planted(self, uri):

        logger.debug("Starting download: %s" % uri)

        vofile = self.vosclient.open(uri, view="data")
        outstr = ''
        while True:
            buff = vofile.read()
            if len(buff) == 0:
                break
            outstr += buff
        return outstr

    def refresh_vos_client(self):
        """
        If we have gotten a new certfile we have to create a new Client
        object before it will get used.
        """
        self.vosclient = self._create_default_vosclient()

    def _create_default_vosclient(self):
        return vos.Client(cadc_short_cut=True)
