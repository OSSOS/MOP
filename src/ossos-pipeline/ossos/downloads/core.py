import re
import requests

__author__ = "David Rusk <drusk@uvic.ca>"

import cStringIO

from astropy.io import fits
import vos

from ossos.gui import logger


SERVER='https://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/vospace/'
# OSSOS/dbimages/1625660/1625660p.fits?cutout=[1][990:1140,4144:4294]'

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
        certfile = self.vosclient.conn.certfile
        if "URL" in kwargs.keys():
            r = requests.get(kwargs['URL'], cert=certfile)
            buff = r.content
            return buff

        if not 'vos:' in uri:
            logger.debug("cadcVOFS download: %s" % uri)
            buff = self.vosclient.open(uri, **kwargs).read()
        else:
            groups = re.match("vos:(?P<path>.*)", uri)
            logger.debug("requests download")
            params = None
            if 'cutout' in kwargs.keys():
                params = {'cutout': kwargs['cutout']}
            r = requests.get(SERVER+groups.group('path'),params=params, cert=certfile)
            buff = r.content

        return buff

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
        hdulist = fits.open(cStringIO.StringIO(self.download_raw(uri, **kwargs)))
        logger.debug("Got fits hdulist of len {}".format(len(hdulist)))
        return hdulist

    def download_apcor(self, uri):
        """
        Downloads apcor data.

        Args:
          uri: The URI of the apcor data file.

        Returns:
          apcor: ossos.downloads.core.ApcorData
        """
        return ApcorData.from_string(self.download_raw(uri, view="data"))

    def download_zmag(self, uri):
        return float(self.download_raw(uri, view="data"))

    def refresh_vos_client(self):
        """
        If we have gotten a new certfile we have to create a new Client
        object before it will get used.
        """
        self.vosclient = self._create_default_vosclient()

    def _create_default_vosclient(self):
        return vos.Client(cadc_short_cut=True)


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
        args = map(float, rawstr.split())
        return cls(*args)

    @property
    def aperture(self):
        return self.ap_in

    @property
    def sky(self):
        return self.ap_out + 1

    @property
    def swidth(self):
        return 3*self.ap_in
