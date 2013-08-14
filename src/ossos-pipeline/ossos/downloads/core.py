__author__ = "David Rusk <drusk@uvic.ca>"

import cStringIO

from astropy.io import fits
import vos

from ossos.downloads.data import ApcorData
from ossos.gui import logger


class Downloader(object):
    def __init__(self, vosclient=None):
        if vosclient is None:
            self.vosclient = self._create_default_vosclient()
        else:
            self.vosclient = vosclient

    def _fetch(self, uri, **kwargs):
        logger.debug("Starting download: %s" % uri)
        return self.vosclient.open(uri, **kwargs).read()

    def download_hdulist(self, uri, **kwargs):
        return fits.open(cStringIO.StringIO(self._fetch(uri, **kwargs)))

    def download_apcor(self, uri):
        return ApcorData.from_string(self._fetch(uri, view="data"))

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
