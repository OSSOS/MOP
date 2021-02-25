from astropy import units
from astropy.units import Quantity
from ossos.gui import config

from ..core import Downloader, ApcorData
from ..cutouts.source import SourceCutout
from ... import storage
from ...astrom import SourceReading
from ...gui import logger


class ImageDownloader(Downloader):

    def download(self, reading, needs_apcor=False):

        """

        @param reading:
        @param needs_apcor:
        @return: SourceCutout
        """
        hdulist = storage.get_hdu(reading.get_image_uri())
        #apcor = needs_apcor and self.download_apcor(reading.get_apcor_uri()) or None
        #zmag = self.download_zmag(reading.get_zmag_uri())
        return SourceCutout(reading, hdulist)


class ImageCutoutDownloader(Downloader):
    """
    Downloads a slice of an image relevant to examining a (potential) source.
    """

    def __init__(self, slice_rows=500, slice_cols=500, vosclient=None):
        """
        Constructor.

        Args:
          slice_rows, slice_cols: int
            The number of rows and columns (pixels) to slice out around the
            source.  Leave as None to use default configuration values.
        """
        super(ImageCutoutDownloader, self).__init__()

        # self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def download_cutout(self, reading, focus=None, needs_apcor=False):
        """
        Downloads a cutout of the FITS image for a given source reading.

        Args:
          reading: ossos.astrom.SourceReading
            The reading which will be the focus of the downloaded image.
          focus: tuple(int, int)
            The x, y coordinates that should be the focus of the downloaded
            image.  These coordinates should be in terms of the
            source_reading parameter's coordinate system.
            Default value is None, in which case the source reading's x, y
            position is used as the focus.
          needs_apcor: bool
            If True, the apcor file with data needed for photometry
            calculations is downloaded in addition to the image.
            Defaults to False.

        Returns:
          cutout: ossos.downloads.data.SourceCutout
        """
        logger.debug("Doing download_cutout with inputs: reading:{} focus:{} needs_apcor:{}".format(reading,
                                                                                                    focus,
                                                                                                    needs_apcor))
        assert isinstance(reading, SourceReading)

        min_radius = config.read('CUTOUTS.SINGLETS.RADIUS')
        if not isinstance(min_radius, Quantity):
            min_radius = min_radius * units.arcsec

        radius = max(reading.uncertainty_ellipse.a,
                     reading.uncertainty_ellipse.b) * 2.5 + min_radius

        logger.debug("got radius for cutout: {}".format(radius))
        image_uri = reading.get_image_uri()
        logger.debug("Getting cutout at {} for {}".format(reading.reference_sky_coord, image_uri))
        hdulist = storage._cutout_expnum(reading.obs,
                                         reading.reference_sky_coord, radius)
        # hdulist = storage.ra_dec_cutout(image_uri, reading.reference_sky_coord, radius)
        logger.debug("Getting the aperture correction.")
        source = SourceCutout(reading, hdulist, radius=radius)
        # Accessing the attribute here to trigger the download.
        try:
            apcor = source.apcor
            zmag = source.zmag
            source.reading.get_observation_header()
        except Exception as ex:
            if needs_apcor:
                import sys, traceback
                sys.stderr.write("Failed to retrieve apcor but apcor required.  "
                                 "Raising error, see logs for more details")
                sys.stderr.write(traceback.print_exc())
            pass
        logger.debug("Sending back the source reading.")
        return source
