import re
from ... import storage
from ...astrom import SourceReading
from astropy import units
from ...gui import logger

from ..core import Downloader
from ..cutouts.source import SourceCutout


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

        #self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def download_cutout(self, reading, focus=None, needs_apcor=False):
        """
        Downloads a cutout of the FITS image for a given source reading.

        Args:
          source_reading: ossos.astrom.SourceReading
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
        assert isinstance(reading, SourceReading)
        radius = max(reading.uncertainty_ellipse.a, reading.uncertainty_ellipse.b, 10 * units.arcminute)
        radius *= 2
        image_uri = reading.get_image_uri()
        hdulist = storage.ra_dec_cutout(image_uri, reading.sky_coord.ra, radius)

        apcor = None
        if needs_apcor:
            try:
                apcor = self.download_apcor(reading.get_apcor_uri())
            except Exception as e:
                logger.error(str(e))
                apcor = None

        zmag = None
        try:
            zmag = self.download_zmag(reading.get_zmag_uri())
        except Exception as e:
            logger.error(str(e))
            pass

        return SourceCutout(reading, hdulist, apcor, zmag=zmag)

