__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.gui import logger

from ossos.downloads.core import Downloader
from ossos.downloads.cutouts.calculator import CutoutCalculator
from ossos.downloads.cutouts.source import SourceCutout


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
        super(ImageCutoutDownloader, self).__init__(vosclient=vosclient)

        self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

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
        if focus is None:
            focus = reading.source_point

        cutout_str, converter, padding = self.cutout_calculator.build_cutout_str(
            reading.get_extension(),
            focus,
            reading.get_original_image_size(),
            inverted=reading.is_inverted(),
            should_invert=reading.should_invert())

        image_uri = reading.get_image_uri()

        logger.debug("Calculated cutout: %s for %s"
                     % (cutout_str, image_uri))

        hdulist = self.download_hdulist(image_uri, view="cutout",
                                        cutout=cutout_str)

        hdulist[0].header['XMINPAD'] = padding[0]
        hdulist[0].header['XMAXPAD'] = padding[1]
        hdulist[0].header['YMINPAD'] = padding[2]
        hdulist[0].header['YMAXPAD'] = padding[3]

        apcor = None
        if needs_apcor:
            apcor = self.download_apcor(reading.get_apcor_uri())

        return SourceCutout(reading, hdulist, converter, apcor)

