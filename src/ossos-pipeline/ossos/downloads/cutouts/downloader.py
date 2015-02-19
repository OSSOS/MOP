import re
from ossos import storage
from ossos.astrom import SourceReading
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
        super(ImageCutoutDownloader, self).__init__()

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

        assert isinstance(reading, SourceReading)
        dx = dy = 2 * max(reading.dra, reading.ddec)
        dx = max(reading.dx, dx)
        dy = max(reading.dy, dy)

        (NAXIS1, NAXIS2) = reading.get_original_image_size()

        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            reading.get_extension(),
            focus,
            (NAXIS1, NAXIS2),
            dx = dx,
            dy = dy,
            inverted=reading.is_inverted)

        image_uri = reading.get_image_uri()
        cutout = re.findall(r'(\d+)', cutout_str)
        y2 = int(cutout[-1])
        y1 = int(cutout[-2])
        logger.debug("Calculated cutout: %s for %s"
                     % (cutout_str, image_uri))

        hdulist = storage.get_image(expnum=reading.get_exposure_number(),
                                    ccd=reading.get_ccd_num(),
                                    cutout=cutout_str,
                                    version=reading.get_observation().ftype,
                                    prefix=reading.get_observation().fk,
                                    return_file=False
                                    )

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

        return SourceCutout(reading, hdulist, converter, apcor, zmag=zmag)

