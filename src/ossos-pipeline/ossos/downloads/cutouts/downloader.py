import re
import math
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

        assert isinstance(reading, SourceReading)
        dx = 3*math.fabs(-reading.dra*math.sin(math.radians(reading.pa)) + reading.ddec*math.cos(math.radians(reading.pa)))
        dy = 3*math.fabs(reading.dra*math.cos(math.radians(reading.pa)) - reading.ddec*math.sin(math.radians(reading.pa)))
        logger.info("Got error ellipse dimensions {} {} from {} {} {} ".format(dx, dy, reading.dra, reading.ddec, reading.pa))
        dx = max(reading.dx, dx)
        dy = max(reading.dy, dy)

        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            reading.get_extension(),
            focus,
            reading.get_original_image_size(),
            dx = dx,
            dy = dy,
            inverted=reading.is_inverted())

        image_uri = reading.get_image_uri()
        cutout = re.findall(r'(\d+)', cutout_str)
        y2 = int(cutout[-1])
        y1 = int(cutout[-2])
        logger.info("Calculated cutout: %s for %s"
                     % (cutout_str, image_uri))

        hdulist = self.download_hdulist(image_uri, view="cutout",
                                        cutout=cutout_str)
        # modify the DATASEC to account for possible flip/flop and changes in dimensions of the image.
        (NAXIS1, NAXIS2) = reading.get_original_image_size()
        DATASEC = hdulist[0].header.get('DATASEC',None)
        if DATASEC is not None:
            datasec = re.findall(r'(\d+)', DATASEC)
            if y2 < y1:
                x2 = int(NAXIS1) - int(datasec[0]) + 1
                x1 = int(NAXIS1) - int(datasec[1]) + 1
                y2 = int(NAXIS2) - int(datasec[2]) + 1
                y1 = int(NAXIS2) - int(datasec[3]) + 1
                logger.info("Flip/Flopped DATASEC from {} to [{}:{}:{}:{}]".format(DATASEC, x1,x2,y1,y2))
                datasec = (x1,x2,y1,y2)
            (x1,y1) = converter.convert((int(datasec[0]),int(datasec[2])))
            x1 = max(1,x1)
            y1 = max(1,y1)
            (x2,y2) = converter.convert((int(datasec[1]),int(datasec[3])))
            x2 = min(x2, int(hdulist[0].header['NAXIS1']))
            y2 = min(y2, int(hdulist[0].header['NAXIS2']))
            datasec = "[{}:{},{}:{}]".format(x1,x2,y1,y2)
            logger.info("Trimmed and offset DATASEC from {} to {}".format(DATASEC, datasec))

            hdulist[0].header['DATASEC'] = datasec

        apcor = None
        if needs_apcor:
            try:
                apcor = self.download_apcor(reading.get_apcor_uri())
            except:
                apcor = None
        zmag = None
        try:
            zmag = self.download_zmag(reading.get_zmag_uri())
        except Exception as e:
	    logger.debug(str(e))
            pass

        return SourceCutout(reading, hdulist, converter, apcor, zmag=zmag)

