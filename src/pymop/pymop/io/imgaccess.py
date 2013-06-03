__author__ = "David Rusk <drusk@uvic.ca>"

import threading

import vos

from pymop import config
from pymop.io.img import FitsImage


class AsynchronousImageDownloadManager(object):
    """
    Coordinates the downloading of images asynchronously from the rest of
    the application.
    """

    def __init__(self, downloader):
        self.downloader = downloader

    def start_download(self, astrom_data,
                       image_loaded_callback=None,
                       all_loaded_callback=None):

        self.image_loaded_callback = image_loaded_callback
        self.all_loaded_callback = all_loaded_callback

        self.do_download(astrom_data)

    def do_download(self, astrom_data):
        SerialImageDownloadThread(self, self.downloader, astrom_data).start()

    def on_image_downloaded(self, fitsimage, reading, source_num, obs_num):
        reading.set_fits_image(fitsimage)

        if self.image_loaded_callback is not None:
            self.image_loaded_callback(source_num, obs_num)

    def on_all_downloaded(self):
        if self.all_loaded_callback is not None:
            self.all_loaded_callback()


class SerialImageDownloadThread(threading.Thread):
    """
    Retrieve each image serially, but in this separate thread so it can
    happen in the background.
    """

    def __init__(self, loader, downloader, astrom_data):
        super(SerialImageDownloadThread, self).__init__()

        self.download_manager = loader
        self.downloader = downloader
        self.astrom_data = astrom_data

    def run(self):
        for source_num, source in enumerate(self.astrom_data.sources):
            for obs_num, reading in enumerate(source):
                fitsimage = self.downloader.download(reading)
                self.download_manager.on_image_downloaded(fitsimage, reading, source_num, obs_num)

        self.download_manager.on_all_downloaded()


class VOSpaceResolver(object):
    """
    Resolves resources in VOSpace.
    """

    def __init__(self):
        self.dataset_root = config.read("IMG_RETRIEVAL.DATASET_ROOT")

    def resolve_image_uri(self, observation):
        # XXX can there be other file extensions?  For example, fits.fz?
        # Do we need to search the vospace directory and choose based on that?
        return "%s/%s/%s%s.fits" % (self.dataset_root, observation.expnum,
                                    observation.expnum, observation.ftype)

    def resolve_apcor_uri(self, observation):
        return "%s/%s/ccd%s/%s.apcor" % (self.dataset_root, observation.expnum,
                                         observation.ccdnum, observation.rawname)


class ImageSliceDownloader(object):
    """
    Downloads a slice of an image relevant to examining a (potential) source.
    """

    def __init__(self, resolver, slice_rows=None, slice_cols=None, vosclient=None):
        """
        Constructor.

        Args:
          resolver:
            Resolves source readings to the URI's from which they can be
            retrieved.
          slice_rows, slice_cols: int
            The number of rows and columns (pixels) to slice out around the
            source.  Leave as None to use default configuration values.
        """
        self.resolver = resolver

        # If not provided, read defaults from application config file
        if slice_rows is None:
            slice_rows = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_ROWS")
        if slice_cols is None:
            slice_cols = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_COLS")

        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

        self.vosclient = vos.Client() if vosclient is None else vosclient

        self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def _download_fits_file(self, uri, source_reading):
        # NOTE: ccd number is the extension, BUT Fits file extensions start at 1
        # Therefore ccd n = extension n + 1
        extension = str(int(source_reading.obs.ccdnum) + 1)

        # XXX have to be careful about boundary locations
        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            extension, source_reading.reference_source_point)

        vofile = self.vosclient.open(uri, view="cutout", cutout=cutout_str)

        return vofile.read(), converter

    def _download_apcor_file(self, uri):
        vofile = self.vosclient.open(uri, view="data")
        return vofile.read()

    def download(self, source_reading, in_memory=True):
        """
        Retrieves a remote image.

        Args:
          source_reading: pymop.io.parser.SourceReading
            The reading to take a cutout around.
          in_memory: bool
            If True, the image is stored in memory without being written to
            disk.  If False, the image will be written to a temporary file.

        Returns:
          fitsimage: pymop.io.img.FitsImage
            The downloaded image, either in-memory or on disk as specified.
        """
        image_uri = self.resolver.resolve_image_uri(source_reading.obs)
        apcor_uri = self.resolver.resolve_apcor_uri(source_reading.obs)

        fits_str, converter = self._download_fits_file(image_uri, source_reading)
        apcor_str = self._download_apcor_file(apcor_uri)

        return FitsImage(fits_str, apcor_str, converter, in_memory=in_memory)


class CutoutCalculator(object):
    def __init__(self, slice_rows, slice_cols):
        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

    def build_cutout_str(self, extnum, point):
        """
        Generates the cutout string needed for the vospace client's open
        with cutout feature.

        Args:
          extnum: str
            The extension number of the cutout.  See also the vospace service
            documentation at http://www.cadc.hia.nrc.gc.ca/data/
          point: (x, y)
            The x and y coordinates of the point which is the focus of the
            cutout.

        Returns:
          cutout_str: str
            A string in the form [extnum][x0:x1,y0:y1]
          converter: CoordinateConverter
            Can be used to find a point in the sliced image based on its
            coordinate in the original image.
        """
        (x0, x1, y0, y1), converter = self.calc_cutout(point)

        cutout_str = "[%s][%d:%d,%d:%d]" % (extnum, x0, x1, y0, y1)

        return cutout_str, converter

    def calc_cutout(self, point):
        """
        Calculates the start and stop points of the cutout around a point.

        Args:
          point: (x, y)
            The x and y coordinates of the point which is the focus of the
            cutout.
        Returns:
            coords: (x0, x1, y0, y1)
              The cutout boundary coordinates
            converter: CoordinateConverter
              Can be used to find a point in the sliced image based on its
              coordinate in the original image.
        """
        x, y = point

        x_mid_offset = self.slice_cols / 2
        y_mid_offset = self.slice_rows / 2

        x0 = x - x_mid_offset
        x1 = x + x_mid_offset
        y0 = y - y_mid_offset
        y1 = y + y_mid_offset

        # Make sure we don't try to slice negative pixel locations
        if x0 < 1:
            diff = abs(x0 - 1)
            x0 += diff
            x1 += diff

        if y0 < 1:
            diff = abs(y0 - 1)
            y0 += diff
            y1 += diff

        # XXX how do we know if we slice too far in the other direction though?

        coords = (x0, x1, y0, y1)

        return coords, CoordinateConverter(x0, y0)


class CoordinateConverter(object):
    def __init__(self, x_offset, y_offset):
        self.x_offset = x_offset
        self.y_offset = y_offset

    def convert(self, point):
        x, y = point
        return x - self.x_offset, y - self.y_offset
