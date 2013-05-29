__author__ = "David Rusk <drusk@uvic.ca>"

import threading

import vos

from pymop import config
from pymop.io.img import InMemoryFitsImage, InFileFitsImage


class AsynchronousImageDownloadManager(object):
    """
    Coordinates the downloading of images asynchronously from the rest of
    the application.
    """

    def __init__(self, resolver, image_retriever):
        self.resolver = resolver
        self.image_retriever = image_retriever

    def start_download(self, astrom_data,
                       image_loaded_callback=None,
                       all_loaded_callback=None):

        self.image_loaded_callback = image_loaded_callback
        self.all_loaded_callback = all_loaded_callback

        lookupinfo = []
        for source_num, source in enumerate(astrom_data.sources):
            for obs_num, reading in enumerate(source):
                image_uri = self.resolver.resolve_uri(reading.obs)
                lookupinfo.append((image_uri, reading, source_num, obs_num))

        self.do_download(lookupinfo)

    def do_download(self, lookupinfo):
        SerialImageDownloadThread(self, self.image_retriever,
                                  lookupinfo).start()

    def on_image_downloaded(self, fitsimage, reading, source_num, obs_num):
        reading.fitsimage = fitsimage

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

    def __init__(self, loader, image_retriever, lookupinfo):
        super(SerialImageDownloadThread, self).__init__()

        self.download_manager = loader
        self.image_retriever = image_retriever
        self.lookupinfo = lookupinfo

    def run(self):
        for image_uri, reading, source_num, obs_num in self.lookupinfo:
            fitsimage = self.image_retriever.download_image_slice(image_uri, reading)
            self.download_manager.on_image_downloaded(fitsimage, reading, source_num, obs_num)

        self.download_manager.on_all_downloaded()


class VOSpaceResolver(object):
    """
    Resolves observation descriptions to their URIs.
    """

    def __init__(self):
        self.dataset_root = config.read("IMG_RETRIEVAL.DATASET_ROOT")

    def resolve_uri(self, observation):
        # XXX can there be other file extensions?  For example, fits.fz?
        # Do we need to search the vospace directory and choose based on that?
        return "%s/%s/%s%s.fits" % (self.dataset_root, observation.expnum,
                                    observation.expnum, observation.ftype)


class AbstractImageSliceDownloader(object):
    def __init__(self, slice_rows=None, slice_cols=None, vosclient=None):
        # If not provided, read defaults from application config file
        if slice_rows is None:
            slice_rows = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_ROWS")
        if slice_cols is None:
            slice_cols = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_COLS")

        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

        self.vosclient = vos.Client() if vosclient is None else vosclient

        self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def _do_download(self, source_reading, uri):
        # NOTE: ccd number is the extension, BUT Fits file extensions start at 1
        # Therefore ccd n = extension n + 1
        extension = str(int(source_reading.obs.ccdnum) + 1)

        # XXX have to be careful about boundary locations
        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            extension, source_reading.source_point)

        vofile = self.vosclient.open(uri, view="cutout", cutout=cutout_str)

        return vofile, converter

    def download_image_slice(self, uri, source_reading):
        raise NotImplementedError("Subclasses must implement this method.")


class InMemoryImageSliceDownloader(AbstractImageSliceDownloader):
    """
    Downloads an image slice without ever writing it to disk.
    """

    def __init__(self, slice_rows=None, slice_cols=None, vosclient=None):
        super(InMemoryImageSliceDownloader, self).__init__(slice_rows, slice_cols, vosclient)

    def download_image_slice(self, uri, source_reading):
        """
        Retrieves a remote image.

        Args:
          uri: str
            URI of the remote image to be retrieved.
          source_reading: pymop.io.parser.SourceReading
            Contains information about the CCD number and point about
            which the slice should be taken.

        Returns:
          hdulist: astropy HDUList
            HDU list for the requested image slice
          converter:
            Can be used to find a point in the sliced image based on its
            coordinate in the original image.
        """
        vofile, converter = self._do_download(source_reading, uri)

        return InMemoryFitsImage(vofile.read(), converter)


class TempfileImageSliceDownloader(AbstractImageSliceDownloader):
    """
    Downloads an image slice and writes it to a temporary file on disk.
    """

    def __init__(self, slice_rows=None, slice_cols=None, vosclient=None):
        super(TempfileImageSliceDownloader, self).__init__(slice_rows, slice_cols, vosclient)

    def download_image_slice(self, uri, source_reading):
        """
        Retrieves a remote image.

        Args:
          uri: str
            URI of the remote image to be retrieved.
          source_reading: pymop.io.parser.SourceReading
            Contains information about the CCD number and point about
            which the slice should be taken.

        Returns:
          imagefile: tempfile.NamedTemporaryFile
            A temporary file storing the FITS data on disk.  Open for
            reading and writing.  Note that this file will be automatically
            deleted when closed.
          converter:
            Can be used to find a point in the sliced image based on its
            coordinate in the original image.
        """
        vofile, converter = self._do_download(source_reading, uri)

        return InFileFitsImage(vofile.read(), converter)


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

        coords = (x - x_mid_offset, x + x_mid_offset,
                  y - y_mid_offset, y + y_mid_offset)

        return coords, CoordinateConverter(x - x_mid_offset, y - y_mid_offset)


class CoordinateConverter(object):
    def __init__(self, x_offset, y_offset):
        self.x_offset = x_offset
        self.y_offset = y_offset

    def convert(self, point):
        x, y = point
        return x - self.x_offset, y - self.y_offset
