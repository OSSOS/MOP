"""
An example of parsing an .astrom file, downloading one of the images,
doing photometry calculations on it, and then displaying the image.
"""

from ossos import astrom
from ossos import fitsviewer
from ossos.downloads.cutouts import ImageCutoutDownloader

sources = astrom.parse_sources("../tests/data/astromdir/realstest2.measure3.reals.astrom")
reading = sources[0].get_reading(0)

downloader = ImageCutoutDownloader(slice_rows=50, slice_cols=50)
cutout = downloader.download_cutout(reading, needs_apcor=True)

print("RA: %f" % cutout.ra)
print("DEC: %f" % cutout.dec)
print("Observed magnitude: %f" % cutout.get_observed_magnitude()[0])

fitsviewer.display_hdulist(cutout.hdulist)
