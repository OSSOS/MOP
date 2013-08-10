"""
An example of parsing an .astrom file, downloading one of the images,
doing photometry calculations on it, and then displaying the image.
"""

from ossos import astrom
from ossos import fitsviewer
from ossos.downloads import cutouts
from ossos.downloads import requests

sources = astrom.parse_sources("../tests/data/astromdir/realstest2.measure3.reals.astrom")
reading = sources[0].get_reading(0)

downloader = cutouts.ImageCutoutDownloader(slice_rows=50, slice_cols=50)
request = requests.DownloadRequest(downloader, reading, needs_apcor=True)

snapshot = request.execute()

print "RA: %f" % snapshot.ra
print "DEC: %f" % snapshot.dec
print "Observed magnitude: %f" % snapshot.get_observed_magnitude()

fitsviewer.display_hdulist(snapshot.hdulist)
