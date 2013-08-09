"""
Compare the measured fluxes of planted sources against those returned for by digiphot.
"""
import sys
import math
from ossos.daophot import TaskError
from ossos.gui.models import ImageReading

__author__ = 'jjk'


from astropy.io import ascii


from ossos.astrom import AstromParser
from ossos.astrom import Observation
from ossos.gui import downloads

image_slice_downloader = downloads.ImageSliceDownloader(slice_rows=100, slice_cols=100)

astrom_file_reader = AstromParser()



fk_candidate_observations = astrom_file_reader.parse('vos:OSSOS/measure3/2013A-E/'+sys.argv[1])

objects_planted_uri = fk_candidate_observations.observations[0].get_object_planted_uri()

objects_planted = image_slice_downloader.download_object_planted(objects_planted_uri)

data = ascii.read(objects_planted)

print "#",fk_candidate_observations.observations[0].rawname
for source in  fk_candidate_observations.get_sources():
    reading = source.get_reading(0)
    downloadable_item = downloads.DownloadableItem(reading, source,
                                                   needs_apcor=True,
                                                   on_finished_callback=None,
                                                   in_memory=True
                                                   )
    fits_image = image_slice_downloader.download(downloadable_item)
    image_reading = ImageReading(reading, fits_image)
    try:
        mag = image_reading.get_observed_magnitude()
    except TaskError as e:
        mag = 0.0

    observation = reading.get_observation()

    matched = False
    for row in data:
        if math.sqrt((reading.x-row['col1'])**2 + (reading.y - row['col2'])**2) < 5.0:
            print reading.x, reading.y, mag, row['col4'], row['col5'], mag-row['col3']
            matched = True
            break

    if not matched:
        print reading.x, reading.y, mag, 0, 0, 0