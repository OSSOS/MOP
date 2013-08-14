"""
Compare the measured fluxes of planted sources against those returned for by digiphot.
"""
import sys
import math
from ossos.daophot import TaskError
from ossos.downloads.data import SourceSnapshot

__author__ = 'jjk'

class PlantedObject(object):

    def __init__(self, line):
        vals = line.strip().split()
        self.id = int(vals.pop())
        self.rate_arcsec = float(vals.pop())
        self.angle = float(vals.pop())
        self.rate_pixels = float(vals.pop())
        self.mag = float(vals.pop())
        self.y = float(vals.pop())
        self.x = float(vals.pop())
        self.line = line

    def __str__(self):
        return self.line


from astropy.io import ascii


from ossos.mpc import Time
from ossos.astrom import AstromParser
from ossos.downloads import cutouts, requests

image_slice_downloader = cutouts.ImageCutoutDownloader(slice_rows=100, slice_cols=100)

astrom_file_reader = AstromParser()



fk_candidate_observations = astrom_file_reader.parse('vos:OSSOS/measure3/2013A-E/'+sys.argv[1])

objects_planted_uri = fk_candidate_observations.observations[0].get_object_planted_uri()

objects_planted = image_slice_downloader.download_raw(objects_planted_uri).split('\n')

planted_objects = []

for line in objects_planted:
    if len(line) == 0 or line[0] == '#':
        continue
    planted_objects.append(PlantedObject(line))


print "#",fk_candidate_observations.observations[0].rawname
for source in  fk_candidate_observations.get_sources():
    reading = source.get_reading(0)
    second = source.get_reading(1)
    third  = source.get_reading(2)

    download_request = requests.DownloadRequest(
        reading,
        needs_apcor=True)

    snapshot = download_request.execute(image_slice_downloader)

    try:
        mag = snapshot.get_observed_magnitude()
    except TaskError as e:
        mag = 0.0

    observation = reading.get_observation()

    matched = -1
    for planted_object in planted_objects:
        dist = math.sqrt((reading.x-planted_object.x)**2 + (reading.y - planted_object.y)**2)
        if matched < 0 or dist < matched:
            matched = dist
            matched_object = planted_object

    start_jd = Time(reading.obs.header['MJD_OBS_CENTER'],format='mpc', scale='utc').jd
    end_jd = Time(third.obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd
    rate = math.sqrt((third.x - reading.x)**2 + (third.y - reading.y)**2)/(
        24*(end_jd - start_jd) )
    angle = math.degrees(math.atan2(third.y - reading.y,third.x - reading.x))
    print "{} {:6.1f} {:6.1f} {:6.2f} {:6.1f} {:6.1f} {:6.2f}".format(
        str(matched_object), reading.x, reading.y, mag, rate, angle, matched)
