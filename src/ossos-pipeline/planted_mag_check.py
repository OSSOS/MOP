"""
Compare the measured fluxes of planted sources against those returned for by digiphot.
"""

__author__ = 'jjk'

import math
import sys

from ossos import astrom
from ossos.daophot import TaskError
from ossos.gui import logger
from ossos.mpc import Time
from ossos.astrom import AstromParser
from ossos.astrom import StreamingAstromWriter
from ossos.downloads.cutouts import ImageCutoutDownloader

import argparse

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



def match_planted(astrom_filename, match_filename, false_positive_filename):
    """
    Using the astrom_filename as input get the Object.planted file from VOSpace and match
    planted sources with found sources.

    The Object.planted list is pulled from VOSpace based on the standard file-layout and name of the
    first exposure as read from the .astrom file.

    :param astrom_filename: name of the fk*reals.astrom file to check against Object.planted
    :param match_filename: a file that will contain a list of all planted sources and the matched found source
    :param false_positive_filename: .astrom format output containing input objects that had no match in planted

    """
    image_slice_downloader = ImageCutoutDownloader(slice_rows=100, slice_cols=100)

    astrom_file_reader = AstromParser()


    fk_candidate_observations = astrom.parse(astrom_filename)
    matches_ftpr = open(match_filename,'w')

    objects_planted_uri = fk_candidate_observations.observations[0].get_object_planted_uri()


    objects_planted = image_slice_downloader.download_raw(objects_planted_uri).split('\n')

    planted_objects = []

    for line in objects_planted[1:]:
        if len(line) == 0 or line[0] == '#':
            continue
        planted_objects.append(PlantedObject(line))

    false_positives_ftpr = None
    false_positives_stream_writer = None

    matches_ftpr.write("#{}\n".format(fk_candidate_observations.observations[0].rawname))
    matches_ftpr.write("{:1s}{} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}\n".format(
        "",objects_planted[0],"x_dao","y_dao","mag_dao","rate_mes", "ang_mes", "dr" ))

    found_idxs = []
    for source in  fk_candidate_observations.get_sources():
        reading = source.get_reading(0)
        third  = source.get_reading(2)
        cutout = image_slice_downloader.download_cutout(reading, needs_apcor=True)

        try:
            mag = cutout.get_observed_magnitude()
        except TaskError as e:
            logger.warning(str(e))
            mag = 0.0

        observation = reading.get_observation()

        matched = None
        repeat = ''
        for idx in range(len(planted_objects)):
            planted_object = planted_objects[idx]
            dist = math.sqrt((reading.x-planted_object.x)**2 + (reading.y - planted_object.y)**2)
            if matched is None or dist < matched:
                matched = dist
                matched_object_idx = idx

        start_jd = Time(reading.obs.header['MJD_OBS_CENTER'],format='mpc', scale='utc').jd
        end_jd = Time(third.obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd
        exptime = float(reading.obs.header['EXPTIME'])
        dt = end_jd - start_jd
        rate = math.sqrt((third.x - reading.x)**2 + (third.y - reading.y)**2)/(
            24*(end_jd - start_jd) )
        angle = math.degrees(math.atan2(third.y - reading.y,third.x - reading.x))

        if matched > 3*rate*exptime/3600.0:
            # this is a false positive (candidate not near artificial source)
            # create a .astrom style line for feeding to validate for checking later
            if false_positives_ftpr is None or false_positives_stream_writer is None:
                # create false positive file for storing results
                false_positives_ftpr = open(false_positive_filename,'w+')
                false_positives_stream_writer = StreamingAstromWriter(
                    false_positives_ftpr,fk_candidate_observations.sys_header)
            false_positives_stream_writer.write_source(source)
            false_positives_ftpr.flush()
            continue
            repeat = '#'
        elif matched_object_idx in found_idxs:
            repeat = '#'
        else:
            repeat = ' '
            found_idxs.append(matched_object_idx)



        matches_ftpr.write("{:1s}{} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format(
            repeat,
            str(planted_objects[matched_object_idx]), reading.x, reading.y, mag, rate, angle, matched))
        matches_ftpr.flush()



    # close the false_positives
    if false_positives_ftpr is not None:
        false_positives_ftpr.close()

    # record the unmatched Object.planted entries, for use in efficiency computing
    for idx in range(len(planted_objects)):
        if idx not in found_idxs:
            planted_object = planted_objects[idx]
            matches_ftpr.write("{:1s}{} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format("",str(planted_object),
                                                                          0, 0, 0, 0, 0, 0))
    matches_ftpr.close()


if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('astrom_filename',
                        help=".astrom containing objects to match with Object.planted list.")
    parser.add_argument('match_filename',
                        help="name of file to start matched objects into.")
    parser.add_argument('false_positive_filename',
                        help='name of file to send false positives into')

    args  = parser.parse_args()

    match_planted(args.astrom_filename,args.match_filename, args.false_positive_filename)


