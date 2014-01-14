"""
Compare the measured fluxes of planted sources against those returned for by digiphot.
"""
from StdSuites import strings, string
from glob import glob
import sys

__author__ = 'jjk'

import math
import os



from ossos.daophot import TaskError
from ossos.gui import logger
from ossos import mpc
from ossos import astrom
from ossos.downloads.cutouts import ImageCutoutDownloader
from ossos import storage

import argparse
image_slice_downloader = ImageCutoutDownloader(slice_rows=100, slice_cols=100)

class Planted_object_file(object):



    def __init__(self, uri):
        self.filename = uri
        objects_planted_lines = image_slice_downloader.download_raw(
            uri, view='data').split('\n')
        self.header = objects_planted_lines[0]
        self.planted_objects = []
        for line in objects_planted_lines[1:]:
            if len(line) > 0 and line.strip()[0] != "#":
                self.planted_objects.append(PlantedObject(line))


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
        self.recovered = None
        self.false_negative = None
        self.confused = None

    def __str__(self):
        return self.line

def get_planted_objects(mpc_comment):
    """
    Using an mpc comment record determine the location of the Object.planted file and retrieve.
    then build the file lines into an array of plantedObject objects.
    """
    assert isinstance(mpc_comment, mpc.MPCComment)
    (expnum, ccd) = mpc_comment.frame.splt('p')

    obs = astrom.Observation.from_source_reference(int(expnum),
                                             int(ccd),
                                             float(mpc_comment.X),
                                             float(mpc_comment.Y))

    object_planted_uri = obs.get_object_planted_uri()
    image_slice_downloader = ImageCutoutDownloader(slice_rows=100, slice_cols=100)
    planted_objects = []
    for line in image_slice_downloader.download_raw(object_planted_uri, view='data').splt('\n')[1:]:
        planted_objects.append(PlantedObject(line))
    return planted_objects


def match_planted(cand_filename, measures):
    """

    """

    # Load the planted objects associated with this candidate file.

    try:
        cands = astrom.parse(cand_filename)
    except:
        sys.stderr("Failed while reading {}".format(cand_filename))
        sys.exit(-1)

    matches_fptr = open(args.cand_filename+".eff", 'w')

    objects_planted_uri = cands.observations[0].get_object_planted_uri()
    planted_object_file = Planted_object_file(objects_planted_uri)
    planted_objects = planted_object_file.planted_objects

    matched = {}
    false_positive_sources = []
    false_negative_sources = []
    for idx in range(len(planted_objects)):
        planted_object = planted_objects[idx]

        # look for a matching .cand entry
        cand_dist = None
        cand_source = None
        for source in cands.get_sources():
            obs = source.get_readings()
            dist = math.sqrt((obs[0].x-planted_object.x)**2 +
                             (obs[0].y - planted_object.y)**2)
            if cand_dist is None or cand_dist > dist:
                cand_dist = dist
                cand_source = source

        # look for a matching .mpc entry
        measure_dist = None
        measure_source = None
        for provisional in measures:
            x = float(measures[provisional][0].comment.X)
            y = float(measures[provisional][0].comment.Y)
            dist = math.sqrt( (x - planted_object.x)**2 + (y-planted_object.y)**2)
            if measure_dist is None or measure_dist > dist:
                measure_dist = dist
                measure_source = measures[provisional]

        # determine if planted_object was found
        if cand_dist < 6.0:
            # In candidate list
            if measure_dist is not None and measure_dist < 6.0:
                # accepted.
                planted_object.recovered = measure_source
                matched[measure_source[0].provisional_name] = True
            else:
                # rejected.
                planted_object.false_negative = cand_source
                false_negative_sources.append(cand_source)
        elif measure_dist < 6.0:
            # not in 'cand' but in .mpc ?
            planted_object.confused = measure_source
            matched[measure_source[0].provisional_name] = True


    matches_fptr.write("{:1s}{} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}\n".format(
    "",planted_object_file.header,"x_dao","y_dao","mag_dao","merr_dao", "rate_mes", "ang_mes" ))


    for planted_object in planted_objects:
        if planted_object.recovered is not None or planted_object.confused is not None:
            if planted_object.recovered is not None:
                measure = planted_object.recovered
                confused = " "
            else:
                measure = planted_object.confused
                confused = "+"
            start_jd = measure[0].date.jd
            x = float(measure[0].comment.X)
            x3 = float(measure[2].comment.X)
            y = float(measure[0].comment.Y)
            y3 = float(measure[2].comment.Y)
            end_jd = measure[2].date.jd

            rate = math.sqrt((x3-x)**2 + (y3-y)**2)/(
                24*(end_jd - start_jd)
            )
            angle = math.degrees(math.atan2(y3 - y,x3 - x))
            mag = 0.0
            merr = 0.0
            try:
                mag = float(measure[0].comment.mag)
                merr = float(measure[0].comment.mag_uncertainty)
            except:
                sys.stderr.write(
                    "Error converting mag/merr to float, got: '{}' '{}'\n".format(measure[0].comment.mag,
                                                                                  measure[0].comment.mag_uncertainty))
            matches_fptr.write("{:1s}{} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format(
            confused, str(planted_object), x, y, mag, merr, rate, angle))

        elif planted_object.false_negative is not None:
            source = planted_object.false_negative
            reading = source.get_reading(0)
            third = source.get_reading(2)
            cutout = image_slice_downloader.download_cutout(reading, needs_apcor=True)

            try:
                (x, y, mag, merr) = cutout.get_observed_magnitude()
            except TaskError as e:
                logger.warning(str(e))
                mag = 0.0
                merr = -1.0

            start_jd = mpc.Time(reading.obs.header['MJD_OBS_CENTER'],format='mpc', scale='utc').jd
            end_jd = mpc.Time(third.obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd

            rate = math.sqrt((third.x - reading.x)**2 + (third.y - reading.y)**2)/(
                24*(end_jd - start_jd) )
            angle = math.degrees(math.atan2(third.y - reading.y,third.x - reading.x))
            matches_fptr.write("{:1s}{} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format(
            "*", str(planted_object), reading.x, reading.y, mag, merr, rate, angle))
        else:
            matches_fptr.write("{:1s}{} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format(
                "",str(planted_object),0, 0, 0, 0, 0, 0))
        matches_fptr.flush()


    blank = """    000.00    0000.00      00.00      00.00       0.00       0.00     00"""
    for provisional in measures:
        if matched.get(provisional,False):
            continue
        # this source is a false positive
        measure = measures[provisional]
        start_jd = measure[0].date.jd
        x = float(measure[0].comment.X)
        x3 = float(measure[2].comment.X)
        y = float(measure[0].comment.Y)
        y3 = float(measure[2].comment.Y)
        end_jd = measure[2].date.jd

        # look for the matching cand entry
        cand_dist = None
        cand_source = None
        for source in cands.get_sources():
            obs = source.get_readings()
            dist = math.sqrt((obs[0].x - x)**2 +
                             (obs[0].y - y)**2)
            if cand_dist is None or cand_dist > dist:
                cand_dist = dist
                cand_source = source

        false_positive_sources.append(cand_source)

        rate = math.sqrt((x3-x)**2 + (y3-y)**2)/(
                24*(end_jd - start_jd))
        angle = math.degrees(math.atan2(y3 - y,x3 - x))

        mag = 0.0
        merr = 0.0
        try:
            mag = float(measure[0].comment.mag)
            merr = float(measure[0].comment.mag_uncertainty)
        except:
            sys.stderr.write(
                "Error converting mag/merr to float, got: '{}' '{}'\n".format(measure[0].comment.mag,
                    measure[0].comment.mag_uncertainty))
            sys.exit(-1)

        matches_fptr.write("{:1s}{} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format(
            '+', blank, x, y, mag, merr, rate, angle))

    matches_fptr.close()


    ## write out the false_positives and false_negatives
    if len(false_positive_sources) > 0  :
        wh = open(cand_filename+'.false_positives','w+')
        writer = astrom.StreamingAstromWriter(wh,cands.sys_header)
        #writer.write_headers(cands.observations)
        for source in false_positive_sources:
            writer.write_source(source)
        writer.flush()
        writer.close()

    if len(false_negative_sources) > 0  :
        wh = open(cand_filename+'.false_negatives','w+')
        writer = astrom.StreamingAstromWriter(wh,cands.sys_header)
        #writer.write_headers(cands.observations)
        for source in false_negative_sources:
            writer.write_source(source)
        writer.flush()
        writer.close()


if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('cand_filename',
                        help="cands.astrom file containing objects to match with Object.planted list.")
    parser.add_argument('--dbimages', default='vos:OSSOS/dbimages')

    args  = parser.parse_args()

    astrom.DATASET_ROOT = args.dbimages
    storage.DBIMAGES = args.dbimages

    ## only measure if we have completed 'reals' on this file.
    reals = args.cand_filename.replace("cands","reals")
    print "Using files %s and %s" % ( args.cand_filename, reals)

    if not os.access(reals+".DONE",os.F_OK):
        sys.exit(0)

    # find and load any .mpc files associated with this candidate file.
    measures =  {}
    filenames = glob(reals+".*.mpc")
    if not len(filenames) > 0 :
        sys.exit(-1)
    for filename in filenames:
        provisional = filename.split(".")[-2]
        measures[provisional] = []
        for line in open(filename,'r').readlines():
            measures[provisional].append(mpc.Observation.from_string(line))

    match_planted(args.cand_filename, measures)


