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
        self.confused = 0

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
        return

    matches_fptr = open(os.path.basename(cand_filename)+".eff", 'w')

    objects_planted_uri = cands.observations[0].get_object_planted_uri()
    planted_object_file = Planted_object_file(objects_planted_uri)
    planted_objects = planted_object_file.planted_objects

    matched = {}
    false_positive_sources = []
    false_negative_sources = []
    confused_measure = {}
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
                if measure_dist < 6.0:
                    # this gets 'unset' if we match this measure with a cand, in the next step.
                    confused_measure[provisional] = planted_object
                    planted_object.confused += 1

        # determine if planted_object was found
        if cand_dist < 6.0:
            # In candidate list
            if measure_dist is not None and measure_dist < 6.0:
                # accepted.
                planted_object.recovered = measure_source
                planted_object.confused -= 1
                del(confused_measure[measure_source[0].provisional_name])
                matched[measure_source[0].provisional_name] = True
            else:
                # rejected.
                planted_object.false_negative = cand_source
                false_negative_sources.append(cand_source)

    matches_fptr.write("{:1s}{} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}\n".format(
    "",planted_object_file.header,"x_dao","y_dao","mag_dao","merr_dao", "rate_mes", "ang_mes" ))


    for planted_object in planted_objects:
        if planted_object.recovered is not None:
            confused = " "
            if planted_object.confused < 1 :
                confused = "+"
            measure = planted_object.recovered
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

        if provisional in confused_measure:
            confused = "+"
            planted_object = confused_measure[provisional]
        else:
            confused = "F"
            planted_object = blank
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


        matches_fptr.write("{:1s}{} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format(
            confused, planted_object, x, y, mag, merr, rate, angle))

    matches_fptr.close()


    ## write out the false_positives and false_negatives
    if not os.access('false_positives',os.R_OK):
        os.mkdir('false_positives')
    if not os.access('false_negatives', os.R_OK):
        os.mkdir('false_negatives')

    if len(false_positive_sources) > 0  :
        wh = open('false_positives/'+os.path.basename(cand_filename),'w+')
        writer = astrom.StreamingAstromWriter(wh,cands.sys_header)
        #writer.write_headers(cands.observations)
        for source in false_positive_sources:
            writer.write_source(source)
        writer.flush()
        writer.close()

    if len(false_negative_sources) > 0  :
        wh = open('false_negatives/'+os.path.basename(cand_filename),'w+')
        writer = astrom.StreamingAstromWriter(wh,cands.sys_header)
        #writer.write_headers(cands.observations)
        for source in false_negative_sources:
            writer.write_source(source)
        writer.flush()
        writer.close()



if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('measure3',
                        help="Directory containing the measure3 files you did cands and reals on")
    parser.add_argument('--reals',
                        help="Directory with the output from validate, default is the same as measure3.")
    parser.add_argument('--dbimages', default='vos:OSSOS/dbimages')

    args  = parser.parse_args()

    astrom.DATASET_ROOT = args.dbimages
    storage.DBIMAGES = args.dbimages
    storage.MEASURE3 = args.measure3

    ## only measure if we have completed 'reals' on this file.
    reals_filepath = ( args.reals is not None and args.reals ) or args.measure3
    cands_filepath = args.measure3
    cands_filelist = storage.my_glob(cands_filepath+"/fk*.cands.astrom")

    for cands in cands_filelist:
        cands_filename = os.path.basename(cands)
        reals_filename = reals_filepath+cands_filename.replace('cands','reals')
        if not (( reals_filename[0:4] == 'vos:' and storage.exists(reals_filename+".DONE") ) or os.access(reals_filename+".DONE",os.R_OK)) :
            # skipping incomplete field/ccd combo
            continue
        sys.stderr.write("Getting list of .mpc files for input "+reals_filename+" ...")
        # find and load any .mpc files associated with this candidate file.
        mpc_list = storage.my_glob(reals_filename+".*.mpc")
        sys.stderr.write(" got {} detections \n".format(len(mpc_list)))
        if not len(mpc_list) > 0 :
            continue
        measures =  {}
        for mpc_fname in mpc_list:
            provisional = mpc_fname.split(".")[-2]
            measures[provisional] = []
            for line in open(mpc_fname,'r').readlines():
                measures[provisional].append(mpc.Observation.from_string(line))

        match_planted(cands, measures)
