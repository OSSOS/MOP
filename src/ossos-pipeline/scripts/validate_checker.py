#!python
"""
Compare the measured fluxes of planted sources against those returned for by digiphot.
"""
from StdSuites import strings, string
from glob import glob
import sys
import astropy.io.ascii
from matplotlib import pyplot
import numpy

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
        #self.header = objects_planted_lines[0]
        self.planted_objects = []
        for line in objects_planted_lines[1:]:
            if len(line) > 0 and line.strip()[0] != "#":
                self.planted_objects.append(PlantedObject(line))
    @property
    def header(self):
        line = "{:4s}"+" {:8s}"*6
        return line.format("id","x_pix","y_pix","r_arcsec","r_pixel","angle","mag_plt")


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
        line = "{:4d}"+" {:8.2f}"*6
        return line.format(self.id, self.x, self.y, self.rate_arcsec, self.rate_pixels, self.angle, self.mag)


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
            try:	
                x = float(measures[provisional][0].comment.X)
                y = float(measures[provisional][0].comment.Y)
            except Exception as e:
                sys.stderr.write(str(e))
                sys.stderr.write(str(provisional))
            dist = math.sqrt( (x - planted_object.x)**2 + (y-planted_object.y)**2)
            if dist < 10.0:
                # flag as a matched measure
                confused_measure[provisional] = planted_object
                planted_object.confused += 1
            if measure_dist is None or measure_dist > dist:
                measure_dist = dist
                measure_source = measures[provisional]

        # determine if planted_object was found
        if cand_dist < 10.0:
            # In candidate list
            if measure_dist is not None and measure_dist < 10.0:
                # accepted.
                planted_object.recovered = measure_source
                planted_object.false_negative = None
                planted_object.confused -= 1
                del(confused_measure[measure_source[0].provisional_name])
                matched[measure_source[0].provisional_name] = True
            else:
                # rejected.
                planted_object.false_negative = cand_source
                false_negative_sources.append(cand_source)

    matches_fptr.write( ("## F: found\n"
                         "## M: multiple matches\n"
                         "## C: other match of confused multiple match\n"
                         "## N: not found\n"
                         "## P: false Positive \n") )
    matches_fptr.write("{} {} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}\n".format(
        "Key",planted_object_file.header,"x_dao","y_dao", "rate_mes", "ang_mes", "mag1_dao","merr1_dao",
        "mag2_dao","merr2_dao", "mag3_dao","merr3_dao" ))


    for planted_object in planted_objects:
        if planted_object.recovered is not None:
            confused = "F"
            if planted_object.confused > 1 :
                confused = "M"
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

            matches_fptr.write("{:3s} {} {:8.2f} {:8.2f} {:8.2f} {:8.2f} ".format(
            confused, str(planted_object), x, y, rate, angle))

            # record the three discovery magnitudes
            for idx in range(3):
                try:
                    mag = float(measure[idx].comment.mag)
                    merr = float(measure[idx].comment.mag_uncertainty)
                except Exception as e:
                    mag = -1.0
                    merr = -1.0
                    logger.warning(str(e))
                matches_fptr.write("{:8.2f} {:8.2f} ".format(mag,merr))
            matches_fptr.write("\n")

        elif planted_object.false_negative is not None:
            source = planted_object.false_negative
            reading = source.get_reading(0)
            third = source.get_reading(2)
            cutout = image_slice_downloader.download_cutout(reading, needs_apcor=True)

            start_jd = mpc.Time(reading.obs.header['MJD_OBS_CENTER'],format='mpc', scale='utc').jd
            end_jd = mpc.Time(third.obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd

            rate = math.sqrt((third.x - reading.x)**2 + (third.y - reading.y)**2)/(
                24*(end_jd - start_jd) )
            angle = math.degrees(math.atan2(third.y - reading.y,third.x - reading.x))
            matches_fptr.write("{:3s} {} {:8.2f} {:8.2f} {:8.2f} {:8.2f} ".format(
            "N", str(planted_object), reading.x, reading.y, rate, angle))

            for idx in range(3):
                try:
                    (x, y, mag, merr) = cutout.get_observed_magnitude()
                except TaskError as e:
                    logger.warning(str(e))
                    mag = -1.0
                    merr = -1.0
                matches_fptr.write("{:8.2f} {:8.2f} ".format(mag,merr))
            matches_fptr.write("\n")


        else:
            matches_fptr.write("{:3s} {}".format("X",str(planted_object)))
            matches_fptr.write(10*" {:8.2f}".format(0.0))
            matches_fptr.write("\n")

        matches_fptr.flush()




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
            confused = "C"
            planted_object = confused_measure[provisional]
        else:
            confused = "P"
            planted_object = " {:4d}".format(-1)+6*" {:8.2f}".format(0)
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

        # record the three discovery magnitudes
        matches_fptr.write("{:3s} {} {:8.2f} {:8.2f} {:8.2f} {:8.2f} ".format(
            confused, planted_object, x, y, rate, angle))


        for idx in range(3):
            try:
                mag = float(measure[idx].comment.mag)
                merr = float(measure[idx].comment.mag_uncertainty)
            except Exception as e:
                mag = -1.0
                merr = -1.0
                logger.warning(str(e))
            matches_fptr.write("{:8.2f} {:8.2f} ".format(mag,merr))
        matches_fptr.write("\n")


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
    return matches_fptr.name

def plot():


    eff_file_list = glob('*.eff')
    mag_found = []
    mag_planted = []
    dmag1 = []
    dmag2 = []
    dmag3 = []
    for eff_file in eff_file_list:
        try:
            d = astropy.io.ascii.read(eff_file)
        except Exception as e:
            sys.stderr.write(str(e))
            sys.stderr.write("{}\n".format(eff_file))
            continue
        mag_found.extend(d['mag_plt'][d['Key']=='F'])
        mag_planted.extend(d['mag_plt'])
        dmag1.extend(d['mag_plt'][d['Key']=='F'] - d['mag1_dao'][d['Key']=='F'])
        dmag2.extend(d['mag_plt'][d['Key']=='F'] - d['mag2_dao'][d['Key']=='F'])
        dmag3.extend(d['mag_plt'][d['Key']=='F'] - d['mag3_dao'][d['Key']=='F'])


    fig = pyplot.figure()
    assert isinstance(fig, pyplot.Figure)
    ax = fig.add_subplot(211)
    ax.plot(mag_found,dmag1,'.')
    ax.plot(mag_found,dmag2,'.')
    ax.plot(mag_found,dmag3,'.')
    ax.plot([21,25],[0,0],'k')

    ax.set_xlabel('mag')
    ax.set_ylabel('merr')
    ax.set_ylim(-0.4,0.4)
    bins=numpy.arange(21,25.2,0.2)
    (found,bin_edges) = numpy.histogram(mag_found,bins=bins)
    (planted,bin_edges) = numpy.histogram(mag_planted,bins=bins)
    f = 1.0*found/(1.0*planted)
    print f
    ax = fig.add_subplot(212)
    ax.plot(bin_edges[1:],f,'o')
    ax.set_xlabel('mag')
    ax.set_ylabel('frac')
    fig.savefig('diag.pdf')



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

    eff_file_list = []
    for cands in cands_filelist:
        cands_filename = os.path.basename(cands)
        if os.path.exists(cands_filename+".eff"):
            logger.info("{}.eff exists, skipping".format(cands_filename))
            continue
        reals_filename = reals_filepath + cands_filename.replace('cands','reals')
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

        eff_file_list.append(match_planted(cands, measures))
    plot()
