#!python
"""
Compare the measured fluxes of planted sources against those returned for by digiphot.
"""
import pprint
import numpy
import sys

__author__ = 'jjk'

import math
import os

from ossos import astrom
from ossos.mpc import Time
from ossos.downloads.cutouts import ImageCutoutDownloader
from ossos import storage
import argparse
import logging

from astropy.io import ascii

from astropy.table import MaskedColumn, Table

logger = logging.getLogger('vos')
logger.setLevel(logging.CRITICAL)
logger.addHandler(logging.StreamHandler())

BRIGHT_LIMIT = 23.0
OBJECT_PLANTED = "Object.planted"
MINIMUM_BRIGHT_DETECTIONS = 5
MINIMUM_BRIGHT_FRACTION = 0.5
MATCH_TOLERANCE = 100.0

# This image_slice_downloader handles pulling a small cutout of the image from VOSpace so we can do photometry
# on this small chunk of the image.
image_slice_downloader = ImageCutoutDownloader(slice_rows=60, slice_cols=60)


def match_lists(pos1, pos2, tolerance=MATCH_TOLERANCE):
    """
    Given two sets of x/y positions match the lists, uniquely.

    :rtype : numpy.ma, numpy.ma
    :param pos1: list of x/y positions.
    :param pos2: list of x/y positions.
    :param tolerance: float distance, in pixels, to consider a match

    Algorithm:
        - Find all the members of pos2 that are within tolerance of pos1[idx1].
                These pos2 members are match_group_1
        - Find all the members of pos1 that are within tolerance of match_group_1[idx2].
                These pos1 members are match_group_2
        - If pos1[idx] is in match_group_2 then pos1[idx] is a match of object at match_group_1[idx2]

    """

    assert isinstance(pos1, numpy.ndarray)
    assert isinstance(pos2, numpy.ndarray)

    # build some arrays to hold the index of things that matched between lists.
    npts1 = len(pos1[:, 0])
    pos1_idx_array = numpy.arange(npts1, dtype=numpy.int16)
    npts2 = len(pos2[:, 0])
    pos2_idx_array = numpy.arange(npts2, dtype=numpy.int16)

    # this is the array of final matched index, -1 indicates no match found.
    match1 = numpy.ma.zeros(npts1, dtype=numpy.int16)
    match1.mask = True

    # this is the array of matches in pos2, -1 indicates no match found.
    match2 = numpy.ma.zeros(npts2, dtype=numpy.int16)
    match2.mask = True

    for idx1 in range(npts1):

        # compute the distance source idx1 to each member of pos2
        sep = numpy.sqrt((pos2[:, 0] - pos1[idx1, 0]) ** 2 + (pos2[:, 1] - pos1[idx1, 1]) ** 2)

        # considered a match if sep is below tolerance and is the closest match available.
        match_condition = numpy.all((sep <= tolerance, sep == sep.min()), axis=0)

        # match_group_1 is list of the indexes of pos2 entries that qualified as possible matches to pos1[idx1]
        match_group_1 = pos2_idx_array[match_condition]

        # For each of those pos2 objects that could be a match to pos1[idx] find the best match in all of pos1
        for idx2 in match_group_1:
            # compute the distance from this pos2 object that is a possible match to pos1[idx1] to all members of pos1
            sep = numpy.sqrt((pos1[:, 0] - pos2[idx2, 0]) ** 2 + (pos1[:, 1] - pos2[idx2, 1]) ** 2)

            # considered a match if sep is below tolerance and is the closest match available.
            match_condition = numpy.all((sep <= tolerance, sep == sep.min()), axis=0)
            match_group_2 = pos1_idx_array[match_condition]

            # Are any of the pos1 members that were matches to the matched pos2 member the pos1[idx] entry?
            if idx1 in match_group_2:
                match1[idx1] = idx2
                match2[idx2] = idx1
                # this BREAK is in here since once we have a match we're done.
                break

    return match1, match2


def measure_mags(measures, table_row):
    try:
        start_jd = Time(measures[0].obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd
        end_jd = Time(measures[-1].obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd
        table_row['measure_x'] = measures[0].x
        table_row['measure_y'] = measures[0].y

        rate = math.sqrt((measures[-1].x - measures[0].x) ** 2 + (measures[-1].y - measures[0].y) ** 2) / (
            24 * (end_jd - start_jd))
        rate = int(rate * 100) / 100.0
        table_row['measure_rate'] = rate
        angle = math.degrees(math.atan2(measures[-1].y - measures[0].y, measures[-1].x - measures[0].x))
        angle = int(angle * 100) / 100.0
        table_row['measure_angle'] = angle
    except Exception as err:
        logger.debug("ERROR: " + str(err))
        pass

    for ridx in range(3):
        measures[ridx].is_inverted = False
        cutout = image_slice_downloader.download_cutout(measures[ridx], needs_apcor=True)
        result = None
        try:
            result = cutout.get_observed_magnitude()
            (x, y, mag, merr) = result
            table_row['measure_mag{}'.format(ridx + 1)] = mag
            table_row['measure_merr{}'.format(ridx + 1)] = merr
        except Exception as e:
            logger.debug("get_observed_magnitude returned: {}".format(result))
            logger.debug("Failed while computing magnitude for source")
            logger.debug(pprint.pformat(measures))
            logger.debug(str(e))
            pass
    return table_row


def match_planted(fk_candidate_observations, match_filename, bright_limit=BRIGHT_LIMIT, object_planted=OBJECT_PLANTED,
                  minimum_bright_detections=MINIMUM_BRIGHT_DETECTIONS, bright_fraction=MINIMUM_BRIGHT_FRACTION):
    """
    Using the fk_candidate_observations as input get the Object.planted file from VOSpace and match
    planted sources with found sources.

    The Object.planted list is pulled from VOSpace based on the standard file-layout and name of the
    first exposure as read from the .astrom file.

    :param fk_candidate_observations: name of the fk*reals.astrom file to check against Object.planted
    :param match_filename: a file that will contain a list of all planted sources and the matched found source

    """

    found_pos = []
    detections = fk_candidate_observations.get_sources()
    for detection in detections:
        reading = detection.get_reading(0)
        # create a list of positions, to be used later by match_lists
        found_pos.append([reading.x, reading.y])

    # Now get the Object.planted file, either from the local FS or from VOSpace.
    objects_planted_uri = object_planted
    if not os.access(objects_planted_uri, os.F_OK):
        objects_planted_uri = fk_candidate_observations.observations[0].get_object_planted_uri()
    lines = storage.open_vos_or_local(objects_planted_uri).read()

    # we are changing the format of the Object.planted header to be compatible with astropy.io.ascii but
    # there are some old Object.planted files out there so we do these string/replace calls to reset those.
    new_lines = lines.replace("pix rate", "pix_rate")
    new_lines = new_lines.replace("""''/h rate""", "sky_rate")
    rdr = ascii.get_reader(Reader=ascii.CommentedHeader)
    planted_objects_table = rdr.read(new_lines)

    # The match_list method expects a list that contains a position, not an x and a y vector, so we transpose.
    planted_pos = numpy.transpose([planted_objects_table['x'].data, planted_objects_table['y'].data])

    # match_idx is an order list.  The list is in the order of the first list of positions and each entry
    # is the index of the matching position from the second list.
    (match_idx, match_fnd) = match_lists(numpy.array(planted_pos), numpy.array(found_pos))
    assert isinstance(match_idx, numpy.ma.MaskedArray)
    assert isinstance(match_fnd, numpy.ma.MaskedArray)

    false_positives_table = Table()

    # Once we've matched the two lists we'll need some new columns to store the information in.
    # these are masked columns so that object.planted entries that have no detected match are left 'blank'.
    new_columns = [MaskedColumn(name="measure_x", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_y", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_rate", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_angle", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_mag1", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_merr1", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_mag2", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_merr2", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_mag3", length=len(planted_objects_table), mask=True),
                   MaskedColumn(name="measure_merr3", length=len(planted_objects_table), mask=True)]
    planted_objects_table.add_columns(new_columns)
    tlength = 0
    new_columns = [MaskedColumn(name="measure_x", length=tlength, mask=True),
                   MaskedColumn(name="measure_y", length=tlength, mask=True),
                   MaskedColumn(name="measure_rate", length=0, mask=True),
                   MaskedColumn(name="measure_angle", length=0, mask=True),
                   MaskedColumn(name="measure_mag1", length=0, mask=True),
                   MaskedColumn(name="measure_merr1", length=0, mask=True),
                   MaskedColumn(name="measure_mag2", length=0, mask=True),
                   MaskedColumn(name="measure_merr2", length=0, mask=True),
                   MaskedColumn(name="measure_mag3", length=tlength, mask=True),
                   MaskedColumn(name="measure_merr3", length=tlength, mask=True)]
    false_positives_table.add_columns(new_columns)
    print len(false_positives_table)

    # We do some 'checks' on the Object.planted match to diagnose pipeline issues.  Those checks are made using just
    # those planted sources we should have detected.
    bright = planted_objects_table['mag'] < bright_limit
    n_bright_planted = numpy.count_nonzero(planted_objects_table['mag'][bright])

    for idx in range(len(match_idx)):
        # The match_idx value is False if nothing was found.
        if not match_idx.mask[idx]:
            # Each 'source' has multiple 'readings'
            measures = detections[match_idx[idx]].get_readings()
            planted_objects_table[idx] = measure_mags(measures, planted_objects_table[idx])

    for idx in range(len(match_fnd)):
        if match_fnd.mask[idx]:
            measures = detections[idx].get_readings()
            false_positives_table.add_row()
            false_positives_table[-1] = measure_mags(measures, false_positives_table[-1])

    # Count an object as detected if it has a measured magnitude in the first frame of the triplet.
    n_bright_found = numpy.count_nonzero(planted_objects_table['measure_mag1'][bright])
    # Also compute the offset and standard deviation of the measured magnitude from that planted ones.
    offset = numpy.mean(planted_objects_table['mag'][bright] - planted_objects_table['measure_mag1'][bright])
    try:
        offset = "{:5.2f}".format(offset)
    except:
        offset = "indef"

    std = numpy.std(planted_objects_table['mag'][bright] - planted_objects_table['measure_mag1'][bright])
    try:
        std = "{:5.2f}".format(std)
    except:
        std = "indef"

    fout = storage.open_vos_or_local(match_filename, 'w')
    fout.write("#K {:10s} {:10s}\n".format("EXPNUM", "FWHM"))
    for measure in detections[0].get_readings():
        fout.write('#V {:10s} {:10s}\n'.format(measure.obs.header['EXPNUM'], measure.obs.header['FWHM']))

    fout.write("#K ")
    for keyword in ["RMIN", "RMAX", "ANGLE", "AWIDTH"]:
        fout.write("{:10s} ".format(keyword))
    fout.write("\n")

    fout.write("#V ")
    for keyword in ["RMIN", "RMAX", "ANGLE", "AWIDTH"]:
        fout.write("{:10s} ".format(fk_candidate_observations.sys_header[keyword]))
    fout.write("\n")

    fout.write("#K ")
    for keyword in ["NBRIGHT", "NFOUND", "OFFSET", "STDEV"]:
        fout.write("{:10s} ".format(keyword))
    fout.write("\n")
    fout.write("#V {:<10} {:<10} {:<10} {:<10}\n".format(n_bright_planted,
                                                         n_bright_found,
                                                         offset,
                                                         std))
    fpout = storage.open_vos_or_local(match_filename+".fp", 'w')
    try:
        writer = ascii.FixedWidth
        # add a hash to the start of line that will have header columns: for JMP
        fout.write("#")
        ascii.write(planted_objects_table, output=fout, Writer=writer, delimiter=None)
        fpout.write("#")
        if len(false_positives_table) > 0:
            ascii.write(false_positives_table, output=fpout, Writer=writer, delimiter=None)
        else:
            fpout.write(" no false positives\n")
    except Exception as e:
        print e
        print str(e)
        raise e
    finally:
        fout.close()
        fpout.close()

    # Some simple checks to report a failure how we're doing.
    if n_bright_planted < minimum_bright_detections:
        raise RuntimeError(1, "Too few bright objects planted.")

    if n_bright_found / float(n_bright_planted) < bright_fraction:
        raise RuntimeError(2, "Too few bright objects found.")

    return "{} {} {} {}".format(n_bright_planted, n_bright_found, offset, std)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('field')
    parser.add_argument('ccd')
    parser.add_argument('--expnum', default=None, help="Which exposure is the lead for this astrom file?")
    parser.add_argument('--astrom-filename', default=None, help="Give the astrom file directly instead of looking-up "
                                                                "using the field/ccd naming scheme.")
    parser.add_argument('--reals', action='store_true', default=False)
    parser.add_argument('--type', choices=['o', 'p', 's'], help="Which type of image.", default='s')
    parser.add_argument('--measure3', default='vos:OSSOS/measure3/2013B-L_redo/')
    parser.add_argument('--dbimages', default=None)
    parser.add_argument('--dry-run', action='store_true', default=False)
    parser.add_argument('--force', action='store_true', default=False)

    parser.add_argument('--object-planted', default=OBJECT_PLANTED,
                        help="Name of file contains list of planted objects.")
    parser.add_argument('--bright-limit', default=BRIGHT_LIMIT,
                        help="Sources brighter than this limit {} are used to diagnose planting issues.".format(
                            BRIGHT_LIMIT))
    parser.add_argument('--minimum-bright-detections', default=MINIMUM_BRIGHT_DETECTIONS,
                        help="required number of detections with mag brighter than bright-limit.")
    parser.add_argument('--minimum-bright-fraction', default=MINIMUM_BRIGHT_FRACTION,
                        help="minimum fraction of objects above bright limit that should be found.")
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    prefix = 'fk'
    ext = args.reals and 'reals' or 'cands'

    storage.MEASURE3 = args.measure3

    if args.dbimages is not None:
        storage.DBIMAGES = args.dbimages
        astrom.DATASET_ROOT = args.dbimages

    astrom_uri = storage.get_cands_uri(args.field,
                                       ccd=args.ccd,
                                       version=args.type,
                                       prefix=prefix,
                                       ext="measure3.{}.astrom".format(ext))

    if args.astrom_filename is None:
        astrom_filename = os.path.basename(astrom_uri)
    else:
        astrom_filename = args.astrom_filename

    if not os.access(astrom_filename, os.F_OK):
        astrom_filename = os.path.dirname(astrom_uri) + "/" + astrom_filename

    # Load the list of astrometric observations that will be looked at.
    fk_candidate_observations = astrom.parse(astrom_filename)
    if args.expnum is None:
        expnum = fk_candidate_observations.observations[0].expnum
    else:
        expnum = args.expnum

    storage.set_logger(os.path.splitext(os.path.basename(sys.argv[0]))[0], prefix, expnum, "", ext, args.dry_run)

    match_filename = os.path.splitext(os.path.basename(astrom_filename))[0] + '.match'

    exit_status = 0
    status = storage.SUCCESS
    try:
        if (not storage.get_status(expnum, ccd=args.ccd, program='astrom_mag_check', version='')) or args.force:
            message = match_planted(fk_candidate_observations,
                                    match_filename=match_filename,
                                    object_planted=args.object_planted,
                                    bright_limit=args.bright_limit,
                                    minimum_bright_detections=args.minimum_bright_detections,
                                    bright_fraction=args.minimum_bright_fraction)
            match_uri = storage.get_cands_uri(args.field,
                                              ccd=args.ccd,
                                              version=args.type,
                                              prefix=prefix,
                                              ext="measure3.{}.match".format(ext))
            if not args.dry_run:
                storage.copy(match_filename, match_uri)
                uri = os.path.dirname(astrom_uri)
                keys = [storage.tag_uri(os.path.basename(astrom_uri))]
                values = [message]
                storage.set_tags_on_uri(uri, keys, values)
    except Exception as err:
        sys.stderr.write(str(err))
        status = str(err)
        exit_status = err.message

    if not args.dry_run:
        storage.set_status(expnum, args.ccd, 'astrom_mag_check', version='', status=status)

    return exit_status


if __name__ == '__main__':
    sys.exit(main())
