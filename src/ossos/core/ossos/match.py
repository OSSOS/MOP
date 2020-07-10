from astropy.io import ascii
from astropy.table import MaskedColumn, Table, Column
import logging
import math
import numpy
import os
from .downloads.cutouts.downloader import ImageDownloader
from . import util
from .downloads.cutouts.source import SourceCutout
from astropy.time import Time
from .astrom import Observation

BRIGHT_LIMIT = 23.0
OBJECT_PLANTED = "Object.planted"
MINIMUM_BRIGHT_DETECTIONS = 5
MINIMUM_BRIGHT_FRACTION = 0.5


def match_mopfiles(mopfile1, mopfile2):
    """
    Given an input list of 'real' detections and candidate detections provide a result file that contains
    the measured values from candidate detections with a flag indicating if they are real or false.

    @rtype MOPFile
    @return mopfile2 with a new column containing index of matching entry in mopfile1
    """
    pos1 = pos2 = numpy.array([])
    if len(mopfile1.data) > 0:
        X_COL = "X_{}".format(mopfile1.header.file_ids[0])
        Y_COL = "Y_{}".format(mopfile1.header.file_ids[0])
        pos1 = numpy.array([mopfile1.data[X_COL].data, mopfile1.data[Y_COL].data]).transpose()

    if len(mopfile2.data) > 0:
        X_COL = "X_{}".format(mopfile2.header.file_ids[0])
        Y_COL = "Y_{}".format(mopfile2.header.file_ids[0])
        pos2 = numpy.array([mopfile2.data[X_COL].data, mopfile2.data[Y_COL].data]).transpose()

    # match_idx is an order list.  The list is in the order of the first list of positions and each entry
    # is the index of the matching position from the second list.
    match_idx1, match_idx2 =  util.match_lists(pos1, pos2)
    mopfile1.data.add_column(Column(data=match_idx1.filled(-1), name="real", length=len(mopfile1.data)))
    idx = 0
    for file_id in mopfile1.header.file_ids:
        idx += 1
        mopfile1.data.add_column(Column(data=[file_id]*len(mopfile1.data), name="ID_{}".format(idx)))
    return mopfile1


def measure_mags(measures):
    """
    Given a list of readings compute the magnitudes for all sources in each reading.

    @param measures: list of readings
    @return: None
    """
    from . import daophot

    image_downloader = ImageDownloader()

    observations = {}
    for measure in measures:
        for reading in measure:
            if reading.obs not in observations:
                observations[reading.obs] = {'x': [],
                                             'y': [],
                                             'source': image_downloader.download(reading, needs_apcor=True)}
            assert isinstance(reading.obs, Observation)
            observations[reading.obs]['x'].append(reading.x)
            observations[reading.obs]['y'].append(reading.y)

    for observation in observations:
        source = observations[observation]['source']
        assert isinstance(source, SourceCutout)
        hdulist_index = source.get_hdulist_idx(observation.ccdnum)
        #source.update_pixel_location((observations[observation]['x'],
        #                              observations[observation]['y']), hdulist_index)
        observations[observation]['mags'] = daophot.phot(source._hdu_on_disk(hdulist_index),
                                                         observations[observation]['x'],
                                                         observations[observation]['y'],
                                                         aperture=source.apcor.aperture,
                                                         sky=source.apcor.sky,
                                                         swidth=source.apcor.swidth,
                                                         apcor=source.apcor.apcor,
                                                         zmag=source.zmag,
                                                         maxcount=30000,
                                                         extno=0)

    return observations


def match_planted(fk_candidate_observations, match_filename, bright_limit=BRIGHT_LIMIT, object_planted=OBJECT_PLANTED,
                  minimum_bright_detections=MINIMUM_BRIGHT_DETECTIONS, bright_fraction=MINIMUM_BRIGHT_FRACTION):
    """
    Using the fk_candidate_observations as input get the Object.planted file from VOSpace and match
    planted sources with found sources.

    The Object.planted list is pulled from VOSpace based on the standard file-layout and name of the
    first exposure as read from the .astrom file.

    :param fk_candidate_observations: name of the fk*reals.astrom file to check against Object.planted
    :param match_filename: a file that will contain a list of all planted sources and the matched found source
    @param minimum_bright_detections: if there are too few bright detections we raise an error.

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
    lines = open(objects_planted_uri).read()

    # we are changing the format of the Object.planted header to be compatible with astropy.io.ascii but
    # there are some old Object.planted files out there so we do these string/replace calls to reset those.
    new_lines = lines.replace("pix rate", "pix_rate")
    new_lines = new_lines.replace("""''/h rate""", "sky_rate")
    planted_objects_table = ascii.read(new_lines, header_start=-1, data_start=0)
    planted_objects_table.meta = None
    # The match_list method expects a list that contains a position, not an x and a y vector, so we transpose.
    planted_pos = numpy.transpose([planted_objects_table['x'].data, planted_objects_table['y'].data])
    # match_idx is an order list.  The list is in the order of the first list of positions and each entry
    # is the index of the matching position from the second list.
    (match_idx, match_fnd) = util.match_lists(numpy.array(planted_pos), numpy.array(found_pos))
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

    # We do some 'checks' on the Object.planted match to diagnose pipeline issues.  Those checks are made using just
    # those planted sources we should have detected.
    bright = planted_objects_table['mag'] < bright_limit
    n_bright_planted = numpy.count_nonzero(planted_objects_table['mag'][bright])

    measures = []
    idxs = []
    for idx in range(len(match_idx)):
        # The match_idx value is False if nothing was found.
        if not match_idx.mask[idx]:
            # Each 'source' has multiple 'readings'
            measures.append(detections[match_idx[idx]].get_readings())
            idxs.append(idx)

    observations = measure_mags(measures)

    for oidx in range(len(measures)):
        idx = idxs[oidx]
        readings = measures[oidx]
        start_jd = Time(readings[0].obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd
        end_jd = Time(readings[-1].obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd
        rate = math.sqrt((readings[-1].x - readings[0].x) ** 2 + (readings[-1].y - readings[0].y) ** 2) / (
            24 * (end_jd - start_jd))
        rate = int(rate * 100) / 100.0
        angle = math.degrees(math.atan2(readings[-1].y - readings[0].y, readings[-1].x - readings[0].x))
        angle = int(angle * 100) / 100.0
        planted_objects_table[idx]['measure_rate'] = rate
        planted_objects_table[idx]['measure_angle'] = angle
        planted_objects_table[idx]['measure_x'] = observations[readings[0].obs]['mags']["XCENTER"][oidx]
        planted_objects_table[idx]['measure_y'] = observations[readings[0].obs]['mags']["YCENTER"][oidx]
        for ridx in range(len(readings)):
            reading = readings[ridx]
            mags = observations[reading.obs]['mags']
            planted_objects_table[idx]['measure_mag{}'.format(ridx+1)] = mags["MAG"][oidx]
            planted_objects_table[idx]['measure_merr{}'.format(ridx+1)] = mags["MERR"][oidx]

    # for idx in range(len(match_fnd)):
    #     if match_fnd.mask[idx]:
    #         measures = detections[idx].get_readings()
    #         false_positives_table.add_row()
    #         false_positives_table[-1] = measure_mags(measures, false_positives_table[-1])

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

    if os.access(match_filename, os.R_OK):
        fout = open(match_filename, 'a')
    else:
        fout = open(match_filename, 'w')

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

    try:
        writer = ascii.FixedWidth
        # add a hash to the start of line that will have header columns: for JMP
        fout.write("# ")
        fout.flush()
        ascii.write(planted_objects_table, output=fout, Writer=writer, delimiter=None)
        if len(false_positives_table) > 0:
            with open(match_filename+".fp", 'a') as fpout:
                fpout.write("#")
                ascii.write(false_positives_table, output=fpout, Writer=writer, delimiter=None)
    except Exception as e:
        logging.error(str(e))
        raise e
    finally:
        fout.close()

    # Some simple checks to report a failure how we're doing.
    if n_bright_planted < minimum_bright_detections:
        raise RuntimeError(1, "Too few bright objects planted.")

    if n_bright_found / float(n_bright_planted) < bright_fraction:
        raise RuntimeError(2, "Too few bright objects found.")

    return "{} {} {} {}".format(n_bright_planted, n_bright_found, offset, std)

