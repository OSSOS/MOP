#!python

import argparse
import logging
import os
import re
import sys

from ossos import mpc
from astropy import units
from astropy.time import Time
from astropy.coordinates.angles import Angle
from ossos.gui import config

rename_map = {}


def load_observations(xxx_todo_changeme, path, filenames):
    """
    Returns a provisional name based dictionary of observations of the object.
    Each observations is keyed on the date. ie. a dictionary of dictionaries.

    @param path: the directory where filenames are.
    @type path str
    @param filenames: list of files in path.
    @type filenames list
    @rtype  None
    """
    (observations, regex, rename) = xxx_todo_changeme
    for filename in filenames:
        if re.search(regex, filename) is None:
            logging.error("Skipping {}".format(filename))
            continue
        obs = mpc.MPCReader().read(os.path.join(path, filename))
        for ob in obs:
            if "568" not in ob.observatory_code:
                continue
            if not isinstance(ob.comment, mpc.OSSOSComment):
                continue
            if ob.date < Time("2013-01-01 00:00:00"):
                continue
            if rename:
                new_provisional_name = os.path.basename(filename)
                new_provisional_name = new_provisional_name[0:new_provisional_name.find(".")]
                rename_map[ob.provisional_name] = new_provisional_name
            try:
                key1 = ob.comment.frame.split('p')[0]
            except Exception as ex:
                logger.warning(str(ex))
                logger.warning(ob.to_string())
                continue
            key2 = ob.provisional_name
            if key1 not in observations:
                observations[key1] = {}
            if key2 in observations[key1]:
                if observations[key1][key2]:
                    continue
                if not observation.null_observation:
                    logger.error(filename)
                    logger.error(str(observations[key1][key2]))
                    raise ValueError("conflicting observations for {} in {}".format(key2, key1))
            observations[key1][key2] = ob

def main():

    description = ("Given two directories containing astrometric observations build"
                   "file that reports the newly measured astrometry.")

    epilog = """This script is intended to provide a simple way of gathering a list of new astrometry for reporting.
Astrometry is reported to the a central service for distribution (dbase fro the OSSOS project, or the
Minor Planet Center, for example). These central system do not want previously reported astrometry in
new submission files. Also, when a line of astrometry needs to be changed there is, nominally, a separate channel
for this reporting.  This script attempts to provide an automated approach to creating these reports.

In addition to the reported astrometry, each file should also contain some comments on the nature of the observations
being submitted. This script, being part of the OSSOS pipeline, provides these comments in a way that is consistent
with the expectations of the OSSOS reporting structure.  The script should build these comments in a reasonably
auto-magical way.
"""

    app_config = config.AppConfig()
    parser = argparse.ArgumentParser(description=description, epilog=epilog)
    parser.add_argument("existing_astrometry_directory",
                        help=("Directory containing previously reported astrometry."
                              "Files can be in dbase ast, gui ast or OSSOS mpc format"))
    parser.add_argument("--idx_filename",
                        default=None,
                        help="File that has the MOP/OSSOS name mapping index.")
    parser.add_argument("new_astrometry_directory",
                        help=("Directory containing astrometry to be searched for new lines to report."
                              "Files should be in gui ast format."
                              "Only observations from a single observatory should be present in these files."))
    parser.add_argument("report_file",
                        help="Name of file that new lines will be reported to")
    parser.add_argument("--rename", action="store_true",
                        help="Rename objects in new measurement files based on the name of the file")
    parser.add_argument("--new_name_regex",
                        default='.*\.ast',
                        help="Only load new observations where provisional name matches")
    parser.add_argument("--existing_name_regex",
                        default='.*\.ast',
                        help="Only load existing observations where provisional name matches")
    parser.add_argument("--start_date", type=mpc.get_date,
                        help="Include observations taken on or after this date in report.")
    parser.add_argument("--end_date", type=mpc.get_date,
                        help="Include observation taken on or before this date in report.")
    parser.add_argument("--replacement", action='store_true', help="select replacement lines")
    parser.add_argument("--tolerance", default=1,
                        help="""tolerance is a flag, if 1 then only report lines that have different
                        positions and/flux, if -1 then any change cuases a report line""")
    parser.add_argument("--COD", default=None,
                        help="Observatory code for report file.")
    parser.add_argument("-q",
                        action="store_true", help="Run quiet")
    parser.add_argument("--OBS", 
                        action="append",
                        help="Names of observers, multiple allowed")
    parser.add_argument("--MEA", 
                        action="append",
                        help="Names of measures, multiple allowed")
    parser.add_argument("--TEL", default=app_config.read("TELESCOPE"),
                        action="store")
    parser.add_argument("--NET", default=app_config.read("ASTROMETRIC_NETWORK"),
                        action="store")


    args = parser.parse_args()
    tolerance = Angle(float(args.tolerance) * units.arcsec)

    logger = logging.getLogger('reporter')
    if args.q:
        logger.setLevel(logging.CRITICAL)

    if args.idx_filename is None:
        args.idx_filename = os.path.dirname(args.existing_astrometry_directory) + "/idx/file.idx"

    idx = mpc.Index(args.idx_filename)

    existing_observations = {}
    os.path.walk(args.existing_astrometry_directory, load_observations,
                 (existing_observations, args.existing_name_regex, False))
    logger.info("Loaded existing observations for {} objects.\n".format(len(existing_observations)))

    new_observations = {}
    os.path.walk(args.new_astrometry_directory, load_observations,
                 (new_observations, args.new_name_regex, args.rename))
    logger.info("Loaded new observations for {} objects.\n".format(len(new_observations)))

    report_observations = {}
    for frame1 in new_observations:
        for name1 in new_observations[frame1]:
            observation1 = new_observations[frame1][name1]
            logger.warning("Checking {}".format(observation1.to_string()))
            assert isinstance(observation1, mpc.Observation)
            if ((args.start_date is None or args.start_date.jd < observation1.date.jd)
                    and (args.end_date is None or args.end_date.jd > observation1.date.jd)):
                report = True
                replacement = False
                if frame1 in existing_observations:
                    for name2 in existing_observations[frame1]:
                        observation2 = existing_observations[frame1][name2]
                        assert isinstance(observation2, mpc.Observation)
                        replacement = False
                        if idx.is_same(observation2.provisional_name, observation1.provisional_name):
                            if ((tolerance < 0 and observation1 != observation2) or
                                    (observation1.mag != observation2.mag or
                                        observation1.ra != observation2.ra or
                                        observation1.dec != observation2.dec)):
                                if not observation2.discovery:
                                    replacement = True
                                else:
                                    print("discovery")
                            else:
                                report = False
                            break
                if report and replacement == args.replacement:
                    logger.warning("Adding {} on {} to report".format(name1, observation1.date))
                    report_observations[name1] = report_observations.get(name1, [])
                    report_observations[name1].append(observation1)

    if not len(report_observations) > 0:
        logger.warning("No observations matched criterion.")
        sys.exit(0)

    if args.report_file == "-":
        outfile = sys.stdout
    else:
        outfile = open(args.report_file, 'w')

    full_observation_list = []
    for name in report_observations:
        full_observation_list.extend(report_observations[name])

    observers = args.OBS is not None and len(args.OBS) > 0 and args.OBS or None
    measurers = args.MEA is not None and len(args.MEA) > 0 and args.MEA or None
    network = len(args.NET) > 0 and args.NET or None
    telescope = len(args.TEL) > 0 and args.TEL or None

    outfile.write(mpc.make_tnodb_header(full_observation_list, 
                                        observers=observers,
                                        measurers=measurers,
                                        telescope=telescope,
                                        astrometric_network=network))
    outfile.write("\n")

    for name in report_observations:
        # sorted("This is a test string from Andrew".split(), key=str.lower)
        report_observations[name].sort(key=lambda x: x.date.jd)
        for observation in report_observations[name]:
            if name in rename_map:
                observation.provisional_name = rename_map[name]
            outfile.write(observation.to_tnodb() + "\n")
        outfile.write("\n")
    outfile.close()


if __name__ == '__main__':
    sys.exit(main())
