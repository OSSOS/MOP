###############################################################################
#                                                                            ##
# Copyright 2013 by its authors                                              ##
# See COPYING, AUTHORS                                                       ##
#                                                                            ##
# This file is part of OSSOS Moving Object Pipeline (OSSOS-MOP)              ##
#                                                                            ##
#    OSSOS-MOP is free software: you can redistribute it and/or modify       ##
#    it under the terms of the GNU General Public License as published by    ##
#    the Free Software Foundation, either version 3 of the License, or       ##
#    (at your option) any later version.                                     ##
#                                                                            ##
#    OSSOS-MOP is distributed in the hope that it will be useful,            ##
#    but WITHOUT ANY WARRANTY; without even the implied warranty of          ##
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ##
#    GNU General Public License for more details.                            ##
#                                                                            ##
#    You should have received a copy of the GNU General Public License       ##
#    along with OSSOS-MOP.  If not, see <http://www.gnu.org/licenses/>.      ##
#                                                                            ##
###############################################################################

import os
import sys
from ossos import storage
from ossos import util
import argparse
import logging

MEASURE3 = 'vos:OSSOS/measure3/'
task = util.task()
dependency = 'step3'


def run(expnum, ccd, prefix=None, version='p', field=None,
        measure3=MEASURE3, dry_run=False, force=False):

    message = storage.SUCCESS

    if storage.get_status(task, prefix, expnum, version, ccd) and not force:
        logging.info("{} completed successfully for {} {} {} {}".format(task, prefix, expnum, version, ccd))
        return

    with storage.LoggingManager(task, prefix, expnum, ccd, version, dry_run):
        try:
            if not storage.get_status(dependency, prefix, expnum, version, ccd):
                raise IOError(35, "Cannot start {} as {} not yet completed for {}{}{}{:02d}".format(
                    task, dependency, prefix, expnum, version, ccd))
            if field is None:
                field = str(expnum)

            if prefix is not None and len(prefix) > 0:
                field = "%s_%s" % (prefix, field)
            field += "_%s%s" % (str(version), str(ccd))

            logging.info("Doing combine on field {}".format(field))

            for ext in ['moving.matt', 'moving.jmp']:
                storage.get_file(expnum, ccd=ccd, version=version, ext=ext, prefix=prefix)

            # Get the list of objects planted into the field if prefix='fk'
            if prefix == 'fk':
                storage.get_file('Object',
                                 version='',
                                 ext='planted',
                                 subdir=str(expnum) + "/ccd%s" % (str(ccd).zfill(2)))
            else:
                prefix = ''

            cmd_args = ['comb-list', prefix + str(expnum) + version + str(ccd).zfill(2)]
            logging.info(str(cmd_args))
            logging.info(util.exec_prog(cmd_args))

            # things to copy back to VOSpace, if this is an 'fk' image then we have missed and found files too.
            ext_list = ['cands.comb']
            if prefix == 'fk':
                ext_list.extend(['jmp.missed', 'matt.missed',
                                 'jmp.found', 'matt.found',
                                 'comb.missed', 'comb.found'])

            for ext in ext_list:
                uri = storage.get_uri(expnum,
                                      ccd=ccd,
                                      prefix=prefix,
                                      version=version,
                                      ext=ext)
                filename = os.path.basename(uri)
                if not os.access(filename, os.R_OK):
                    logging.critical("No %s file" % filename)
                    continue
                vospace_name = "%s.%s" % (field, ext)
                if not dry_run:
                    logging.info("%s -> %s" % (filename, os.path.join(measure3, vospace_name)))
                    storage.copy(filename, os.path.join(measure3, vospace_name))

            base_name = prefix + str(expnum) + version + str(ccd).zfill(2)
            cands_file = base_name + '.cands.comb'

            if not os.access(cands_file, os.R_OK):
                no_cands_file = (prefix +
                                 str(expnum) +
                                 version +
                                 str(ccd).zfill(2) +
                                 '.no_candidates')
                open(no_cands_file, 'w').close()
                if not dry_run:
                    vospace_name = "%s.no_candidates" % field
                    storage.copy(no_cands_file, os.path.join(measure3, vospace_name))

                return storage.SUCCESS

            cmd_args = ['measure3', prefix + str(expnum) + version + str(ccd).zfill(2)]
            logging.info("Running measure3: {}".format(str(cmd_args)))
            logging.info(util.exec_prog(cmd_args))

            if not dry_run:
                filename = base_name + ".measure3.cands.astrom"
                vospace_filename = "%s.measure3.cands.astrom" % field
                storage.copy(filename, os.path.join(measure3, vospace_filename))

        except Exception as ex:
            message = str(ex)
            logging.error(message)

        if not dry_run:
            storage.set_status(task, prefix, expnum, version, ccd, status=message)


def main():
    parser = argparse.ArgumentParser(
        description='Run step1jmp and step1matt on a given exposure.')

    parser.add_argument("--ccd", "-c",
                        action="store",
                        default=None,
                        type=int,
                        dest="ccd")
    parser.add_argument("--fk", help="add the fk prefix on processing?",
                        default=False,
                        action='store_true')
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("--field",
                        action="store",
                        default=None,
                        help="Name of the field being combined")
    parser.add_argument("expnum",
                        type=int,
                        help="expnum to process")
    parser.add_argument("--measure3",
                        action="store",
                        help="VOSpace location for measure3 files",
                        default=MEASURE3)
    parser.add_argument('--type', default='p', choices=['o', 'p', 's'], help="which type of image")
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--debug", '-d',
                        action='store_true')
    parser.add_argument("--force", '-f', action='store_true')
    parser.add_argument("--dry-run", action="store_true", help="Don't push back to VOSpace, implies --force")

    cmd_line = " ".join(sys.argv)
    args = parser.parse_args()

    util.set_logger(args)
    logging.info("Starting {}".format(cmd_line))

    storage.DBIMAGES = args.dbimages
    storage.MEASURE3 = args.measure3
    expnum = args.expnum
    version = args.type
    prefix = (args.fk and 'fk') or ''

    if args.ccd is None:
        if int(expnum) < 1785619:
            # Last exposures with 36 CCD Megaprime
            ccdlist = list(range(0, 36))
        else:
            # First exposrues with 40 CCD Megaprime
            ccdlist = list(range(0, 40))
    else:
        ccdlist = [args.ccd]

    for ccd in ccdlist:
        run(args.expnum,
            ccd,
            prefix=prefix,
            version=version,
            field=args.field,
            measure3=args.measure3,
            dry_run=args.dry_run,
            force=args.force)


if __name__ == '__main__':
    main()
