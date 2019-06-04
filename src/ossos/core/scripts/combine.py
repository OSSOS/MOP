#!python 
################################################################################
##                                                                            ##
## Copyright 2013 by its authors                                              ##
## See COPYING, AUTHORS                                                       ##
##                                                                            ##
## This file is part of OSSOS Moving Object Pipeline (OSSOS-MOP)              ##
##                                                                            ##
##    OSSOS-MOP is free software: you can redistribute it and/or modify       ##
##    it under the terms of the GNU General Public License as published by    ##
##    the Free Software Foundation, either version 3 of the License, or       ##
##    (at your option) any later version.                                     ##
##                                                                            ##
##    OSSOS-MOP is distributed in the hope that it will be useful,            ##
##    but WITHOUT ANY WARRANTY; without even the implied warranty of          ##
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ##
##    GNU General Public License for more details.                            ##
##                                                                            ##
##    You should have received a copy of the GNU General Public License       ##
##    along with OSSOS-MOP.  If not, see <http://www.gnu.org/licenses/>.      ##
##                                                                            ##
################################################################################

import os
from subprocess import CalledProcessError
import sys
from ossos import storage
from ossos import util
import argparse
import logging

MEASURE3 = 'vos:OSSOS/measure3/'


def combine(expnum, ccd, prefix=None, file_type='p', field=None, measure3=MEASURE3, dry_run=False):
    if field is None:
        field = str(expnum)

    if prefix is not None and len(prefix) > 0:
        field = "%s_%s" % (prefix, field)
    field += "_%s%s" % (str(file_type), str(ccd))

    logging.info("Doing combine on field {}".format(field))

    for ext in ['moving.matt', 'moving.jmp']:
        storage.get_file(expnum, ccd=ccd, version=file_type, ext=ext, prefix=prefix)

    if prefix is not None and len(prefix) > 0:
        storage.get_file('Object', version='', ext='planted', subdir=str(expnum) + "/ccd%s" % (str(ccd).zfill(2)))
    else:
        prefix = ''

    cmd_args = ['comb-list', prefix + str(expnum) + file_type + str(ccd).zfill(2)]
    logging.info(str(cmd_args))
    logging.info(util.exec_prog(cmd_args))
    ext_list = ['cands.comb']
    if prefix is not None and len(prefix) > 0:
        ext_list.extend(['jmp.missed', 'matt.missed',
                         'jmp.found', 'matt.found',
                         'comb.missed', 'comb.found'])

    for ext in ext_list:
        uri = storage.get_uri(expnum,
                              ccd=ccd,
                              prefix=prefix,
                              version=file_type,
                              ext=ext)
        filename = os.path.basename(uri)
        if not os.access(filename, os.R_OK):
            logging.critical("No %s file" % filename)
            continue
        vospace_name = "%s.%s" % (field, ext)
        if not dry_run:
            logging.info("%s -> %s" % (filename, os.path.join(measure3, vospace_name)))
            storage.copy(filename, os.path.join(measure3, vospace_name))

    base_name = prefix + str(expnum) + file_type + str(ccd).zfill(2)
    cands_file = base_name + '.cands.comb'

    if not os.access(cands_file, os.R_OK):
        no_cands_file = (prefix +
                         str(expnum) +
                         file_type +
                         str(ccd).zfill(2) +
                         '.no_candidates')
        open(no_cands_file, 'w').close()
        if not dry_run:
            vospace_name = "%s.no_candidates" % field
            storage.copy(no_cands_file, os.path.join(measure3, vospace_name))

        return storage.SUCCESS

    # get the images we need to compute x/y ra/dec transforms
    # cands_file = mop_file.Parser().parse(cands_file)
    # print cands_file
    # for file_id in cands_file.header.file_ids:
    #     rec_no = cands_file.header.file_ids.index(file_id)
    #     storage.get_image(expnum=cands_file.header.keywords['EXPNUM'][rec_no], ccd=ccd, version=file_type, ext='fits',
    #                       prefix=prefix)

    cmd_args = ['measure3', prefix + str(expnum) + file_type + str(ccd).zfill(2)]
    logging.info("Running measure3")
    logging.info(util.exec_prog(cmd_args))

    if not dry_run:
        filename = base_name + ".measure3.cands.astrom"
        vospace_filename = "%s.measure3.cands.astrom" % field
        storage.copy(filename, os.path.join(measure3, vospace_filename))

    return storage.SUCCESS


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

    args = parser.parse_args()

    level = logging.CRITICAL
    if args.debug:
        level = logging.DEBUG
    elif args.verbose:
        level = logging.INFO

    logging.basicConfig(level=level, format="%(message)s")

    storage.DBIMAGES = args.dbimages
    storage.MEASURE3 = args.measure3

    prefix = (args.fk and 'fk') or ''
    task = util.task()
    dependency = 'step3'

    ccd_list = (args.ccd is None and list(range(0, 36))) or [args.ccd]

    exit_code = 0
    for ccd in ccd_list:
        # storage.set_logger(task, prefix, args.expnum, ccd, args.type, args.dry_run)
        try:
        #    if not storage.get_status(dependency, prefix, args.expnum, version=args.type, ccd=ccd) and not args.dry_run:
        #       message = storage.get_status(dependency, prefix, args.expnum, "p", ccd, return_message=True)
        #       raise IOError(35, message)

        #    if storage.get_status(task, prefix, args.expnum, version=args.type, ccd=ccd) and not args.force:
        #       continue
            message = combine(args.expnum,
                              ccd,
                              prefix=prefix,
                              file_type=args.type,
                              field=args.field,
                              measure3=args.measure3,
                              dry_run=args.dry_run)
        except CalledProcessError as cpe:
            message = str(cpe)
            exit_code = message
        except Exception as e:
            message = str(e)
            exit_code = message
        print(message)
        logging.info(message)
        #if not args.dry_run:
        #    storage.set_status(task, prefix, args.expnum, version=args.type, ccd=ccd, status=message)
        return exit_code


if __name__ == '__main__':
    main()
