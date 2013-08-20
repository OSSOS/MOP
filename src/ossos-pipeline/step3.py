#!/usr/bin/env python
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
'''run step3 of the OSSOS pipeline.'''

import os
import argparse
import logging
from ossos import util
from ossos import storage

def step3(expnums, ccd, version, rate_min,
              rate_max, angle, width, field=None, prefix=None):
    '''run the actual step2  on the given exp/ccd combo'''

    jmp_args = ['step3jmp']
    matt_args = ['step3matt']

    idx = 0
    cmd_args = []
    for expnum in expnums:
        idx += 1
        for ext in ['unid.jmp', 'unid.matt',
                    'trans.jmp' ]:
            filename = storage.get_image(expnum,
                                         ccd=ccd,
                                         version=version,
                                         ext=ext,
                                         prefix=prefix
                                         )
        image = os.path.splitext(os.path.splitext(os.path.basename(filename))[0])[0]
        cmd_args.append('-f%d' % ( idx))
        cmd_args.append(image)

    cmd_args.extend(['-rn', str(rate_min),
                     '-rx', str(rate_max),
                     '-a', str(angle),
                     '-w', str(width)])
    jmp_args.extend(cmd_args)
    matt_args.extend(cmd_args)
    util.exec_prog(jmp_args)
    util.exec_prog(matt_args)


    if field is None:
        field = str(expnums[0])
    storage.mkdir(os.path.dirname(
        storage.get_uri(field,
                        ccd=ccd,
                        version=version,
                        ext=ext,
                        prefix=prefix)))

    for ext in ['moving.jmp', 'moving.matt']:
        uri = storage.get_uri(field,
                              ccd=ccd,
                              version=version,
                              ext=ext,
                              prefix=prefix)
        filename = '%s%d%s%s.%s' % ( prefix, expnums[0],
                                   version,
                                   str(ccd).zfill(2),
                                   ext)
        storage.copy(filename, uri)


    return

if __name__ == '__main__':
    ### Must be running as a script

    parser=argparse.ArgumentParser(
        description='Run step3jmp and step3matt on a given triple.')

    parser.add_argument("--ccd","-c",
                        action="store",
                        default=None,
                        type=int,
                        dest="ccd")
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("expnums",
                        type=int,
                        nargs=3,
                        help="3 expnums to process")
    parser.add_argument("--version",
                        action='version',
                        version='%(prog)s 1.0')
    parser.add_argument('-t','--type',
                        help='which type of image to process',
                        choices=['s','p','o'],
                        default='p'
                        )
    parser.add_argument('--fk', help='Do fakes?', default=False, action='store_true')
    parser.add_argument('--field',
                        help='a string that identifies which field is being searched',
                        nargs='?')
    parser.add_argument('--no-sort',
                        help='preserve input exposure order',
                        action='store_true')
    parser.add_argument("--verbose","-v",
                        action="store_true")
    parser.add_argument("--rate_min", default=0.4,
                        help='minimum rate to accept',
                        type=float)
    parser.add_argument('--rate_max', default=15,
                        help='maximum rate to accept',
                        type=float)
    parser.add_argument('--angle', default=20,
                        help='angle of x/y motion, West is 0, North 90',
                        type=float)
    parser.add_argument('--width', default=30,
                        help='openning angle of search cone',
                        type=float)
    parser.add_argument("--force", action="store_true")

    args=parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    storage._dbimages = args.dbimages

    if args.ccd is None:
        ccdlist = range(0,36)
    else:
        ccdlist = [args.ccd]

    if not args.no_sort:
        args.expnums.sort()

    prefix = ( args.fk and 'fk') or ''

    for ccd in ccdlist:
        message = storage.SUCCESS

        try:
            if not storage.get_status(args.expnums[0], ccd, prefix+'step2'):
                raise IOError(35, "did step2 run on %s" % ( str(args.expnums)))
            if storage.get_status(args.expnums[0], ccd, prefix+'step3') and not args.force:
                logging.critical("step3 alread ran on expnum :%s, ccd: %d" % (
                        str(args.expnums), ccd))
                continue
            step3(args.expnums, ccd, version=args.type,
                  rate_min=args.rate_min,
                  rate_max=args.rate_max,
                  angle=args.angle,
                  width=args.width,
                  field=args.field,
                  prefix=prefix)
        except Exception as e:
            message = str(e)

        logging.error(message)

        storage.set_status(args.expnums[0],
                           ccd,
                           prefix+'step3',
                           message)
        
            


