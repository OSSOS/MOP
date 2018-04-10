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
"""
Match .cands.comb file objects with .measure3.reals.astrom to create an classifcation learning set.

eg, compare: 15BS+0+0_p13.cands.comb and 15BS+0+0_p13.measure3.reals.astrom

the result is a file with a single row for all three measurements in cands.comb and a 1 or 0 at the end to indicate class.

"""
from astropy.io import ascii

__author__ = 'jjk'

import sys
from ossos import mop_file
from ossos.match import match_mopfiles

import argparse
import logging

logger = logging.getLogger('vos')
logger.setLevel(logging.INFO)
logger.addHandler(logging.StreamHandler())

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('astrom_filename')
    parser.add_argument('cands_filename')
    parser.add_argument('--measure3', default='vos:OSSOS/measure3/')

    args = parser.parse_args()

    real_detections = mop_file.MOPFile(filename=args.astrom_filename)
    cand_detections = mop_file.MOPFile(filename=args.cands_filename)

    classification_filename = args.cands_filename+".cls"

    ascii.write(match_mopfiles(cand_detections, real_detections).data, classification_filename,
                format='commented_header', overwrite=True)


if __name__ == '__main__':
    code = main()
    sys.exit(code)
