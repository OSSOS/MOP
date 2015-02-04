# !/usr/bin/env python
# ###############################################################################
# #                                                                            ##
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
"""Populate the dbimages directory with new images acquired by this project"""

import argparse

import ephem

from ossos import storage
from planning.ObsStatus import OSSOS_RUNIDS, SURVEY_START, query_for_observations


def import_lastrun_data(opt):
    mjd_yesterday = ephem.date(ephem.julian_date(ephem.date(opt.date))) - 2400000.5

    data_to_date = query_for_observations(mjd_yesterday, opt.cal, OSSOS_RUNIDS)
    for row in data_to_date:
        storage.populate(row['dataset_name'])


def main():
    parser = argparse.ArgumentParser(description="Make links in the dbimages directory for a given set of observations")
    parser.add_argument('date', nargs='?', action='store', default=SURVEY_START)
    parser.add_argument('--cal', action='store', default=1)
    opt = parser.parse_args()

    import_lastrun_data(opt)


if __name__ == '__main__':
    main()
