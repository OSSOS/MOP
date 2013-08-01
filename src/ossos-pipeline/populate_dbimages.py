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
"""Populate the dbimages directory with new images acquired by this project"""


import argparse
import urllib, datetime, tempfile, math, ephem
from astropy.io.votable import parse
from astropy.io.votable.tree import Field
import sys
import time
from ossos import storage
import errno



parser = argparse.ArgumentParser(description="Make links in the dbimages directory for a given set of observations")
parser.add_argument('date', nargs='?', action='store',default='2013-01-01')
parser.add_argument('--runid', nargs='*', action='store', default= list(('13AP05','13AP06')))
parser.add_argument('--cal', action='store', default="RAW")
opt = parser.parse_args()

runids = tuple(opt.runid)

mjd_yesterday = 56401.5841892

data={"QUERY": """SELECT Observation.target_name as TargetName, COORD1(CENTROID(Plane.position_bounds)) AS RA, COORD2(CENTROID(Plane.position_bounds)) AS DEC, Plane.time_bounds_cval1 AS StartDate, Plane.time_exposure AS ExposureTime, Observation.instrument_name AS Instrument, Plane.energy_bandpassName AS Filter, Observation.collectionID AS dataset_name, Observation.proposal_id AS ProposalID, Observation.proposal_pi AS PI FROM caom.Observation AS Observation JOIN caom.Plane AS Plane ON Observation.obsID = Plane.obsID WHERE  ( Observation.collection = 'CFHT' ) AND Plane.time_bounds_cval1 > %d AND Plane.observable_ctype='%s' AND Observation.proposal_id IN %s """ %  ( mjd_yesterday, opt.cal, str(tuple(opt.runid)) ),
      "REQUEST": "doQuery",
      "LANG": "ADQL",
      "FORMAT": "votable"}

url="http://www.cadc.hia.nrc.gc.ca/tap/sync?"+urllib.urlencode(data)

tmpFile = tempfile.NamedTemporaryFile()

urllib.urlretrieve(url,tmpFile.name)


vot = parse(tmpFile.name).get_first_table()
vot.array.sort(order='StartDate')
t=vot.array


for row in t.data:
    storage.populate(row['dataset_name'])
