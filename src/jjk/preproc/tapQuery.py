#!/usr/bin/env python

import argparse
import urllib, ephem, datetime, atpy, tempfile
import sys

parser = argparse.ArgumentParser(description="Query the CADC for OSSOS observations.")
parser.add_argument('date', action='store',default='2013-01-01')
parser.add_argument('--runid', nargs='*', action='store', default= list(('13AP05','13AP06')))
parser.add_argument('--cal', action='store', default="RAW")
opt = parser.parse_args()

runids = tuple(opt.runid)

try:
    mjd_yesterday = ephem.date(ephem.julian_date(ephem.date(opt.date))) - 2400000.5 
except Exception as e:
    sys.stderr.write("you said date = %s" %(opt.date))
    sys.stderr.write(str(e))
    sys.exit(-1)

## instead of having specific collumns in the SELECT clause below you could just put a '*' to get all possible columns.
## 
## the 'position_bounds' column provides a polygon that describes the edge of the field of the mosaic, not individual ccds.

data={"QUERY": """SELECT Observation.target_name as TargetName, COORD1(CENTROID(Plane.position_bounds)) AS RA, COORD2(CENTROID(Plane.position_bounds)) AS DEC, Plane.time_bounds_cval1 AS StartDate, Plane.time_exposure AS ExposureTime, Observation.instrument_name AS Instrument, Plane.energy_bandpassName AS Filter, Observation.collectionID AS dataset_name, Observation.proposal_id AS ProposalID, Observation.proposal_pi AS PI FROM caom.Observation AS Observation JOIN caom.Plane AS Plane ON Observation.obsID = Plane.obsID WHERE  ( Observation.collection = 'CFHT' ) AND Plane.time_bounds_cval1 > %d AND Plane.observable_ctype='%s' AND Observation.proposal_id IN %s """ %  ( mjd_yesterday, opt.cal, str(tuple(opt.runid)) ),
      "REQUEST": "doQuery",
      "LANG": "ADQL",
      "FORMAT": "votable"}

url="http://www.cadc.hia.nrc.gc.ca/tap/sync?"+urllib.urlencode(data)

# uncomment this to see the URL
# print data["QUERY"]
# print url

tmpFile = tempfile.NamedTemporaryFile()

urllib.urlretrieve(url,tmpFile.name)


t = atpy.Table()
t.read(tmpFile.name,type='vo')

t.sort('StartDate')


header= "| %20s | %20s | %20s | %20s | %20s |\n"  % ( "EXPNUM", "OBS-DATE", "FIELD", "EXPTIME(s)", "RUNID")
bar = "="*(len(header)-1)+"\n"

sys.stdout.write(bar+header+bar)
for i in range(len(t)):
    print "| %20s | %20s | %20s | %20.1f | %20s |" % ( str(t.dataset_name[i]), str(ephem.date(t.StartDate[i] + 2400000.5- ephem.julian_date(ephem.date(0))))[:20], t.TargetName[i][:20], t.ExposureTime[i], t.ProposalID[i][:20] )

sys.stdout.write(bar)
tmpFile.close()
