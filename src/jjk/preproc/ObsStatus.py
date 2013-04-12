#!/usr/bin/env python

import argparse
import urllib, ephem, datetime, atpy, tempfile, math
import sys
import vos, os
import time

parser = argparse.ArgumentParser(description="Query the CADC for OSSOS observations.")
parser.add_argument('date', nargs='?', action='store',default='2013-01-01')
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
t.add_column(name='DateStart', data=-1*t.StartDate)
t.sort('DateStart')

stamp = "#\n# Last Updated: "+time.asctime()+"\n#\n"
header= "| %20s | %20s | %20s | %20s | %20s | %20s | %20s |\n"  % ( "EXPNUM", "OBS-DATE", "FIELD", "EXPTIME(s)", "RA", "DEC", "RUNID")
bar = "="*(len(header)-1)+"\n"

fout = vos.Client().open("vos:OSSOS/ObservingStatus/obsList.txt",mode=os.O_WRONLY)

t2 = None
fout.write(bar+stamp+bar+header)
sys.stdout.write(bar+stamp+bar+header)
for i in range(len(t)):
    sDate = str(ephem.date(t.StartDate[i] + 2400000.5- ephem.julian_date(ephem.date(0))))[:20]
    t1 = time.strptime(sDate,"%Y/%m/%d %H:%M:%S")
    if t2 is None or math.fabs(time.mktime(t2)-time.mktime(t1)) > 3*3600.0:
        sys.stdout.write(bar)
        fout.write(bar)
    t2 = t1
    ra = str(ephem.hours(math.radians(t.RA[i])))
    dec = str(ephem.degrees(math.radians(t.DEC[i])))
    fout.write("| %20s | %20s | %20s | %20.1f | %20s | %20s | %20s |\n" % ( str(t.dataset_name[i]), str(ephem.date(t.StartDate[i] + 2400000.5- ephem.julian_date(ephem.date(0))))[:20], t.TargetName[i][:20], t.ExposureTime[i], ra[:20], dec[:20], t.ProposalID[i][:20] ))
    sys.stdout.write("| %20s | %20s | %20s | %20.1f | %20s | %20s | %20s |\n" % ( str(t.dataset_name[i]), str(ephem.date(t.StartDate[i] + 2400000.5- ephem.julian_date(ephem.date(0))))[:20], t.TargetName[i][:20], t.ExposureTime[i], ra[:20], dec[:20], t.ProposalID[i][:20] ))

fout.write(bar)
sys.stdout.write(bar)
fout.close()
tmpFile.close()
