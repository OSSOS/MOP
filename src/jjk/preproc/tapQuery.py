#!/usr/bin/env python


import urllib, ephem, datetime, atpy

mjd_yesterday = ephem.date(ephem.julian_date(ephem.date(str(datetime.date.today())))) - 2400000.5 - 10.0

## instead of having specific collumns in the SELECT clause below you could just put a '*' to get all possible columns.
## 
## the 'position_bounds' column provides a polygon that describes the edge of the field of the mosaic, not individual ccds.

data={"QUERY": """SELECT Observation.target_name as TargetName, COORD1(CENTROID(Plane.position_bounds)) AS RA, COORD2(CENTROID(Plane.position_bounds)) AS DEC, Plane.time_bounds_cval1 AS StartDate, Plane.time_exposure AS ExposureTime, Observation.instrument_name AS Instrument, Plane.energy_bandpassName AS Filter, Observation.collectionID AS dataset_name, Observation.proposal_id AS ProposalID, Observation.proposal_pi AS PI FROM caom.Observation AS Observation JOIN caom.Plane AS Plane ON Observation.obsID = Plane.obsID WHERE  ( Observation.collection = 'CFHT' ) AND Plane.time_bounds_cval1 > %d AND Plane.observable_ctype='RAW' AND Observation.proposal_id IN ('13AP05', '13AP06' ) """ %  ( mjd_yesterday, ),
      "REQUEST": "doQuery",
      "LANG": "ADQL",
      "FORMAT": "votable"}

url="http://www.cadc.hia.nrc.gc.ca/tap/sync?"+urllib.urlencode(data)

# uncomment this to see the URL
# print data["QUERY"]
# print url

urllib.urlretrieve(url,"temp.vot")


t = atpy.Table()
t.read("temp.vot",type='vo')

t.sort('StartDate')

print "# " 

for i in range(len(t)):
    print "%20s | %20s | %20s | %20s | %20s | %20s" % ( str(t.dataset_name[i]), str(ephem.date(t.StartDate[i] + 2400000.5- ephem.julian_date(ephem.date(0))))[:20], t.TargetName[i][:20],t.Instrument[i][:20], t.ProposalID[i][:20], t.PI[i][:20])
