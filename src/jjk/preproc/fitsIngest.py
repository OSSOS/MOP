#!/usr/cadc/misc/bin/python
"""Read in a fits image and store the keywords in a SQL table."""

import MOPdbaccess
import pyfits

##
## set up a has array to get the desired info from the frame.
## hash has the structure "dbcol" => "fits keyword"
##
## currently the correlation between the image header and the exposure
## table is 1:1

dataDictionary = {
    "expnum": "expnum",
    "chipid":  "extname",
    "runid": "runid",
    "object": "object",
    "ra_deg": "ra_deg",
    "dec_deg": "dec_deg",
    "exptime": "exptime",
    "mjdate": "mjdate",
    "date_obs": "date-obs",
    "utc_obs": "utc-obs",
    "filter": "filter" }

import sys
import os
import wcsutil

basename = os.path.basename

db=MOPdbaccess.connect('cfeps','cfhls','MYSQL')
cfeps=db.cursor()

for file in sys.argv[1:] :
    file = file.strip()
    try:
        fits = pyfits.open(file)
    except:
        sys.stderr.write("\nERROR: failed to open %s\n" % ( file))
        continue
    values={}
    values['file_id']=basename(file).split('.')[0]
    values['active']=file
    values['archive']='not available'
    sys.stdout.write("Ingesting %30s \r" % (file))

    for key in dataDictionary:
        values[key]=fits[0].header.get(key,None)

    wcs = wcsutil.WCSObject(fits[0].header)

    print wcs
    print values




