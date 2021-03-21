#!/usr/bin/env python
# coding: utf-8

from astroquery import cadc
import pyds9
import requests
from astropy.io import fits
from io import BytesIO
import numpy as np

client = cadc.Cadc()
query = client.query("""select publisherID, target_name, time_bounds_lower from caom2.Observation o JOIN caom2.Plane p on o.obsID=p.obsID where proposal_id = '15648' AND calibrationLevel = 2""")

target_names = np.unique(query['target_name'])

ds9 = pyds9.DS9('SSOLS')

for target_name in target_names:
    ids = query[query['target_name']==target_name]
    ids = ids[np.argsort(ids['time_bounds_lower'])]
    urls = client.get_data_urls(ids)
    print("Downloading and display {} images for target {}".format(len(urls), target_name))
    ds9.set('frame delete all')
    for url in urls:
        response = requests.get(url)
        stream = BytesIO(response.content)
        hdulist = fits.open(stream)
        ds9.set('frame new')
        ds9.set_fits(hdulist)
    # something to pause downloading.  in DS9 just press a key to move forward.
    junk=ds9.get('iexam key data')




