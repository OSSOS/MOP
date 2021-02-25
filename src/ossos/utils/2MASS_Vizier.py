#!python
from astropy.io import votable
import requests
from io import StringIO

def query(ra ,dec, rad=0.1, query=None):
    """Query the CADC TAP service to determine the list of images for the
NewHorizons Search.  Things to determine:
   

   a- Images to have the reference subtracted from.
   b- Image to use as the 'REFERENCE' image.
   c- Images to be used for input into the reference image

Logic: Given a particular Image/CCD find all the CCDs of the same field that
overlap that CCD but are taken more than 7 days later or earlier than
that image.

    """
    if query is None:
       query=( """ SELECT """
               """ "II/246/out".raj2000 as ra, "II/246/out".dej2000 as dec, "II/246/out".jmag as jmag """
               """ FROM "II/246/out" """
               """ WHERE """
               """ CONTAINS(POINT('ICRS', raj2000, dej2000), CIRCLE('ICRS', {}, {}, {})) = 1 """.format(ra,dec,rad) )

    tapURL = "http://TAPVizieR.u-strasbg.fr/TAPVizieR/tap/sync"


    ## Some default parameters for that TAP service queries.
    tapParams={'REQUEST': 'doQuery',
               'LANG':    'ADQL',
               'FORMAT':  'votable',
               'QUERY':   query}

    response = requests.get(tapURL, params=tapParams)
    data = StringIO(response.text)
    data.seek(0)
    data.seek(0)
    T = votable.parse_single_table(data).to_table()

    return T


if __name__=='__main__':
    from astropy import coordinates
    from astropy.time.core import Time
    import numpy as np
    import time
    import sys
    stack = None
    Ts = {}
    T = query(ra=sys.argv[1], dec=sys.argv[2], rad=sys.argv[3])
    T.sort('jmag')
    f = open('usno.ccmap', 'w')
    count = 0 
    for row in T:
        count += 1 
        f.write("{} {} {} {}\n".format(row['ra'], row['dec'], row['jmag'], count))
    f.close()



