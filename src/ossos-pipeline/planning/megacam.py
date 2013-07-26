#!/usr/bin/env python


import urllib

def TAPQuery(RAdeg=180.0, DECdeg=0.0, width=1, height=1):
    """Do a query of the CADC Megacam table.  Get all observations insize the box.  Returns a file-like object"""

    radius = max(width, height)/2.0

    QUERY =( """ SELECT """
             """ COORD1(CENTROID(Plane.position_bounds)) AS "RAJ2000", COORD2(CENTROID(Plane.position_bounds)) AS "DEJ2000" """
             """ FROM """
             """ caom.Observation as o JOIN caom.Plane as Plane on o.obsID=Plane.obsID """
             """ WHERE """
             """ o.collection = 'CFHT' """
             """ AND o.instrument_name = 'MegaPrime' """
             """ AND INTERSECTS( CIRCLE('ICRS', %f, %f, %f), Plane.position_bounds ) = 1 """ )

    QUERY = QUERY % ( RAdeg, DECdeg, radius)

    data={"QUERY": QUERY,
          "REQUEST": "doQuery",
          "LANG": "ADQL",
          "FORMAT": "votable"}
    
    url="http://www.cadc.hia.nrc.gc.ca/tap/sync"
    

    print url, data

    return urllib.urlopen(url,urllib.urlencode(data))



if __name__ == '__main__':
    ra_cen = (37+7)/2.0
    dec_cen = (20+0)/2.0
    width = 20-0
    height = 37-7
    print TAPQuery(ra_cen, dec_cen, width, height).read()
