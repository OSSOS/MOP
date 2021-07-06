#!/usr/bin/env python

import urllib.request, urllib.parse, urllib.error

def TAPQuery(RAdeg=180.0, DECdeg=0.0, width=1, height=1):
    """Do a query of the CADC Megacam table.  Get all observations insize the box.  Returns a file-like object"""


    QUERY =( """ SELECT """
             """ COORD1(CENTROID(Plane.position_bounds)) AS "RAJ2000", COORD2(CENTROID(Plane.position_bounds)) AS "DEJ2000", Plane.time_bounds_lower as "MJDATE" """
             """ FROM """
             """ caom2.Observation as o JOIN caom2.Plane as Plane on o.obsID=Plane.obsID """
             """ WHERE """
             """ o.collection = 'CFHT' """
             """ AND o.instrument_name = 'MegaPrime' """
             """ AND INTERSECTS( BOX('ICRS', {}, {}, {}, {}), Plane.position_bounds ) = 1 """
             """ AND ( o.proposal_id LIKE '%P05') """)
    # """ AND ( o.proposal_id LIKE '%P05' OR o.proposal_id LIKE '%L03' or o.proposal_id LIKE '%L06' or o.proposal_id
    # in ( '06AF33', '06BF98' ) ) """ )

    QUERY = QUERY.format( RAdeg, DECdeg, width, height)

    data={"QUERY": QUERY,
          "REQUEST": "doQuery",
          "LANG": "ADQL",
          "FORMAT": "votable"}
    
    url="http://www.cadc.hia.nrc.gc.ca/tap/sync"

    print(url, data)

    return urllib.request.urlopen(url,urllib.parse.urlencode(data))


def TAPQuery(RAdeg=180.0, DECdeg=0.0, width=1, height=1):
    """Do a query of the CADC Megacam table.  Get all observations insize the box.  Returns a file-like object"""


    QUERY =( """ SELECT """
             """ COORD1(CENTROID(Plane.position_bounds)) AS "RAJ2000", COORD2(CENTROID(Plane.position_bounds)) AS "DEJ2000", Plane.time_bounds_lower as "MJDATE" """
             """ FROM """
             """ caom2.Observation as o JOIN caom2.Plane as Plane on o.obsID=Plane.obsID """
             """ WHERE """
             """ o.collection = 'CFHT' """
             """ AND o.instrument_name = 'MegaPrime' """
             """ AND INTERSECTS( BOX('ICRS', {}, {}, {}, {}), Plane.position_bounds ) = 1 """
             """ AND ( o.proposal_id LIKE '%P05') """)
    # """ AND ( o.proposal_id LIKE '%P05' OR o.proposal_id LIKE '%L03' or o.proposal_id LIKE '%L06' or o.proposal_id
    # in ( '06AF33', '06BF98' ) ) """ )

    QUERY = QUERY.format( RAdeg, DECdeg, width, height)

    data={"QUERY": QUERY,
          "REQUEST": "doQuery",
          "LANG": "ADQL",
          "FORMAT": "votable"}
    
    url="http://www.cadc.hia.nrc.gc.ca/tap/sync"
    

    print(url, data)

    return urllib.request.urlopen(url,urllib.parse.urlencode(data))



if __name__ == '__main__':
    ra_cen = (37+7)/2.0
    dec_cen = (20+0)/2.0
    width = 20-0
    height = 37-7
    print(TAPQuery(ra_cen, dec_cen, width, height).read())
