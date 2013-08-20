#!/usr/bin/env python



import urllib
from astropy.io import votable

def TAPQuery(RAdeg, DECdeg, width, height):
    """Given the RA/DEC query the USNO2B catalog for stars insize box at that location.

    returns a file-like object

    """


    QUERY=( """ SELECT """
            """  RAJ2000, """
            """ DEJ2000, """
            """ Bmag, """
            """ Vmag, """
            """ rmag """
            """ FROM "I/322/out" """
            """ WHERE 1=CONTAINS(POINT('ICRS',RAJ2000, DEJ2000), BOX('ICRS', %f, %f, %f, %f)) AND Bmag < 19 """ % ( RAdeg, DECdeg, width, height))
    
    
    data={"query": QUERY,
          "request": "doQuery",
          "lang": "ADQL",
          "format": "votable"}
    
    
    url="http://tapvizier.u-strasbg.fr/TAPVizieR/tap/sync"
    
    print url, data
    return urllib.urlopen(url, urllib.urlencode(data))




if __name__ == '__main__':
    ra_cen = (37+7)/2.0
    dec_cen = (20+0)/2.0
    width = 20-0
    height = 37-7
    print votable.parse(TAPQuery(ra_cen, dec_cen, width, height).read()).get_first_table()
    
