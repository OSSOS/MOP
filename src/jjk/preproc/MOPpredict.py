#!/usr/cadc/misc/bin/python

import orbfit
import sys
import ephem
import wcsutil
import pyfits
import os, math

name = sys.argv[1]

obs = 568
fitsImages=sys.argv[2:]

for fitsImage in fitsImages:
    f = pyfits.open(fitsImage)
    sdate=f[0].header.get('DATE-OBS',None)
    if sdate is None:
       mjdate= f[0].header['MEANMJD']
    else:
       sdate=sdate.replace("-","/")
       mjdate=f[0].header['MJD-OBS']
    #   date=ephem.julian_date(sdate)+dfrac
    date = mjdate + 2400000.5
    dfrac = mjdate-math.floor(mjdate)
    
    (ra,dec,dra,ddec,dang)=orbfit.predict("/home/cadc/kavelaar/dbase/TNOdb/dbase/data/orbb/"+name+".abg",
                                          date,obs)

    window = max(dra/0.18+100,ddec/0.18+100,1000.)

    wcsObject = wcsutil.WCSObject(f[0].header)
    (x,y)=wcsObject.rd2xy((ra,dec))
    print fitsImage,ra,dec, x,y,dra, ddec, dang    
    x1=math.floor(max(x-window,1))
    x2=math.floor(min(x+window,f[0].header['NAXIS1']))
    y1=math.floor(max(y-window,1))
    y2=math.floor(min(y+window,f[0].header['NAXIS2']))
    if not (x1< f[0].header['NAXIS1'] and x2 > 0 and y1 < f[0].header['NAXIS2'] and y2 >0 ):
        continue

    cmd='imcopy %s[%d:%d,%d:%d] - | xpaset ds9 fits new ' % ( fitsImage,x1,x2,y1,y2)
    print cmd
    os.system(cmd)
    cmd = 'echo "icrs; box %f %f %f %f %f" | xpaset ds9 regions' % ( ra, dec, 2.0*dra/3600.0, 2.0*ddec/3600.0, dang-90)
    print cmd
    os.system(cmd)

