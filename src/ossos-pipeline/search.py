from ossos import mpc, storage, orbfit
from ossos import wcs
import sys
import numpy

o = orbfit.Orbfit(mpc.MPCReader(sys.argv[1]).mpc_observations)


o.predict('2014 07 20.0')

fields = storage.cone_search(o.coordinate.ra.degrees, o.coordinate.dec.degrees, dra=1.0, ddec=1.0)

mjdates = numpy.unique(fields['mjdate'])

collectionIDs = []

for mjdate in mjdates:
   jd = 2400000.5 + mjdate
   o.predict(jd)
   for field in  storage.cone_search(o.coordinate.ra.degrees, o.coordinate.dec.degrees, dra=3.6/3600.0, ddec=3.6/3600.0, mjdate=mjdate):
       collectionIDs.append(field['collectionID'])

expnums = numpy.unique(numpy.array(collectionIDs))

for expnum in expnums:
  header = storage.get_astheader(expnum, 22) 
  o.predict(header['MJDATE']+2400000.5)
  for ccd in range(36):
     header = storage.get_astheader(expnum, ccd) 
     w = wcs.WCS(header)
     (x, y) = w.sky2xy(o.coordinate.ra.degrees, o.coordinate.dec.degrees)
     if 0 < x < header['NAXIS1']  and 0 < y < header['NAXIS2']:
         print expnum, ccd, x, y, header['MJDATE'], header['FILTER'], header['EXPTIME'], o.coordinate.ra.degrees, o.coordinate.dec.degrees, header['OBJECT'], 'CFHT/MegaCam', "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/CFHT/{}p[{}]".format(expnum, ccd)
         break
