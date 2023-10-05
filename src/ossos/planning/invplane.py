import invariable
from astropy.coordinates import SkyCoord
import numpy


print("RA_J2000,DEC_J2000,LON_HCRS,LAT_HCRS,LON_INV,LAT_INV")
for invlon in numpy.arange(0, numpy.pi*2, numpy.pi*2/100):
    elat, elon = invariable.trevonc(0.0, invlon)
    if elon < 0: 
       elon = 2*numpy.pi + elon
    coord = SkyCoord(elon, elat, distance=40.0, obstime="2000-01-01T00:00:00", unit=('radian', 'radian', 'au'), frame='heliocentrictrueecliptic').transform_to('icrs')
    print(f"{coord.ra.degree:10.4},{coord.dec.degree:10.4},{numpy.degrees(elon):10.4},{numpy.degrees(elat):10.4},{numpy.degrees(invlon):10.4},{0.0:10.4}")
