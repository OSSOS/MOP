"""
A script to provide uncertainty in future position based on simulated input observations.
"""

input_orbit_mpc_lines = """     O13AE41* C2013 04 04.41785 14 07 33.847-13 03 49.11         23.51r      568
1615907p22 O13AE41 Y  1422.3  758.6 23.51 0.09 UUUU %
     O13AE41  C2013 04 04.45816 14 07 33.613-13 03 48.06         23.54r      568 1615917p22 O13AE41 Y  1438.3  763.7
     23.54 0.12 UUUU %
     O13AE41  C2013 04 04.49859 14 07 33.377-13 03 46.88         23.78r      568 1615927p22 O13AE41 Y  1451.2  769.4
     23.78 0.16 UUUU %
     O13AE41  C2013 04 05.54022 14 07 27.456-13 03 17.66         23.64r      568 1616089p22 O13AE41 Y  1598.8  818.0
     23.64 0.19 UUUU %
     O13AE41  C2013 04 05.54410 14 07 27.433-13 03 17.35         23.29r      568 1616090p22 O13AE41 Y  1599.4  815.0
     23.29 0.18 UUUU %
     O13AE41  C2013 04 05.54905 14 07 27.400-13 03 17.38         23.57r      568 1616091p22 O13AE41 Y  1603.5  817.9
     23.57 0.20 UUUU %
     O13AE41  C2013 04 06.59098 14 07 21.417-13 02 47.87         23.70r      568 1616195p22 O13AE41 Y  1738.5  864.2
     23.70 0.18 UUUU %
     O13AE41  C2013 04 08.39181 14 07 11.020-13 01 56.36         24.16r      568 1616496p22 O13AE41 Y  1939.2  427.1
     24.16 0.20 UUUU %
     O13AE41  C2013 05 11.42210 14 03 53.205-12 45 26.60         23.40r      568 1626152p24 O13AE41 Y  1800.6 2250.6
     23.40 0.07 UUUU %
     O13AE41  C2013 05 12.42371 14 03 47.489-12 44 57.82         23.53r      568 1626288p24 O13AE41 Y  1924.6 1814.5
     23.53 0.07 UUUU %
     O13AE41  C2013 06 13.26162 14 01 18.663-12 32 29.85                     568 1631611p25 O13AE41 Z  1868.3 2838.7
      UUUU % Photometry cannot be performed.  No magnitude calculated.
     O13AE41  C2013 06 13.30208 14 01 18.514-12 32 29.11         23.69r      568 1631621p25 O13AE41 Y  1875.5 2841.1
     23.69 0.12 UUUU %
     O13AE41  C2013 07 04.28289 14 00 31.136-12 28 42.08         23.84r      568 1635766p25 O13AE41 Y  1927.9 3585.0
     23.84 0.12 UUUU %
     O13AE41  C2013 07 06.29052 14 00 29.181-12 28 34.11         24.13r      568 1636124p25 O13AE41 Y  1846.8 3100.2
     24.13 0.12 UUUU %    """

import tempfile

import astropy
import numpy
from astropy import units as u

from ossos import mpc
from ossos import orbfit

obs = []
for line in input_orbit_mpc_lines.split('\n'):
    obs.append(mpc.Observation.from_string(line))

orbit = orbfit.Orbfit(obs)
from   astropy import units
# # now create a set of lines to act as input observations.
## these are HST observations.

ephem_file = tempfile.NamedTemporaryFile()
date = '2013 01 01'
orbit.predict(date, 568)
ra1 = orbit.coordinate.ra.degrees
dec1 = orbit.coordinate.dec.degrees
start_date = mpc.Time('2013 01 01', scale='utc', precision=6)
for trial in range(1000):
    new_obs = []
    for dt in [0, 0.01, 0.02, 2.0, 5.0, 30, 60]:
        date = start_date.jd + dt
        date = mpc.Time(val=int(date), val2=date - int(date), format='jd', scale='utc').copy(format='mpc')
        orbit.predict(date, 568)
        ra = orbit.coordinate.ra.degrees + numpy.random.normal() * 0.05 / 3600.0 + 0.05 / 3600.0
        dec = orbit.coordinate.dec.degrees + numpy.random.normal() * 0.05 / 3600.0 + 0.05 / 3600.0
        position = astropy.coordinates.ICRSCoordinates(ra, dec, unit=(u.degree, u.degree))
        print('{} {} {} {:f} {}'.format(date.jd, position.ra.format(sep=':', unit=units.hour),
                                        position.dec.format(sep=':', unit=units.degree), 0.2 / 3600.0, 568))

    break
