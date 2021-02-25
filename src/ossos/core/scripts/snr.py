#!python
################################################################################
##                                                                            ##
## Copyright 2013 by its authors                                              ##
## See COPYING, AUTHORS                                                       ##
##                                                                            ##
## This file is part of OSSOS Moving Object Pipeline (OSSOS-MOP)              ##
##                                                                            ##
##    OSSOS-MOP is free software: you can redistribute it and/or modify       ##
##    it under the terms of the GNU General Public License as published by    ##
##    the Free Software Foundation, either version 3 of the License, or       ##
##    (at your option) any later version.                                     ##
##                                                                            ##
##    OSSOS-MOP is distributed in the hope that it will be useful,            ##
##    but WITHOUT ANY WARRANTY; without even the implied warranty of          ##
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ##
##    GNU General Public License for more details.                            ##
##                                                                            ##
##    You should have received a copy of the GNU General Public License       ##
##    along with OSSOS-MOP.  If not, see <http://www.gnu.org/licenses/>.      ##
##                                                                            ##
################################################################################


from ossos import storage
from astropy.io import ascii
from io import StringIO
import math
import sys

expnum = sys.argv[1]
ccd = sys.argv[2]

fwhm = storage.get_fwhm(expnum, ccd)

class Apcor(object):
    def __init__(self, expnum, ccd):
        self.expnum = expnum
        self.ccd = ccd
        self.apcor_array = storage.open_vos_or_local(self.uri).read().split()


    @property
    def ap_in(self):
        return float(self.apcor_array[0])

    @property
    def ap_cor(self):
        return float(self.apcor_array[2])

    @property
    def uri(self):
        return storage.get_uri(self.expnum, self.ccd, ext='apcor')

def get_sky(expnum, ccd):
    uri = storage.get_uri(expnum, ccd, ext='phot')

    fobj = StringIO(storage.open_vos_or_local(uri).read())
    fobj.seek(0)
    phot_table = ascii.read(fobj)
    return phot_table['MSKY'].mean()
        


apcor = Apcor(expnum, ccd)
fwhm = storage.get_fwhm(expnum, ccd)
zp = storage.get_zeropoint(expnum, ccd)
sky = get_sky(expnum, ccd)



flux_24 = 10**(-((24.0 + apcor.ap_cor) - zp)/2.5)
flux_sky = math.pi * apcor.ap_in**2 * sky

snr = flux_24 / math.sqrt(flux_24 + flux_sky)
storage.set_tag(expnum, "snr_{:02d}".format(int(ccd)), "{:5.2f}".format(snr))

print(expnum, ccd, sky, zp, fwhm, flux_sky, flux_24, snr)

