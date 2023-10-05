from astropy.time import Time
from astropy.table import Table
from astropy.coordinates import SkyCoord
import numpy as np

class Ephemeris(object):

    def __init__(self, provisional_name, filename):
        self.provisional_name = provisional_name
        self.ephem_file = filename
        self._table = None
        self._date = None

    def _load(self):
        print(self.ephem_file)
        self._table = Table.read(self.ephem_file,
                                 format='ascii.no_header',
                                 names=['UTC', 'JD', "RA", "DEC", "DRA", "DDEC", "COV"])

    @property
    def table(self):
        if self._table is None:
            self._load()
        return self._table

    @property
    def date(self):
        if self._date is None:
            self.date = self.table['UTC'][0]
        return self._date

    @date.setter
    def date(self, date):
        try:
            self._date = Time(date)
        except Exception as ex:
            print(ex)
            self._date = Time(date, format='jd')

    def predict(self, date):
        self.date = date

    @property
    def r_mag(self):
        return 26.0

    @property
    def coordinate(self):
        ra = np.interp(self.date.jd, self.table['JD'], self.table['RA'])
        dec = np.interp(self.date.jd, self.table['JD'], self.table['DEC'])
        return SkyCoord(ra, dec, unit='degree')

    @property
    def dra(self):
        return np.interp(self.date.jd, self.table['JD'], self.table['DRA'])/3600.0

    @property
    def ddec(self):
        return np.interp(self.date.jd, self.table['JD'], self.table['DDEC'])/3600.0
