import os
import math
import ephem
from ctypes.util import find_library
import ctypes
import tempfile
from ossos.mpc import Observation
from ossos.mpc import Time

LIBORBFIT='/usr/local/lib/liborbfit.so'

__author__ = 'jjk'

class Orbfit(object):

    def __init__(self, observations=[], abg=[], residuals=[]):
        self.orbfit = ctypes.CDLL(LIBORBFIT)
        self.abg = abg
        self.residuals = residuals
        self.observations = observations
        self._fit_radec()

    def _fit_radec(self):
        """
        call fit_radec of BK passing in the observations.

        """

        # call fitradec with mpcfile, abgfile, resfile
        self.orbfit.fitradec.restype = ctypes.c_int
        self.orbfit.fitradec.argtypes = [ ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p ]

        mpc_file = tempfile.NamedTemporaryFile(suffix='.mpc')
        for observation in self.observations:
            mpc_file.write(observation)
        mpc_file.seek(0)

        abg_file = tempfile.NamedTemporaryFile()
        res_file = tempfile.NamedTemporaryFile()

        self.orbfit.fitradec(ctypes.c_char_p(mpc_file.name),
                             ctypes.c_char_p(abg_file.name),
                             ctypes.c_char_p(res_file.name))

        self.abg = abg_file
        self.abg.seek(0)
        self.residuals = res_file
        self.residuals.seek(0)

    def predict(self, date, obs_code=568):
        """
        use the bk predict method to compute the location of the source on the given date.
        """
        time = Time(date, scale='utc')
        # call predict with agbfile, jdate, obscode
        self.orbfit.predict.restype = ctypes.POINTER(ctypes.c_double * 5)
        self.orbfit.predict.argtypes = [ ctypes.c_char_p, ctypes.c_float, ctypes.c_int ]
        predict = self.orbfit.predict(ctypes.c_char_p(self.abg.name),
                       ctypes.c_float(time.jd),
                       ctypes.c_int(obs_code))

        self.ra = predict.contents[0]
        self.dec = predict.contents[1]

        self.dra = predict.contents[2]
        self.ddec = predict.contents[3]
        self.pa = predict.contents[4]




mpc_lines="""     HL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568
     HL7j2    C2013 04 04.58296 17 11 59.80 +04 14 05.5          24.0 R      568
     HL7j2    C2013 05 03.52252 17 10 38.28 +04 28 00.9          23.4 R      568
     HL7j2    C2013 05 08.56725 17 10 17.39 +04 29 47.8          23.4 R      568"""

