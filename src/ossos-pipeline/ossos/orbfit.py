__author__ = 'jjk'
import ctypes
LIBORBFIT = "/usr/local/lib/liborbfit.so"
import tempfile
from mpc import Time
from astropy import coordinates
from astropy import units

class Orbfit(object):
    """
    This class provides orbital information derived by calling 'fit_radec'.
    """

    def __init__(self, observations=[]):
        """
        Given a set of observations compute the orbit using fit_radec and provide methods for
        accessing that orbit.

        Requires at least 3 observations.
        """
        if len(observations) < 3:
            raise Exception("Insufficent enough observations for an orbit.")
        self.orbfit = ctypes.CDLL(LIBORBFIT)
        self.observations = observations
        self._fit_radec()

    @property
    def abg(self):
        """
        A print out the abg file.

        abg is stored in a temporary file and is deleted on code exit.  This is the content of that file.
        """
        self._abg.seek(0)
        return self._abg.readlines()

    def _fit_radec(self):
        """
        call fit_radec of BK passing in the observations.

        """

        # call fitradec with mpcfile, abgfile, resfile
        self.orbfit.fitradec.restype = ctypes.c_int
        self.orbfit.fitradec.argtypes = [ ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p ]

        mpc_file = tempfile.NamedTemporaryFile(suffix='.mpc')
        for observation in self.observations:
            mpc_file.write("{}\n".format(str(observation)))
        mpc_file.seek(0)

        abg_file = tempfile.NamedTemporaryFile()
        res_file = tempfile.NamedTemporaryFile()

        self.orbfit.fitradec(ctypes.c_char_p(mpc_file.name),
                             ctypes.c_char_p(abg_file.name),
                             ctypes.c_char_p(res_file.name))

        self._abg = abg_file

    def predict(self, date, obs_code=568):
        """
        use the bk predict method to compute the location of the source on the given date.
        """
        time = Time(date, scale='utc', precision=6)
        jd = ctypes.c_double(time.jd)
        # call predict with agbfile, jdate, obscode
        self.orbfit.predict.restype = ctypes.POINTER(ctypes.c_double * 5)
        self.orbfit.predict.argtypes = [ ctypes.c_char_p, ctypes.c_double, ctypes.c_int ]
        predict = self.orbfit.predict(ctypes.c_char_p(self._abg.name),
                                      jd,
                                      ctypes.c_int(obs_code))
        self.coordinate = coordinates.ICRSCoordinates(predict.contents[0],
                                                      predict.contents[1],
                                                      unit=(units.degree, units.degree))
        self.dra = predict.contents[2]
        self.ddec = predict.contents[3]
        self.pa = predict.contents[4]
        self.date = str(time)
