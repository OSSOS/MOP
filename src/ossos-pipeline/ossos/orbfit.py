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
        self.orbfit.fitradec.restype = ctypes.POINTER(ctypes.c_double * 2)
        self.orbfit.fitradec.argtypes = [ ctypes.c_char_p, ctypes.c_char_p ]

        mpc_file = tempfile.NamedTemporaryFile(suffix='.mpc')
        for observation in self.observations:
            mpc_file.write("{}\n".format(str(observation)))
        mpc_file.seek(0)

        self._abg = tempfile.NamedTemporaryFile()

        result = self.orbfit.fitradec(ctypes.c_char_p(mpc_file.name),
                                      ctypes.c_char_p(self._abg.name))

        self.distance = result.contents[0]
        self.distance_uncertainty = result.contents[1]

        self.orbfit.abg_to_aei.restype = ctypes.POINTER(ctypes.c_double * 12)
        self.orbfit.abg_to_aei.argtypes = [ ctypes.c_char_p ]
        result = self.orbfit.abg_to_aei(ctypes.c_char_p(self._abg.name))
        self.a = result.contents[0]
        self.da = result.contents[6]
        self.e = result.contents[1]
        self.de = result.contents[7]
        self.inc = result.contents[2]
        self.dinc = result.contents[8]
        self.Node = result.contents[3]
        self.dNode = result.contents[9]
        self.om = result.contents[4]
        self.dom = result.contents[10]
        self.T = result.contents[5]
        self.dT = result.contents[11]
        self._residuals()

    def _residuals(self):
        for observation in self.observations:
            self.predict(observation.date)
            dra = coordinates.Angle(self.coordinate.ra - observation.coordinate.ra )
            if dra.degrees > 180 :
                dra = dra - coordinates.Angle(360, unit=units.degree )
            ddec = coordinates.Angle(self.coordinate.dec - observation.coordinate.dec )
            if ddec.degrees > 180:
                dra = ddec - coordinates.Angle(360, unit=units.degree)
            observation.ra_residual = dra.degrees*3600.0
            observation.dec_residual = ddec.degrees*3600.0

    def __str__(self):
        """

        """
        res = "{:>10s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}\n".format(self.observations[0].provisional_name.strip(' '),
                                                            "a (AU)",
                                                            "e",
                                                            "Inc.",
                                                            "Node",
                                                            "peri.")
        res += "{:>10s} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format("fit",
                                                   self.a,
                                                   self.e,
                                                   self.inc,
                                                   self.Node,
                                                   self.om)
        res += "{:>10s} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format("uncert",
                                                               self.da,
                                                               self.de,
                                                               self.dinc,
                                                               self.dNode,
                                                               self.dom)
        return res

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
