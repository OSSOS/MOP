__author__ = 'jjk'

import ctypes
import tempfile

from astropy import coordinates
from astropy import units

from mpc import Observation
from ossos.mpc import Time


LIBORBFIT = "/usr/local/lib/liborbfit.so"


class OrbfitError(Exception):
    def __init__(self):
        super(OrbfitError, self).__init__(
            "Insufficient observations for an orbit.")


class Orbfit(object):
    """
    This class provides orbital information derived by calling 'fit_radec'.
    """

    def __init__(self, observations=None):
        """
        Given a set of observations compute the orbit using fit_radec and provide methods for
        accessing that orbit.

        Requires at least 3 observations.
        """
        assert isinstance(observations, tuple) or isinstance(observations, list)
        if not observations:
            observations = []
        if len(observations) < 3:
            raise OrbfitError()
        self.orbfit = ctypes.CDLL(LIBORBFIT)
        assert isinstance(observations[0], Observation)
        self.observations = observations
        self.arc_length = observations[-1].date.jd - observations[0].date.jd
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

        # call fit_radec with mpcfile, abgfile, resfile
        self.orbfit.fitradec.restype = ctypes.POINTER(ctypes.c_double * 2)
        self.orbfit.fitradec.argtypes = [ctypes.c_char_p, ctypes.c_char_p]

        mpc_file = tempfile.NamedTemporaryFile(suffix='.mpc')
        for observation in self.observations:
            if not observation.null_observation:
                obs = observation
                ra = obs.ra.replace(" ", ":")
                dec = obs.dec.replace(" ", ":")
                res = 0.3
                print "FIT: "+str(observation)
                mpc_file.write("{} {} {} {} {}\n".format(obs.date.jd, ra, dec, res, 568, ))
        mpc_file.seek(0)

        self._abg = tempfile.NamedTemporaryFile()

        result = self.orbfit.fitradec(ctypes.c_char_p(mpc_file.name),
                                      ctypes.c_char_p(self._abg.name))

        self.distance = result.contents[0]
        self.distance_uncertainty = result.contents[1]

        self.orbfit.abg_to_aei.restype = ctypes.POINTER(ctypes.c_double * 12)
        self.orbfit.abg_to_aei.argtypes = [ctypes.c_char_p]
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
        residuals = ""
        for observation in self.observations:
            self.predict(observation.date)
            dra = coordinates.Angle(self.coordinate.ra - observation.coordinate.ra)
            if dra.degrees > 180:
                dra = dra - coordinates.Angle(360, unit=units.degree)
            ddec = coordinates.Angle(self.coordinate.dec - observation.coordinate.dec)
            if ddec.degrees > 180:
                dra = ddec - coordinates.Angle(360, unit=units.degree)
            observation.ra_residual = dra.degrees * 3600.0
            observation.dec_residual = ddec.degrees * 3600.0
            if observation.null_observation:
                residuals += "!"
            else:
                residuals += " "
            residuals += "{:12s} {:+05.2f} {:+05.2f}\n".format(observation.date, observation.ra_residual,
                                                               observation.dec_residual)
        self.residuals = residuals

    def __str__(self):
        """

        """
        res = "{:>10s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}\n".format(
            self.observations[0].provisional_name.strip(' '),
            "r (AU)",
            "a (AU)",
            "e",
            "Inc.",
            "Node",
            "peri.")
        res += "{:>10s} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format("fit",
                                                                                  self.distance,
                                                                                  self.a,
                                                                                  self.e,
                                                                                  self.inc,
                                                                                  self.Node,
                                                                                  self.om)
        res += "{:>10s} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f}\n".format("uncert",
                                                                                  self.distance_uncertainty,
                                                                                  self.da,
                                                                                  self.de,
                                                                                  self.dinc,
                                                                                  self.dNode,
                                                                                  self.dom)
        res += "{:>10s} {:8.2f} days\n".format("arc", self.arc_length)

        return res

    def predict(self, date, obs_code=568):
        """
        use the bk predict method to compute the location of the source on the given date.
        """
        if not isinstance(date, Time):
            if isinstance(date, float):
                try:
                    date = Time(date, format='jd', scale='utc', precision=6)
                except:
                    date = None  # FIXME: this might blow up, not sure
            else:
                try:
                    date = Time(date, format='jd', scale='utc', precision=6)
                except ValueError:
                    try:
                        date = Time(date, format='mpc', scale='utc', precision=6)
                    except ValueError:
                        date = Time(date, scale='utc', precision=6)  # see if it can guess

        jd = ctypes.c_double(date.jd)
        # call predict with agbfile, jdate, obscode
        self.orbfit.predict.restype = ctypes.POINTER(ctypes.c_double * 5)
        self.orbfit.predict.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_int]
        predict = self.orbfit.predict(ctypes.c_char_p(self._abg.name),
                                      jd,
                                      ctypes.c_int(obs_code))
        self.coordinate = coordinates.ICRSCoordinates(predict.contents[0],
                                                      predict.contents[1],
                                                      unit=(units.degree, units.degree))
        self.dra = predict.contents[2]
        self.ddec = predict.contents[3]
        self.pa = predict.contents[4]
        self.date = str(date)
