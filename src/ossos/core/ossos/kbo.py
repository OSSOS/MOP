import ctypes
import os
import tempfile
from astropy import coordinates
from astropy import units
from ossos.mpc import Time, Observation

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
            mpc_file.write("{}\n".format(str(observation)))
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
        time = Time(date, scale='utc', precision=6)
        jd = ctypes.c_double(time.jd)
        # call predict with agbfile, jdate, obscode
        self.orbfit.predict.restype = ctypes.POINTER(ctypes.c_double * 5)
        self.orbfit.predict.argtypes = [ ctypes.c_char_p, ctypes.c_double, ctypes.c_int ]
        predict = self.orbfit.predict(ctypes.c_char_p(self.abg.name),
                       jd,
                       ctypes.c_int(obs_code))
        self.coordinate = coordinates.SkyCoord(predict.contents[0],
                                               predict.contents[1],
                                               unit=(units.degree, units.degree))
        self.dra = predict.contents[2]
        self.ddec = predict.contents[3]
        self.pa = predict.contents[4]
        self.date = str(time)


if __name__ == '__main__':

    mpc_lines=("     HL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568",
               "     HL7j2    C2013 04 04.58296 17 11 59.80 +04 14 05.5          24.0 R      568",
               "     HL7j2    C2013 05 03.52252 17 10 38.28 +04 28 00.9          23.4 R      568",
               "     HL7j2    C2013 05 08.56725 17 10 17.39 +04 29 47.8          23.4 R      568")

    observations = []
    for line in mpc_lines:
        observations.append(Observation().from_string(line))

    os.environ['ORBIT_EPHEMERIS']='/Users/jjk/MOP/config/binEphem.405'
    os.environ['ORBIT_OBSERVATORIES']='/Users/jjk/MOP/config/observatories.dat'
    HL7j2 = Orbfit(observations=observations)
    for observation in observations:
        HL7j2.predict(observation.date)
        dra = coordinates.Angle(HL7j2.coordinate.ra - observation.coordinate.ra)
        if dra.degrees > 180 :
            dra = dra - coordinates.Angle(360, unit=units.degree)
        ddec = coordinates.Angle(HL7j2.coordinate.dec - observation.coordinate.dec)
        if ddec.degrees > 180:
            dra = ddec - coordinates.Angle(360, unit=units.degree)

        print("Input  : {} {} {} [ {:+4.2f} {:+4.2f} ]".format(observation.date,
                                               observation.ra,
                                               observation.dec,
                                               dra.degrees*3600.0,
                                               ddec.degrees*3600.0))
        #print "Predict: {} {} {}".format(HL7j2.date,
        #                                 HL7j2.coordinate.ra.format(unit=units.hour,
        #                                                           precision=3,
        #                                                            pad=True,
        #                                                            sep=" "),
        #                                 HL7j2.coordinate.dec.format(unit=units.degree,
        #                                                            decimal=False,
        #                                                            precision=2,
        #                                                            pad=True,
        #                                                            sep=" ",
        #                                                            alwayssign=True))
