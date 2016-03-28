"""
send queries to the Horizons batch query system via a web UI.
"""
import copy
import re
import requests
import scipy
from astropy.coordinates import SkyCoord
from astropy.time import Time
from astropy import units
from astropy.units.quantity import Quantity
from astropy.io.ascii import Csv
from gui import logger


class Query(object):
    """
    A query object is used to build the query structure that will be sent to Horizons web system.
    """

    SERVER = 'ssd.jpl.nasa.gov'
    PROTOCOL = 'http'
    END_POINT = 'horizons_batch.cgi'

    horizons_quantities = [0, 'Astrometric RA & DEC', 'Apparent RA & DEC',
                           'Rates; RA & DEC', 'Apparent AZ & EL', 'Rates; AZ & EL',
                           'Sat. X & Y, pos. ang', 'Local app. sid. time',
                           'Airmass', 'Vis mag. & Surf Brt', 'Illuminated fraction',
                           'Defect of illumin.', 'Sat. angle separ/vis',
                           'Target angular diam.', 'Obs sub-lng & sub-lat',
                           'Sun sub-long & sub-lat', 'Sub Sun Pos. Ang & Dis',
                           'N. Pole Pos. Ang & Dis', 'Helio eclip. lon & lat',
                           'Helio range & rng rate', 'Obsrv range & rng rate',
                           'One-Way Light-Time', 'Speed wrt Sun & obsrvr',
                           'Sun-Obs-Targ ELONG ang', 'Sun-Targ-Obs PHASE ang',
                           'Targ-Obsrv-Moon/Illum%', 'Obs-Primary-Targ angl',
                           'Pos. Ang;radius & -vel', 'Orbit plane angle',
                           'Constellation ID', 'Delta-T (CT - UT)', 'Obs eclip. lon & lat',
                           'North pole RA & DEC', 'Galactic latitude', 'Local app. SOLAR time',
                           'Earth->Site lt-time', 'RA & DEC uncertainty', 'POS error ellipse',
                           'POS uncertainty (RSS)', 'Range & Rng-rate sig.', 'Doppler/delay sigmas',
                           'True anomaly angle', 'Local app. hour angle']

    default_quantities = ['Astrometric RA & DEC', 'Rates; RA & DEC',
                          'RA & DEC uncertainty', 'Vis mag. & Surf Brt',
                          'POS error ellipse']

    default_horizons_params = {'batch': "'{}'".format(1),
                               'COMMAND': "'{}'".format('Ceres'),
                               'MAKE_EPHEM': "'YES'",
                               'TABLE_TYPE': "'OBSERVER'",
                               'CAL_FORMAT': "'BOTH'",
                               'TIME_DIGITS': "'SECONDS'",
                               'ANG_FORMAT': "'DEG'",
                               'CENTER': "'568@399'",
                               'START_TIME': "'JD {}'".format(Time.now()),
                               'STOP_TIME': "'JD {}'".format(Time.now() + 10*units.day),
                               'STEP_SIZE': "'{} {}'".format(1, 'd'),
                               'QUANTITIES': None,
                               'REF_SYSTEM': "'J2000'",
                               'SKIP_DAYLT': "'NO'",
                               'EXTRA_PREC': "'NO'",
                               'R_T_S_ONLY': "'NO'",
                               'CSV_FORMAT': "'YES'"}

    def __init__(self, target):
        """

        @return:
        """
        self._target = target
        self._params = copy.copy(Query.default_horizons_params)
        self._quantities = None
        self.quantities = Query.default_quantities
        self._data = None
        self._ephemeris = None
        self._elements = None
        self._start_time = Time.now()
        self._stop_time = Time.now() + 1*units.day
        self._step_size = 1*units.day
        self._center = 568
        self._nobs = None
        self._arc_length = None
        self._current_time = None

    @property
    def target(self):
        return self._target

    @target.setter
    def target(self, target):
        if self._target != target:
            self._target = target
            self.reset()

    def reset(self):
        self._data = None
        self._ephemeris = None
        self._elements = None

    @property
    def center(self):
        return "'{}@399'".format(self._center)

    @property
    def command(self):
        return "'{}'".format(self.target)

    @property
    def params(self):
        """
        Build the params set for the Horizons query.  There is a default set that is overridden by matching attributes.
        @return: dict
        """
        for key in self._params:
            attr = key.lower()
            if hasattr(self, attr):
                self._params[key] = getattr(self, attr, self._params[key])
        return self._params

    @params.setter
    def params(self, **kwargs):
        """
        Set the parameters to be set on the Horizons query.

        See Query.default_horizons_params for a list of possible parameters.
        @return:
        """
        for (key, value) in enumerate(kwargs):
            self._params[key] = value

        # this is a required setting, or we have trouble with parsing.
        self._params['CSV_FORMAT'] = "'YES'"

    def parse(self):
        return self._data

    @property
    def quantities(self):
        s = "{}".format(self._quantities)
        return "'{}'".format(s[1:-1])

    @quantities.setter
    def quantities(self, quantities=None):
        if quantities is None:
            quantities = []
        quantities.extend(Query.default_quantities)

        self._quantities = []
        for quantity in quantities:
            try:
                idx = int(quantity)
            except:
                idx = Query.horizons_quantities.index(quantity)
            if idx is not None and idx not in self._quantities:
                self._quantities.append(idx)

    @property
    def start_time(self):
        return "'JD {}'".format(self._start_time.jd)

    @start_time.setter
    def start_time(self, start_time):
        assert isinstance(start_time, Time)
        if start_time != self._start_time:
            self.reset()
            self._start_time = start_time

    @property
    def stop_time(self):
        return "'JD {}'".format(self._stop_time.jd)

    @stop_time.setter
    def stop_time(self, stop_time):
        assert isinstance(stop_time, Time)
        if self._stop_time != stop_time:
            self._stop_time = stop_time
            self.reset()

    @property
    def step_size(self):
        """

        @return: Quantity size of time step
        """
        s = "{:1.0f}".format(self._step_size)[:3]
        return "'{}'".format(s)

    @step_size.setter
    def step_size(self, step_size):
        assert isinstance(step_size, Quantity)
        if self._step_size != step_size:
            self._step_size = step_size
            self.reset()

    @property
    def ephemeris(self):
        if self._ephemeris is None:
            self._parse_ephemeris()
        return self._ephemeris

    @property
    def data(self):
        if self._data is None:
            self._get()
        return self._data

    @property
    def elements(self):
        if self._elements is None:
            self._parse_elements()
        return self._elements

    def _get(self):
        """
        Connect to the service and make the query.
        @return:
        """
        url = '{}://{}/{}'.format(Query.PROTOCOL,
                                  Query.SERVER,
                                  Query.END_POINT)
        logger.info("Sending JPL/Hoirzons query.\n")
        self._response = requests.get(url, params=self.params)
        self._response.raise_for_status()
        self._data = []
        for line in self._response.iter_lines():
            self._data.append(line)

    def _parse_ephemeris(self):
        """Parse the ephemeris out of the responses from Horizons and place in self._ephemeris."""

        start_of_ephemeris = '$$SOE'
        end_of_ephemeris = '$$EOE'
        start_idx = None
        end_idx = None
        for idx, line in enumerate(self.data):
            if line.startswith(start_of_ephemeris):
                start_idx = idx
            if line.startswith(end_of_ephemeris):
                end_idx = idx

        if start_idx is None or end_idx is None:
            raise ValueError(self._response.content, "Not a valid response.")

        # the header of the CSV structure is 2 lines before the start_of_ephmeris
        csv_lines = [self.data[start_idx - 2]]
        csv_lines.extend(self.data[start_idx + 1: end_idx])
        csv = Csv()
        table = csv.read(csv_lines)
        table['Time'] = Time(table['Date_________JDUT'], format='jd')
        self._ephemeris = table

    def _parse_elements(self):
        """Parse the elements out of the response from Horizons and place in elements object."""

        # the elements are in a '****' record at the start of file
        elements_record_started = False
        self._elements = {}
        for line in self.data:
            if line.startswith('****'):
                if elements_record_started:
                    break
                elements_record_started = True
                continue
            for part in re.findall('((\S+=)\s+(\S+))', line):
                key = part[1].strip().strip('=')
                try:
                    value = float(part[2].strip())
                except:
                    value = part[2].strip()
                self._elements[key] = value

    def _parse_obs_arc(self):
        parts = re.search('# obs: (\d+) \((\d+)-(\d+)\)', self.data)
        if parts is None:
            # Just fake some data
            self._arc_length = 30 * units.day
            self._nobs = 3
        else:
            self._arc_length = (int(parts.group(3)) - int(parts.group(2))) * units.year
            self._nobs = int(parts.group(1))

    def nobs(self):
        if self._nobs is None:
            self._parse_obs_arc()
        return self._nobs

    def arc_length(self):
        if self._arc_length is None:
            self._parse_obs_arc()
        return self._arc_length

    @property
    def current_time(self):
        """
        Current time for position predictions.

        @return: Time
        """
        if self._current_time is None:
            self._current_time = self._stop_time + (self._stop_time - self._start_time)/2.0
        return self._current_time

    @current_time.setter
    def current_time(self, current_time):
        self._current_time = Time(current_time, scale='utc')

    @property
    def coordinate(self):
        """
        Prediction position of the target at current_time

        @return: SkyCoord
        """
        ra = scipy.interp(self.current_time.jd,
                          self.ephemeris['Time'].jd,
                          self.ephemeris['R.A._(ICRF/J2000.0)']) * units.degree
        dec = scipy.interp(self.current_time.jd,
                           self.ephemeris['Time'].jd,
                           self.ephemeris['DEC_(ICRF/J2000.0)']) * units.degree
        return SkyCoord(ra, dec)

    @property
    def dra(self):
        """
        Uncertainty in the prediction location.

        @return: Quantity
        """

        dra = scipy.interp(self.current_time.jd,
                           self.ephemeris['Time'].jd,
                           self.ephemeris['RA_3sigma']) * units.arcsec

        return dra

    @property
    def ddec(self):
        """
        Uncertainty in the prediction location.

        @return: Quantity
        """
        return scipy.interp(self.current_time.jd,
                            self.ephemeris['Time'].jd,
                            self.ephemeris['DEC_3sigma']) * units.arcsec

    @property
    def pa(self):
        """
        Plane Of Sky angle of the ra/dec uncertainty ellipse.

        @return: Quantity
        """
        return scipy.interp(self.current_time.jd,
                            self.ephemeris['Time'].jd,
                            self.ephemeris['Theta']) * units.degree

    def predict(self, time):
        self.current_time = time
        if not (self._stop_time >= self.current_time >= self._start_time):
            self.start_time = time - 1.0*units.day
            self.stop_time = time + 1.0*units.day


def get(target):
    return Query(target)
