"""
send queries to the Horizons batch query system via a web UI.
"""
import copy
import re
import requests
from astropy.time import Time
from astropy import units
from astropy.units.quantity import Quantity
from astropy.io.ascii import Csv


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
    default_quantities = [1, 3, 9]

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
                               'QUANTITIES': "'{}'".format(default_quantities),
                               'REF_SYSTEM': "'J2000'",
                               'SKIP_DAYLT': "'NO'",
                               'EXTRA_PREC': "'NO'",
                               'R_T_S_ONLY': "'NO'",
                               'CSV_FORMAT': "'YES'"}

    def __init__(self, target):
        """

        @return:
        """
        self.target = target
        self._params = copy.copy(Query.default_horizons_params)
        self._quantities = [1, 3, 9]
        self._data = None
        self._ephemeris = None
        self._elements = None
        self._start_time = Time.now()
        self._stop_time = Time.now() + 1*units.day
        self._step_size = 1*units.day
        self._center = 568

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
        return "'{}'".format(s[1:-2])

    @quantities.setter
    def quantities(self, quantities=None):
        if quantities is None:
            self._quantities = Query.default_quantities
            return

        self._quantities = []
        for quantity in quantities:
            try:
                idx = int(quantity)
            except:
                idx = Query.horizons_quantities.index(quantity)
            if idx is not None:
                self._quantities.append(idx)

    @property
    def start_time(self):
        return "'JD {}'".format(self._start_time.jd)

    @start_time.setter
    def start_time(self, start_time):
        assert isinstance(start_time, Time)
        self._start_time = start_time

    @property
    def stop_time(self):
        return "'JD {}'".format(self._stop_time.jd)

    @stop_time.setter
    def stop_time(self, stop_time):
        assert isinstance(stop_time, Time)
        self._stop_time = stop_time

    @property
    def step_size(self):
        s = "{:1.0f}".format(self._step_size * self._step_size)[:3]
        return "'{}'".format(s)

    @step_size.setter
    def step_size(self, step_size):
        assert isinstance(step_size, Quantity)
        self._step_size = step_size

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
        self._ephemeris = csv.read(csv_lines)

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

def get(target):
    return Query(target)