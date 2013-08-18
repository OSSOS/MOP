import datetime
import os
import urllib
import urllib2
import cStringIO
import astropy
from astropy.io import ascii
from astropy.time import Time
from astropy.table import Table

from mpc import URLWriter
import requests
from ossos import storage

__author__ = 'Michele Bannister'

SSOS_URL = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/ssos/ssos.pl"
RESPONSE_FORMAT = 'tsv'
# was set to \r\n ?
NEW_LINE = '\r\n'

class Parser(object):
    """
    Parse the result of an SSOS query, which is stored in an astropy Table object
    """
    def __init__(self):
        """
        setup the parser.
        """
    def parse(self, ssos_result_table):
        """
        given the result table create 'source' objects.

        :type ssos_result_table: Table
        :param ssos_result_table:
        """

        ssos_result_table

        for row in ssos_result_table:
            # check if a dbimages object exists
            dbimages_uri = storage.dbimages_uri(expnum=row['Image'],
                                                ccd=row['Ext']-1,
                                                version='p')
            if not storage.exists(dbimages_uri):
                continue
            x_ref = row['X']
            y_ref = row['Y']



class ParamDictBuilder(object):
    """
    Build a dictionary of parameters needed for and SSOS Query.

    This should be fun!
    """
    def __init__(self,
                 observations,
                 verbose=False,
                 search_start_date=Time('2013-01-01', scale='utc'),
                 search_end_date=Time('2017-01-01', scale='utc'),
                 orbit_method='bern',
                 error_ellipse='bern',
                 resolve_extension=True,
                 resolve_position=True):
        self.observations = observations
        self.verbose = verbose
        self.search_start_date = search_start_date
        self.search_end_date = search_end_date
        self.orbit_method = orbit_method
        self.error_ellipse = error_ellipse
        self.resolve_extension = resolve_extension
        self.resolve_position = resolve_position

    @property
    def observations(self):
        """
        The observtions to be used in fitting, returned as list of on the mpc format lines.

        This should be set to a list of objects whose 'str' values will be valid MPC observations.
        """
        return self._observations

    @observations.setter
    def observations(self, observations):
        self._observations = observations


    @property
    def verbose(self):
        """
        In verbose mode the SSOS query will return diagnoistic information about how the search was done.
        """
        return self._verbose

    @verbose.setter
    def verbose(self,verbose):
        self._verbose = ( verbose and 'yes') or 'no'

    @property
    def search_start_date(self):
        """
        astropy.io.Time object. The start date of SSOS search window.
        """
        return self._search_start_date

    @search_start_date.setter
    def search_start_date(self, search_start_date):
        """

        :type search_start_date: astropy.io.Time
        :param search_start_date: search for frames take after the given date.
        """
        assert isinstance(search_start_date, Time)
        self._search_start_date = search_start_date.replicate(format='iso')
        self._search_start_date.out_subfmt = 'date'

    @property
    def search_end_date(self):
        """
        astropy.io.Time object. The end date of SSOS search window.
        """
        return self._search_end_date

    @search_end_date.setter
    def search_end_date(self, search_end_date):
        """
        :type search_end_date: astropy.io.Time
        :param search_end_date: search for frames take after the given date.
        """
        assert isinstance(search_end_date, Time)
        self._search_end_date = search_end_date.replicate(format='iso')
        self._search_end_date.out_subfmt = 'date'

    @property
    def orbit_method(self):
        """
        What fitting method should be used to turn the observations into an orbit.

        Must be one of ['bern', 'mpc']
        """
        return self._orbit_method

    @orbit_method.setter
    def orbit_method(self, orbit_method):
        assert orbit_method in ['bern', 'mpc']
        self._orbit_method = orbit_method

    @property
    def error_ellipse(self):
        """
        The size of the error ellipse to assign to each position, or 'bern' to use the output of the BK fit.
        """
        return self._error_ellipse

    @error_ellipse.setter
    def error_ellipse(self, error_ellipse):
        """

        :param error_ellipse: either a number or the work 'bern'
        """
        if error_ellipse == 'bern':
            assert self.orbit_method == 'bern'
        else:
            error_ellipse = float(error_ellipse)
        self._error_ellipse = error_ellipse

    @property
    def resolve_extension(self):
        """
        Should SSOS resolve and return which extension of a frame the object would be in?
        """
        return self._resolve_extension

    @resolve_extension.setter
    def resolve_extension(self, resolve_extension):
        if str(resolve_extension).lower() == "no":
            resolve_extension = False
        self._resolve_extension = (resolve_extension and "yes") or "no"

    @property
    def resolve_position(self):
        """
        Should SSOS resolve and return the predicted X/Y location of the source?
        """
        return self._resolve_position

    @resolve_position.setter
    def resolve_position(self, resolve_position):
        if str(resolve_position).lower() == "no":
            resolve_position = False
        self._resolve_position = (resolve_position and "yes") or "no"

    @property
    def params(self):
        """
        The SSOS Query parameters as dictoinary, appropriate for url_encoding
        """
        return dict(format=RESPONSE_FORMAT,
                    verbose=self.verbose,
                    epoch1=str(self.search_start_date),
                    epoch2=str(self.search_end_date),
                    search=self.orbit_method,
                    eunits=self.error_ellipse,
                    extres=self.resolve_extension,
                    xyres=self.resolve_position,
                    obs=NEW_LINE.join((str(observation) for observation in self.observations))
        )



class Query(object):
    """
    Query the CADC's Solar System Object search for a given set of MPC-formatted moving object detection lines.

    Inputs:
        - a list of ossos.mpc.Observation instances
    Optional:
        - a tuple of the start and end times to be searched between. Format '%Y-%m-%d'
    Otherwise the temporal range defaults to spanning from the start of OSSOS surveying on 2013-01-01 to
    the present day.

    """


    def __init__(self, observations, search_start_date=Time('2013-01-01', scale='utc'),
                 search_end_date = Time('2017-01-01', scale='utc')):

        self.param_dict_biulder = ParamDictBuilder(observations,
                                                   search_start_date=search_start_date,
                                                   search_end_date=search_end_date)

        self.headers = {'User-Agent': 'OSSOS Target Track'}

    def get_table(self):

        """


        :return: astropy.table.table
        :raise: AssertionError
        """
        params = self.param_dict_biulder.params
        self.response = requests.get(SSOS_URL, params=params, headers=self.headers)


        assert isinstance(self.response, requests.Response)


        lines = self.response.content
        if str(lines[1]).startswith("An error occured getting the ephemeris"):
            raise IOError(os.errno.EBADRPC, "call to SSOS failed on format error")

        table_reader = ascii.get_reader(Reader=ascii.Basic)
        table_reader.inconsistent_handler = self._skip_missing_datalink
        table_reader.header.splitter.delimiter = '\t'
        table_reader.data.splitter.delimiter = '\t'
        table = table_reader.read(lines)

        return table

    def _skip_missing_datalink(self, str_vals, ncols):
        """
        add a extra column if one is missing, else return None.
        """
        if len(str_vals) == 12 and not "cadc" in str(str_vals[-1]).lower():
            # the 'DataLink' column is empty if no link available...
            # which should not happen for CADC data.
            str_vals.append('None')
            return str_vals
        else:
            return None
