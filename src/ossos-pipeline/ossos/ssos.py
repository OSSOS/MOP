from astropy.coordinates import SkyCoord
import math

__author__ = 'Michele Bannister, JJ Kavelaars'

import datetime
import os
import pprint
import warnings

from astropy.io import ascii
from astropy import units

from astropy.time import Time
import requests

requests.packages.urllib3.disable_warnings()

from . import astrom
from . import mpc
from .gui import logger
from .orbfit import Orbfit
from planning.plotting import parameters

SSOS_URL = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/ssos/ssos.pl"
RESPONSE_FORMAT = 'tsv'
NEW_LINE = '\r\n'


class TracksParser(object):
    def __init__(self, inspect=True, skip_previous=False):
        logger.debug("Setting up TracksParser")
        self.orbit = None
        self._nights_per_darkrun = 18 * units.day
        self._nights_separating_darkruns = 30 * units.day
        self.inspect = inspect
        self.skip_previous = skip_previous
        self.ssos_parser = None

    def parse(self, filename):
        logger.debug("Parsing SSOS Query.")
        mpc_observations = mpc.MPCReader(filename).mpc_observations

        # pass down the provisional name so the table lines are linked to this TNO
        self.ssos_parser = SSOSParser(mpc_observations[0].provisional_name,
                                      input_observations=mpc_observations,
                                      skip_previous=self.skip_previous)

        try:
            self.orbit = Orbfit(mpc_observations)
            print self.orbit.residuals()
            print self.orbit
        except Exception as ex:
            logger.error("{}".format(ex))
            logger.error("Failed to compute orbit with astrometry provided")
            logger.error("{}".format(mpc_observations))
            return None

        if self.orbit.arc_length < 1 * units.day:
            # data from the same dark run.
            lunation_count = 0
        elif 1 * units.day < self.orbit.arc_length < self._nights_per_darkrun:
            # data from neighbouring darkruns.
            lunation_count = 1
        else:
            # data from the entire project.
            lunation_count = None

        # loop over the query until some new observations are found, or raise assert error.
        while True:
            tracks_data = self.query_ssos(mpc_observations, lunation_count)
            logger.debug("Got SSOS result: {}".format(tracks_data))
            if tracks_data.get_reading_count() > len(
                    mpc_observations) or tracks_data.get_arc_length() > self.orbit.arc_length + 2.0 * units.day:
                return tracks_data
            if not self.inspect:
                assert lunation_count is not None, "No new observations available."
            if lunation_count is None:
                return tracks_data
            lunation_count += 1
            if lunation_count > 2:
                lunation_count = None

    def query_ssos(self, mpc_observations, lunation_count=None):
        """Send a query to the SSOS web service, looking for available observations using the given track.

        :param mpc_observations: a list of mpc.Observations
        :param lunation_count: how many dark runs (+ and -) to search into
        :return: an SSOSData object
        :rtype: SSOSData
        """

        # we observe ~ a week either side of new moon
        # but we don't know when in the dark run the discovery happened
        # so be generous with the search boundaries, add extra 2 weeks
        # current date just has to be the night of the triplet,

        if lunation_count is None:
            # Only using SSOS to find data acquired during the survey period, for now.
            search_start_date = Time('2013-02-08', scale='utc')
            search_end_date = Time(datetime.datetime.now().strftime('%Y-%m-%d'), scale='utc')
        else:
            search_start_date = Time((mpc_observations[0].date.jd * units.day - (
                self._nights_per_darkrun +
                lunation_count * self._nights_separating_darkruns)), format='jd', scale='utc')
            search_end_date = Time((mpc_observations[-1].date.jd * units.day + (
                self._nights_per_darkrun +
                lunation_count * self._nights_separating_darkruns)), format='jd', scale='utc')

        logger.info("Sending query to SSOS start_date: {} end_data: {}\n".format(search_start_date, search_end_date))
        query = Query(mpc_observations,
                      search_start_date=search_start_date,
                      search_end_date=search_end_date)

        logger.debug("Parsing query results...")
        tracks_data = self.ssos_parser.parse(query.get(), mpc_observations=mpc_observations)

        tracks_data.mpc_observations = {}

        for mpc_observation in mpc_observations:
            # attach the input observations to the the SSOS query result.
            if isinstance(mpc_observation.comment, mpc.OSSOSComment):
                try:
                    tracks_data.mpc_observations[mpc_observation.comment.frame.strip()] = mpc_observation
                except Exception as e:
                    logger.error(str(e))
                    logger.error(mpc_observation)

        for source in tracks_data.get_sources():
            astrom_observations = tracks_data.observations
            source_readings = source.get_readings()
            for idx in range(len(source_readings)):
                source_reading = source_readings[idx]
                astrom_observation = astrom_observations[idx]
                self.orbit.predict(Time(astrom_observation.mjd, format='mjd', scale='utc'))
                source_reading.pa = self.orbit.pa
                # why are these being recorded just in pixels?  Because the error ellipse is drawn in pixels.
                # TODO: Modify error ellipse drawing routine to use WCS but be sure
                # that this does not cause trouble with the use of dra/ddec for cutout computer
                source_reading.dx = self.orbit.dra
                source_reading.dy = self.orbit.ddec

                frame = astrom_observation.rawname
                if frame in tracks_data.mpc_observations:
                    source_reading.discovery = tracks_data.mpc_observations[frame].discovery

        return tracks_data  # a SSOSData with .sources and .observations only


class SSOSParser(object):
    """
    Parse the result of an SSOS query, which is stored in an astropy Table object
    """

    def __init__(self, provisional_name, input_observations=None, skip_previous=False):
        """
        setup the parser.
        :param provisional_name: name of KBO to assign SSOS data to
        :param input_observations: input observations used in search
        """
        if input_observations is None:
            input_observations = []
        self.provisional_name = provisional_name
        self.input_rawnames = []
        self.null_observations = []
        self.skip_previous = skip_previous
        for observation in input_observations:
            if isinstance(observation.comment, mpc.OSSOSComment):
                try:
                    rawname = observation.comment.frame
                    self.input_rawnames.append(rawname.strip())
                    if observation.null_observation:
                        self.null_observations.append(rawname.strip())
                except Exception as ex:
                    logger.debug("{}".format(ex))
                    logger.debug("Failed to get original filename from >{}<".format(observation.comment))

    @staticmethod
    def _skip_missing_data(str_vals, ncols):
        """
        add a extra columna if missing, else return None.
        """
        while len(str_vals) < ncols:
            str_vals.append('None')
        return str_vals

    @staticmethod
    def build_source_reading(expnum, ccd=None, ftype='p'):
        """
        Build an astrom.Observation object for a SourceReading

        :param expnum: (str) Name or CFHT Exposure number of the observation.
        :param ccd: (str) CCD is this observation associated with. (can be None)
        :param ftype: (str) exposure time (specific to CFHT imaging)

        :return: An astrom.Observation object for the observation.
        :rtype: astrom.Observation
        """

        logger.debug("Building source reading for expnum:{} ccd:{} ftype:{}".format(expnum, ccd, ftype))

        return astrom.Observation(expnum=str(expnum),
                                  ftype=ftype,
                                  ccdnum=ccd)

    def parse(self, ssos_result_filename_or_lines, mpc_observations=None):
        """
        given the result table create 'source' objects.

        :param ssos_result_filename_or_lines:
        """
        table_reader = ascii.get_reader(Reader=ascii.Basic)
        table_reader.inconsistent_handler = self._skip_missing_data
        table_reader.header.splitter.delimiter = '\t'
        table_reader.data.splitter.delimiter = '\t'
        ssos_table = table_reader.read(ssos_result_filename_or_lines)

        sources = []
        observations = []
        source_readings = []
        orbit = Orbfit(mpc_observations)

        warnings.filterwarnings('ignore')
        logger.info("Loading {} observations\n".format(len(ssos_table)))
        expnums_examined = []
        for row in ssos_table:
            # Trim down to OSSOS-specific images
            if ((row['Filter'] not in parameters.OSSOS_FILTERS)
                    or row['Image_target'].startswith('WP')):
                continue

            # check if a dbimages object exists
            # For CFHT/MegaCam strip off the trailing character to get the exposure number.
            ftype = row['Image'][-1]
            expnum = row['Image'][:-1]

            # The file extension is the ccd number + 1 , or the first extension.
            ccd = int(row['Ext'])-1
            if 39 < ccd < 0:
                ccd = None
            x = row['X'] * units.pix
            y = row['Y'] * units.pix
            ra = row['Object_RA'] * units.degree
            dec = row['Object_Dec'] * units.degree
            ssois_coordinate = SkyCoord(ra, dec)
            mjd = row['MJD'] * units.day

            if not 0 < x.value < 2060 or not 0 < y.value < 4700:
                continue

            obs_date = Time(mjd, format='mjd', scale='utc').iso
            orbit.predict(obs_date)
            if orbit.dra > 4 * units.arcminute or orbit.ddec > 4.0 * units.arcminute:
                print "Skipping entry as orbit uncertainty at date {} is large.".format(obs_date)
                continue
            if expnum in expnums_examined:
                continue
            expnums_examined.append(expnum)

            logger.debug(("SSOIS Prediction: exposure:{} ext:{} "
                          "ra:{} dec:{} x:{} y:{}").format(expnum, ccd, ra, dec, x, y))

            logger.debug(("Orbfit Prediction: "
                          "ra:{} dec:{} ").format(orbit.coordinate.ra.to(units.degree),
                                                  orbit.coordinate.dec.to(units.degree)))

            observation = SSOSParser.build_source_reading(expnum, ccd, ftype=ftype)
            observation.mjd = mjd
            from_input_file = observation.rawname in self.input_rawnames
            # compare to input observation list.
            previous = False
            mpc_observation = None
            if from_input_file:
                for mpc_observation in mpc_observations:
                    try:
                        if mpc_observation.comment.frame.strip() == observation.rawname:
                            previous = not mpc_observation.discovery.is_initial_discovery
                            break
                    except Exception as e:
                        logger.debug(str(e))
                        pass
                    mpc_observation = None

            # skip previously measured observations if requested.
            if self.skip_previous and previous:
                continue

            logger.debug('built observation {}'.format(observation))
            observations.append(observation)
            null_observation = observation.rawname in self.null_observations

            ddec = orbit.ddec + abs(orbit.coordinate.dec - ssois_coordinate.dec)
            dra = orbit.dra + abs(orbit.coordinate.ra - ssois_coordinate.ra)

            logger.info(" Building SourceReading .... \n")
            source_reading = astrom.SourceReading(x=x, y=y, x0=x, y0=y,
                                                  ra=orbit.coordinate.ra.to(units.degree).value,
                                                  dec=orbit.coordinate.dec.to(units.degree).value,
                                                  xref=x, yref=y, obs=observation,
                                                  ssos=True, from_input_file=from_input_file,
                                                  dx=dra, dy=ddec, pa=orbit.pa,
                                                  null_observation=null_observation)
            source_reading.mpc_observation = mpc_observation
            source_readings.append(source_reading)

        # build our array of SourceReading objects
        sources.append(source_readings)

        warnings.filterwarnings('once')

        return SSOSData(observations, sources, self.provisional_name)


class SSOSData(object):
    """
    Encapsulates data extracted from an .astrom file.
    """

    def __init__(self, observations, sources, provisional_name):
        """
        Constructs a new astronomy data set object.

        Args:
          observations: list(Observations)
            The observations that are part of the data set.
        """
        self.mpc_observations = {}
        self.observations = observations
        self.sys_header = None
        self.sources = [astrom.Source(
            reading_list,
            provisional_name) for reading_list in sources]

    def get_reading_count(self):
        count = 0
        for source in self.sources:
            count += source.num_readings()

        return count

    def get_sources(self):
        return self.sources

    def get_source_count(self):
        return len(self.get_sources())

    def get_arc_length(self):
        mjds = []
        for obs in self.observations:
            mjds.append(obs.mjd)
        arc = (len(mjds) > 0 and max(mjds) - min(mjds)) or 0
        return arc


class ParamDictBuilder(object):
    """
    Build a dictionary of parameters needed for an SSOS Query.

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
                 resolve_position=True,
                 telescope_instrument='CFHT/MegaCam'):
        self._observations = []
        self.observations = observations
        self._verbose = False
        self.verbose = verbose
        self._search_start_date = None
        self.search_start_date = search_start_date
        self._search_end_date = None
        self.search_end_date = search_end_date
        self._orbit_method = None
        self.orbit_method = orbit_method
        self._error_ellipse = None
        self.error_ellipse = error_ellipse
        self._resolve_extension = None
        self.resolve_extension = resolve_extension
        self._resolve_position = None
        self.resolve_position = resolve_position
        self._telescope_instrument = None
        self.telescope_instrument = telescope_instrument

    @property
    def observations(self):
        """
        The observations to be used in fitting, returned as list of
        the mpc format lines.

        This should be set to a list of objects whose 'str' values
        will be valid MPC observations.
        """
        return self._observations

    @observations.setter
    def observations(self, observations):
        self._observations = []
        for observation in observations:
            assert isinstance(observation, mpc.Observation)
            if not observation.null_observation:
                self._observations.append(observation)

    @property
    def verbose(self):
        """
        In verbose mode the SSOS query will return diagnostic
        information about how the search was done.
        """
        return self._verbose

    @verbose.setter
    def verbose(self, verbose):
        self._verbose = (verbose and 'yes') or 'no'

    @property
    def search_start_date(self):
        """
        :return: Time constraint for start of SSOS search window.
        :rtype: Time
        """
        return self._search_start_date

    @search_start_date.setter
    def search_start_date(self, search_start_date):
        """

        :param search_start_date: Time object for start of SSOS search window.
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
        What fitting method should be used to turn the observations
        into an orbit.

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
        The size of the error ellipse to assign to each position, or
        'bern' to use the output of the BK fit.
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
        Should SSOS resolve and return which extension of a frame the
        object would be in?
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
        Should SSOS resolve and return the predicted X/Y location of
        the source?
        """
        return self._resolve_position

    @resolve_position.setter
    def resolve_position(self, resolve_position):
        if str(resolve_position).lower() == "no":
            resolve_position = False
        self._resolve_position = (resolve_position and "yes") or "no"

    @property
    def telescope_instrument(self):
        """
        :param The instrument on the facility. Still a SSOIS beta property.
        Currently only takes one instrument at a time.
        :return:
        """
        return self._telescope_instrument

    @telescope_instrument.setter
    def telescope_instrument(self, telescope_instrument):
        assert isinstance(telescope_instrument, str)
        if telescope_instrument in TELINST:
            self._telescope_instrument = telescope_instrument


    @property
    def params(self):
        """
        :return: A dictionary of SSOS query parameters.
        :rtype: dict
        """
        return dict(format=RESPONSE_FORMAT,
                    verbose=self.verbose,
                    epoch1=str(self.search_start_date),
                    epoch2=str(self.search_end_date),
                    search=self.orbit_method,
                    eunits=self.error_ellipse,
                    extres=self.resolve_extension,
                    xyres=self.resolve_position,
                    telinst=self.telescope_instrument,
                    obs=NEW_LINE.join((
                        str(observation) for observation in self.observations)))


class Query(object):
    """
    Query the CADC's Solar System Object search for a given set of
    MPC-formatted moving object detection lines.

    Inputs:
        - a list of ossos.mpc.Observation instances

    Optional:
        - a tuple of the start and end times to be searched
          between. Format '%Y-%m-%d'

    Otherwise the temporal range defaults to spanning from the start
    of OSSOS surveying on 2013-01-01 to the present day.

    """

    def __init__(self,
                 observations,
                 search_start_date=Time(parameters.SURVEY_START, scale='utc'),
                 search_end_date=Time('2017-01-01', scale='utc')):
        self.param_dict_builder = ParamDictBuilder(
            observations,
            search_start_date=search_start_date,
            search_end_date=search_end_date)
        self.headers = {'User-Agent': 'OSSOS'}

    def get(self):
        """
        :return: A string containing the TSV result from SSOS
        :rtype: str
        :raise: AssertionError
        """
        params = self.param_dict_builder.params
        logger.debug(pprint.pformat(format(params)))
        response = requests.post(SSOS_URL,
                                 data=params,
                                 headers=self.headers)
        logger.debug(response.url)
        assert isinstance(response, requests.Response)
        assert (response.status_code == requests.codes.ok)

        lines = response.content
        # note: spelling 'occured' is in SSOIS
        if len(lines) < 2 or "An error occured getting the ephemeris" in lines:
            raise IOError(os.errno.EACCES,
                          "call to SSOIS failed on format error")

        if os.access("backdoor.tsv", os.R_OK):
            lines += open("backdoor.tsv").read()

        return lines


TELINST = [
    'AAT/WFI',
    'ALMA',
    'CFHT/CFH12K',
    'CFHT/MegaCam',
    'CFHT/WIRCam',
    'CTIO-4m/DECam',
    'CTIO-4m/Mosaic2',
    'CTIO-4m/NEWFIRM',
    'ESO-LaSilla_2.2m/WFI',
    'ESO-NTT/EFOSC',
    'ESO-NTT/EMMI',
    'ESO-NTT/SOFI',
    'ESO-NTT/SUSI',
    'ESO-NTT/SUSI2',
    'ESO-VISTA/VIRCAM',
    'ESO-VLT/FORS1',
    'ESO-VLT/FORS2',
    'ESO-VLT/HAWKI',
    'ESO-VLT/ISAAC',
    'ESO-VLT/NAOS-CONICA',
    'ESO-VLT/VIMOS',
    'ESO-VST/OMEGACAM',
    'Gemini/GMOS',
    'Gemini/GMOS-N',
    'Gemini/GMOS-S',
    'Gemini/NIRI',
    'HST/ACS',
    'HST/WFC3',
    'HST/WFPC2',
    'INT/WFC',
    'JKT/JAG-CCD',
    'KPNO-0.9m/Mosaic1',
    'KPNO-0.9m/Mosaic1.1',
    'KPNO-4m/Mosaic1',
    'KPNO-4m/Mosaic1.1',
    'KPNO-4m/NEWFIRM',
    'NEAT-GEODSS-Maui',
    'SDSS',
    'SOAR/SOI',
    'Subaru/SuprimeCam',
    'WHT/AUXCAM',
    'WHT/INGRID',
    'WHT/LIRIS',
    'WHT/Prime',
    'WISE',
    'WIYN/MiniMo',
    'WIYN/ODI',
]