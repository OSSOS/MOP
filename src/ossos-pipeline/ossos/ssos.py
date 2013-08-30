__author__ = 'Michele Bannister'

import cStringIO
import datetime
import os
import warnings

from astropy.io import ascii, fits
from astropy.table import Table
from astropy.time import Time
import requests

from ossos import astrom
from ossos.downloads.core import Downloader
from ossos.gui import logger
from ossos import mpc
from ossos.orbfit import Orbfit
from ossos import storage
from ossos import wcs

MAXCOUNT = 30000

SSOS_URL = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/ssos/ssos.pl"
RESPONSE_FORMAT = 'tsv'
# was set to \r\n ?
NEW_LINE = '\r\n'


mopheaders = {}
astheaders = {}

class TracksParser(object):

    def __init__(self):
        self._nights_per_darkrun = 18
        self._nights_separating_darkruns = 30

    def parse(self, filename):
        filehandle = storage.open_vos_or_local(filename, "rb")
        filestr = filehandle.read()
        filehandle.close()

        input_mpc_lines = filestr.split('\n')

        mpc_observations = []
        for line in input_mpc_lines:
            mpc_observation = mpc.Observation.from_string(line)
            if mpc_observation is not None:
                mpc_observations.append(mpc_observation)

        mpc_observations.sort(key=lambda obs: obs.date.jd)


        # pass down the provisional name so the table lines are linked to this TNO
        self.ssos_parser=SSOSParser(mpc_observations[0].provisional_name,
                                    input_observations=mpc_observations)

        self.orbit = Orbfit(mpc_observations)

        print self.orbit
        print self.orbit.residuals

        self.orbit.predict('2013-04-01')  # HARDWIRING FOR E BLOCK FOR NOW
        coord1 = self.orbit.coordinate
        self.orbit.predict('2013-05-01')
        coord2 = self.orbit.coordinate
        motion_rate = coord1.separation(coord2).arcsecs/(self.orbit.arc_length*60.)  # how best to get arcsec moved between first/last?
        print "{:>10s} {:8.2f}".format('rate ("/hr)', motion_rate)

        self.orbit.predict('2014-04-04')  # hardwiring next year's prediction date for the moment
        print "{:>10s} {:8.2f} {:8.2f}\n".format("Expected accuracy on 4 April 2014 (arcsec)", self.orbit.dra, self.orbit.ddec)

        length_of_observation_arc = mpc_observations[-1].date.jd - mpc_observations[0].date.jd

        if  length_of_observation_arc < 1:
            # data from the same dark run.
            lunation_count = 0
        elif length_of_observation_arc > 1 and length_of_observation_arc < self._nights_per_darkrun :
            # data from neighbouring darkruns.
            lunation_count = 1
        else:
            # data from the entire project.
            lunation_count = None

        # loop over the query until some new observations are found, or raise assert error.
        while True:
            tracks_data = self.query_ssos(mpc_observations, lunation_count)

            print len(mpc_observations), tracks_data.get_reading_count(), lunation_count
            if ( tracks_data.get_arc_length() > length_of_observation_arc or
                tracks_data.get_reading_count() > len(mpc_observations) ) :
                return tracks_data
            assert lunation_count is not None, "No new observations available."
            lunation_count += 1
            if lunation_count > 2 :
                lunation_count = None

    def query_ssos(self, mpc_observations, lunation_count):
        # we observe ~ a week either side of new moon
        # but we don't know when in the dark run the discovery happened
        # so be generous with the search boundaries, add extra 2 weeks
        # current date just has to be the night of the triplet,

        """

        :param mpc_observations: a list of mpc.Observations
        :param lunation_count: how many dark runs (+ and -) to search into
        :return: an SSOSData object
        """
        if lunation_count is None:
            search_start_date = Time('2013-02-08', scale='utc')
            search_end_date = Time(datetime.datetime.now().strftime('%Y-%m-%d'), scale='utc')
        else:
            search_start_date = Time((mpc_observations[0].date.jd - (
                self._nights_per_darkrun +
                lunation_count*self._nights_separating_darkruns) ),
                                     format='jd', scale='utc')
            search_end_date = Time((mpc_observations[-1].date.jd + (
                self._nights_per_darkrun +
                lunation_count*self._nights_separating_darkruns) ),
                                   format='jd', scale='utc')

        query = Query(mpc_observations,
                      search_start_date=search_start_date,
                      search_end_date=search_end_date)

        tracks_data = self.ssos_parser.parse(query.get())

        tracks_data.mpc_observations = {}
        for mpc_observation in mpc_observations:
            assert isinstance(mpc_observation,mpc.Observation)
            tracks_data.mpc_observations[mpc_observation.comment.frame] = mpc_observation

        for source in tracks_data.get_sources():
            astrom_observations = tracks_data.observations
            source_readings = source.get_readings()
            for idx in range(len(source_readings)):
                source_reading = source_readings[idx]
                astrom_observation = astrom_observations[idx]
                logger.info("About to call orbfit predict")
                self.orbit.predict(astrom_observation.header['MJD_OBS_CENTER'])
                logger.info("Finished predict")
                source_reading.pa = self.orbit.pa
                # why are these being recorded just in pixels?
                source_reading.dra = self.orbit.dra / astrom_observation.header['SCALE']
                source_reading.ddec = self.orbit.ddec / astrom_observation.header['SCALE']

                frame = astrom_observation.rawname
                if frame in tracks_data.mpc_observations:
                    source_reading.discovery = tracks_data.mpc_observations[frame].discovery

        return tracks_data  # a SSOSData with .sources and .observations only


class SSOSParser(object):
    """
    Parse the result of an SSOS query, which is stored in an astropy Table object
    """
    def __init__(self, provisional_name, input_observations=[], null_observations=[]):
        """
        setup the parser.
        """
        self.provisional_name = provisional_name
        self.input_rawnames = []
        self.null_observations = []
        for observation in input_observations:
            rawname = observation.comment.frame
            self.input_rawnames.append(rawname)
            if observation.null_observation:
                self.null_observations.append(rawname)

    def _skip_missing_data(self, str_vals, ncols):
        """
        add a extra column if one is missing, else return None.
        """
        if len(str_vals) == ncols - 1:
            str_vals.append('None')
            return str_vals
        else:
            raise ValueError("not enough columns in table")

    def parse(self, ssos_result_filename_or_lines):
        """
        given the result table create 'source' objects.

        :type ssos_result_table: Table
        :param ssos_result_table:
        """
        table_reader = ascii.get_reader(Reader=ascii.Basic)
        table_reader.inconsistent_handler = self._skip_missing_data
        table_reader.header.splitter.delimiter = '\t'
        table_reader.data.splitter.delimiter = '\t'
        table = table_reader.read(ssos_result_filename_or_lines)

        sources = []
        observations = []
        source_readings = []

        ref_pvwcs = None
        downloader = Downloader()
        warnings.filterwarnings('ignore')

        for row in table:
            # check if a dbimages object exists
            ccd = int(row['Ext']) - 1
            expnum = row['Image'].rstrip('p')

            # ADDING THIS TEMPORARILY TO GET THE NON-OSSOS DATA OUT OF THE WAY WHILE DEBUGGING
            if (row['Telescope_Insturment'] != 'CFHT/MegaCam') or (row['Filter'] != 'r.MP9601'):
                continue

            # it's fine for OSSOS, go get the image
            image_uri = storage.dbimages_uri(expnum=expnum,
                                             ccd=None,
                                             version='p',
                                             ext='.fits',
                                             subdir=None)
            logger.info('Trying to access %s\n%s' % (row.data, image_uri))

            if not storage.exists(image_uri, force=False):
                logger.warning('Image not in dbimages? Trying subdir.')
                image_uri = storage.dbimages_uri(expnum=expnum,
                                                 ccd=ccd,
                                                 version='p')

                if not storage.exists(image_uri, force=False):
                    logger.warning("Image doesn't exist in ccd subdir. %s" % image_uri)
                    continue

            if row['X'] == -9999 or row['Y'] == -9999 :
                logger.warning("Skipping %s as x/y not resolved." % ( row['Image']))
                continue

            mopheader_uri = storage.dbimages_uri(expnum=expnum,
                                                 ccd=ccd,
                                                 version='p',
                                                 ext='.mopheader')

            if not mopheader_uri in mopheaders:
                if not storage.exists(mopheader_uri, force=False):
                    logger.warning('mopheader missing, but images exists')
                    continue

                # raise flag if no MOPHEADER
                mopheader_fpt = cStringIO.StringIO(storage.open_vos_or_local(mopheader_uri).read())
                mopheader = fits.open(mopheader_fpt)
                mopheaders[mopheader_uri] = mopheader
            mopheader = mopheaders[mopheader_uri]
            
            # Build astrom.Observation
            observation = astrom.Observation(expnum=str(expnum),
                                             ftype='p',
                                             ccdnum=str(ccd),
                                             fk="")

            observation.rawname = os.path.splitext(os.path.basename(image_uri))[0]+str(ccd).zfill(2)

            observation.header = mopheader[0].header
            MJD_OBS_CENTER = mpc.Time(observation.header['MJD-OBSC'],
                                      format='mjd',
                                      scale='utc', precision=5 ).replicate(format='mpc')
            observation.header['MJD_OBS_CENTER'] = str(MJD_OBS_CENTER)
            observation.header['MAXCOUNT'] = MAXCOUNT
            observation.header['SCALE'] = observation.header['PIXSCALE']
            #observation.header['CHIP'] = str(observation.header['CHIPNUM']).zfill(2)
            observation.header['NAX1'] = observation.header['NAXIS1']
            observation.header['NAX2'] = observation.header['NAXIS2']
            observation.header['MOPversion'] = observation.header['MOP_VER']
            observation.header['FWHM'] = 4



            # a download pixel 1,1 of this data to due offsets with.
            x_cen = int(min(max(1,row['X']),observation.header['NAX1']))
            y_cen = int(min(max(1,row['Y']),observation.header['NAX2']))
            if image_uri not in astheaders:
               hdulist = downloader.download_hdulist(
                   uri=image_uri,
                   view='cutout',
                   cutout='[{}][{}:{},{}:{}]'.format(ccd+1, x_cen, x_cen, y_cen, y_cen))
               astheaders[image_uri] = hdulist
            hdulist = astheaders[image_uri]

            pvwcs = wcs.WCS(hdulist[0].header)
            (ra,dec)  = pvwcs.xy2sky(x_cen, y_cen)
            if ref_pvwcs is None:
                ref_pvwcs = pvwcs
                xref = row['X']
                yref = row['Y']
            (x0, y0) = ref_pvwcs.sky2xy(ra,dec)
            x0 += row['X'] - x_cen
            y0 += row['Y'] - y_cen

            # Build astrom.SourceReading
            observations.append(observation)

            from_input_file = observation.rawname in self.input_rawnames
            null_observation = observation.rawname in self.null_observations

            print observation.rawname, observation.header['MJD_OBS_CENTER'], null_observation, from_input_file

            source_reading = astrom.SourceReading(x=row['X'], y=row['Y'],
                                                        xref=xref, yref=yref,
                                                        x0=x0, y0=y0,
                                                        ra=row['Object_RA'], dec=row['Object_Dec'],
                                                        obs=observation,
                                                        ssos=True,
                                                        from_input_file=from_input_file,
                                                        null_observation=null_observation)
            #if observation.rawname in  self.input_rawnames:
            #    source_readings.insert(0, source_reading)
            #else:
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
          sys_header: dict
            Key-value pairs of system settings applicable to the data set.
            Ex: RMIN, RMAX, ANGLE, AWIDTH
          sources: list(list(SourceReading))
            A list of point sources found in the data set.  These are
            potential moving objects.  Each point source is itself a list
            of source readings, one for each observation in
            <code>observations</code>.  By convention the ordering of
            source readings must match the ordering of the observations.
        """
        self.mpc_observations = {}
        self.observations = observations
        self.sys_header = None
        self.sources = [astrom.Source(reading_list, provisional_name) for reading_list in sources]

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

            mjds.append(Time(obs.header['MJD_OBS_CENTER'], format='mpc', scale='utc').jd)
        arc = (len(mjds) > 0 and max(mjds) - min(mjds) ) or 0
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

    def get(self):
        """
        :return: astropy.table.table
        :raise: AssertionError
        """
        params = self.param_dict_biulder.params
        self.response = requests.get(SSOS_URL, params=params, headers=self.headers)

        assert isinstance(self.response, requests.Response)

        assert(self.response.status_code == requests.codes.ok )

        lines = self.response.content
        if len(lines) < 1 or str(lines[1]).startswith("An error occurred getting the ephemeris"):
            raise IOError(os.errno.EACCES, "call to SSOS failed on format error")

        return lines

