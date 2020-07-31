"""
Reads and writes .astrom files.
"""
import math

__author__ = "David Rusk <drusk@uvic.ca>"
import os
import re
import sys

from astropy import units
from astropy.coordinates import SkyCoord
from astropy.units import Quantity
from astropy.time import TimeDelta, Time
from . import util

from .gui import logger
from . import storage

DATASET_ROOT = storage.DBIMAGES

# Images from CCDs < 18 have their coordinates flipped
MAX_INVERTED_CCD = 17
INVERTED_CCDS = list(range(0, 18))
INVERTED_CCDS.append(36)
INVERTED_CCDS.append(37)

HEADER_LINE_LENGTH = 80

FAKE_PREFIX = "fk"

OBS_LIST_PATTERN = "#\s+(?P<rawname>(?P<fk>%s)?(?P<expnum>\d{6,7})(?P<ftype>[ops])(?P<ccdnum>\d+))" % FAKE_PREFIX

STATIONARY_LIST_PATTERN = "(?P<rawname>(?P<fk>fk)?(?P<expnum>\d{6,7})(?P<ftype>[ops])).vetting"

# Observation header keys
MOPVERSION = "MOPversion"

# NOTE: MJD_OBS_CENTER is actually MJD-OBS-CENTER in the .astrom files, but
# dashes aren't valid as regex names so I use underscores
MJD_OBS_CENTER = "MJD_OBS_CENTER"
EXPTIME = "EXPTIME"
THRES = "THRES"
FWHM = "FWHM"
MAXCOUNT = "MAXCOUNT"
CRVAL1 = "CRVAL1"
CRVAL2 = "CRVAL2"
EXPNUM = "EXPNUM"
SCALE = "SCALE"
CHIP = "CHIP"
CRPIX1 = "CRPIX1"
CRPIX2 = "CRPIX2"
NAX1 = "NAX1"
NAX2 = "NAX2"
DETECTOR = "DETECTOR"
PHADU = "PHADU"
RDNOIS = "RDNOIS"

# System header keys
RMIN = "RMIN"
RMAX = "RMAX"
ANGLE = "ANGLE"
AWIDTH = "AWIDTH"


def parse(filename):
    return AstromParser().parse(filename)


def parse_sources(filename):
    return parse(filename).get_sources()


class AstromFormatError(Exception):
    """Base class for errors in working with Astrom files."""


class AstromParser(object):
    """
    Parses a .astrom file (our own format) which specifies exposure numbers,
    identified point sources, their x, y location, source readings for
    potential moving objects, etc.
    """

    def __init__(self):
        """Creates the parser"""

        # Set up the regexes need to parse each section of the .astrom file

        self.obs_list_regex = re.compile(OBS_LIST_PATTERN)

        self.obs_header_regex = re.compile(
            "##\s+MOPversion\s+#\s+"
            "(?P<MOPversion>\d+\.[\d\w]+)\s+"
            "##\s+MJD-OBS-CENTER\s+EXPTIME\s+THRES\s+FWHM\s+MAXCOUNT\s+CRVAL1\s+CRVAL2\s+EXPNUM\s+"
            "#\s+(?P<MJD_OBS_CENTER>\d{4} \d{2} \d+\.\d+)\s+"
            "(?P<EXPTIME>\d+\.\d+)\s+"
            "(?P<THRES>\d+\.\d+)\s+"
            "(?P<FWHM>\d+\.\d+)\s+"
            "(?P<MAXCOUNT>\d+\.\d+)\s+"
            "(?P<CRVAL1>-?\d+\.\d+)\s+"
            "(?P<CRVAL2>-?\d+\.\d+)\s+"
            "(?P<EXPNUM>\d+)\s+"
            "##\s+SCALE\s+CHIP\s+CRPIX1\s+CRPIX2\s+NAX1\s+NAX2\s+DETECTOR\s+PHADU\s+RDNOIS\s+#\s+"
            "(?P<SCALE>\d+\.\d+)\s+"
            "(?P<CHIP>\d+)\s+"
            "(?P<CRPIX1>-?\d+\.\d+)\s+"
            "(?P<CRPIX2>-?\d+\.\d+)\s+"
            "(?P<NAX1>\d+)\s+"
            "(?P<NAX2>\d+)\s+"
            "(?P<DETECTOR>\w+)\s+"
            "(?P<PHADU>\d+\.\d+)\s+"
            "(?P<RDNOIS>\d+\.\d+)"
        )

        self.sys_header_regex = re.compile(
            "##\s+RMIN\s+RMAX\s+ANGLE\s+AWIDTH\s+#\s+"
            "(?P<RMIN>\d+\.\d+)\s+"
            "(?P<RMAX>\d+\.\d+)\s+"
            "(?P<ANGLE>-?\d+\.\d+)\s+"
            "(?P<AWIDTH>\d+\.\d+)"
        )

        self.source_list_reg = re.compile(
            "##\s+X\s+Y\s+X_0\s+Y_0\s+R.A.\s+DEC\s+(.*)",
            re.DOTALL
        )
        # Should we only load the discovery images during Candidate vetting?
        self.discovery_only = False

    def _parse_observation_list(self, filestr):
        matches = self.obs_list_regex.findall(filestr)  # returns list of tuples
        return [Observation.from_parse_data(*match) for match in matches]

    def _parse_observation_headers(self, filestr, observations):
        obsnum = 0
        for match in self.obs_header_regex.finditer(filestr):
            obs = observations[obsnum]
            for header_key, header_val in match.groupdict().items():
                obs.header[header_key] = header_val
            obsnum += 1

        assert obsnum == len(observations), ("Number of observations headers "
                                             "parsed doesn't match length of "
                                             "observation list")

    def _parse_system_header(self, filestr):
        sys_header_match = self.sys_header_regex.search(filestr)

        assert sys_header_match is not None, "Could not parse system header"

        return sys_header_match.groupdict()

    def _parse_source_data(self, file_str, observations):
        source_list_match = self.source_list_reg.search(file_str)

        assert source_list_match is not None, "Could not find the source list"

        raw_source_list = (source_list_match.group(1)).split("\n\n")

        sources = []
        for raw_source in raw_source_list:
            source = []
            source_obs = raw_source.strip().split('\n')
            assert len(source_obs) == len(
                observations), ("Source doesn't have same number of observations"
                                " ({0:d}) as in observations list ({1:d}).".format(len(source_obs), len(observations)))

            x_0 = []
            y_0 = []
            x_ref = None
            y_ref = None
            for i, source_ob in enumerate(source_obs):
                fields = [float(x) for x in source_ob.split()]
                x_0.append(fields[2])
                y_0.append(fields[3])
                if i == 0:
                    x_ref = fields[0]
                    y_ref = fields[1]
                fields.append(x_ref)
                fields.append(y_ref)
                # Find the observation corresponding to this reading
                fields.append(observations[i])

                source.append(SourceReading(*fields))

            # Add an ra/dec reference to the source.
            ref_index = int(math.ceil(len(source) / 2.0)) - 1
            for reading in source:
                assert isinstance(reading, SourceReading)
                reading.reference_sky_coord = source[ref_index].sky_coord

            # determine the smallest cutout that will include the reference coordinate and all the readings
            min_cutout = 30 * units.arcsec
            for reading in source:
                sep = reading.reference_sky_coord.separation(reading.sky_coord)
                if min_cutout < sep:
                    min_cutout = sep
            # Overload the 'uncertainty' criterion to ensure we get a large enough cutout.
            for fields in source:
                assert isinstance(fields, SourceReading)
                fields.uncertainty_ellipse.a = sep/2.5
                fields.uncertainty_ellipse.b = sep/2.5
                fields.uncertainty_ellipse.pa = 0.0 * units.degree

            sources.append(source)

        return sources

    def parse(self, filename):
        """
        Parses a file into an AstromData structure.

        Args:
          filename: str
            The name of the file whose contents will be parsed.

        Returns:
          data: AstromData
            The file contents extracted into a data structure for programmatic
            access.
        """
        filehandle = storage.open_vos_or_local(filename, "rb")
        assert filehandle is not None, "Failed to open file {} ".format(filename)
        filestr = filehandle.read().decode('utf-8')
        filehandle.close()

        assert filestr is not None, "File contents are None"

        observations = self._parse_observation_list(str(filestr))

        self._parse_observation_headers(filestr, observations)

        sys_header = self._parse_system_header(filestr)

        sources = self._parse_source_data(filestr, observations)

        return AstromData(observations, sys_header, sources, discovery_only=self.discovery_only)


class StationaryParser(AstromParser):

    def __init__(self, discovery_only=True):
        super(StationaryParser, self).__init__()
        self.observations = []
        self.obs_list_regex = re.compile(STATIONARY_LIST_PATTERN)
        self.discovery_only = discovery_only
        self.num_of_images_to_display = None

    def _parse_observation_list(self, filestr):
        pass

    def _parse_observation_headers(self, filestr, observations):
        """
        This should provide a list of headers that go with the given observation, as read from the input
        file, but for stationary sources we grab those from the storage system.

        @param filestr:  string to scan for observation header info.
        @param observations:  list of observations whose headers will be augmented by the contents in filestr
        @return:

        """
        try:
            self.num_of_images_to_display = int(filestr.split("\n")[0])
        except:
            pass


    def _parse_system_header(self, filestr):
        """
        For stationary catalog check we don't have this information.
        @param filestr: string to match for system header information.
        @return: dict
        """
        return {'RMIN': 0.01, 'RMAX': 0.2, 'ANGLE': 0, 'AWIDTH': 90}

    def _parse_source_data(self, file_str, observations):

        sources = []
        observations = {}
        raw_source_list = file_str.split("\n")
        for raw_source in raw_source_list:
            readings = []
            if not len(raw_source) > 0:
                continue
            if raw_source.strip()[0] == "!" or raw_source.strip()[0] == "#":
                continue
            parts = raw_source.split()
            if not len(parts) > 5:
                if len(readings) > 0:
                    raise AstromFormatError("line in vetting file has too few elements in line: {}".format(raw_source))
                else:
                    continue
            object_id = parts[0]
            ra = float(parts[1])
            dec = float(parts[2])
            file_ids = parts[3:]
            count = 0
            for file_id in file_ids:
                match_groups = re.match("(?P<expnum>\d{6,7})(?P<ftype>[ops])(?P<ccdnum>\d{2})", file_id)
                assert match_groups is not None, "Failed to parse file_id in stationary line"
                expnum = int(match_groups.group('expnum'))
                ftype = match_groups.group('ftype')
                ccd = match_groups.group('ccdnum')
                observation = Observation(expnum, ftype=ftype, ccdnum=ccd)
                source_reading = SourceReading(-1, -1, -1, -1,
                                               ra, dec, -1, -1,
                                               observation,
                                               discovery=self.num_of_images_to_display is None or count < self.num_of_images_to_display
                                               )
                source_reading.reference_sky_coord = source_reading.sky_coord
                source_reading.object_id = object_id
                readings.append(source_reading)
                count += 1
            sources.append(readings)
        return sources


class BaseAstromWriter(object):
    """
    Provides base functionality for AstromWriters.  Use the subclass for your
    use case.
    """

    def __init__(self, filehandle):
        self.output_file = filehandle

        self._header_written = False

    def get_filename(self):
        return self.output_file.name

    def _write_line(self, line, ljust=True):
        if ljust:
            line = line.ljust(HEADER_LINE_LENGTH)

        self.output_file.write(line + "\n")

    def _write_blank_line(self):
        self._write_line("", ljust=False)

    def _write_observation_list(self, observations):
        for observation in observations:
            self._write_line("# %s" % observation.rawname)

    def _write_observation_headers(self, observations):
        """
        See src/pipematt/step1matt-c
        """
        for observation in observations:
            header = observation.header

            def get_header_vals(header_list):
                header_vals = []
                for key in header_list:
                    val = header[key]

                    if key == MJD_OBS_CENTER:
                        header_vals.append(val)
                    elif key == DETECTOR:
                        header_vals.append(val.ljust(20))
                    else:
                        header_vals.append(float(val))

                return tuple(header_vals)

            self._write_line("## MOPversion")
            self._write_line("#  %s" % header[MOPVERSION])
            self._write_line("## MJD-OBS-CENTER  EXPTIME THRES FWHM  MAXCOUNT CRVAL1     CRVAL2     EXPNUM")
            self._write_line("# %s%8.2f%6.2f%6.2f%9.1f%11.5f%11.5f%9d" % get_header_vals(
                [MJD_OBS_CENTER, EXPTIME, THRES, FWHM, MAXCOUNT, CRVAL1, CRVAL2, EXPNUM]))
            self._write_line("## SCALE CHIP CRPIX1    CRPIX2    NAX1  NAX2   DETECTOR           PHADU RDNOIS")
            self._write_line("# %6.3f%4d%10.2f%10.2f%6d%6d %s%5.2f %5.2f" % get_header_vals(
                [SCALE, CHIP, CRPIX1, CRPIX2, NAX1, NAX2, DETECTOR, PHADU, RDNOIS]))

    def _write_sys_header(self, sys_header):
        """
        See src/pipematt/step3matt-c
        """
        header_vals = [sys_header[RMIN], sys_header[RMAX], sys_header[ANGLE],
                       sys_header[AWIDTH]]
        self._write_line("##     RMIN    RMAX   ANGLE   AWIDTH")
        self._write_line("# %8.1f%8.1f%8.1f%8.1f" % tuple(map(float, header_vals)))

    def _write_source_data(self, sources):
        """
        See src/jjk/measure3
        """
        for i, source in enumerate(sources):
            self._write_source(source)

    def _write_source(self, source):
        self._write_blank_line()

        for reading in source.get_readings():
            self._write_line(" %8.2f %8.2f %8.2f %8.2f %12.7f %12.7f" % (
                reading.x, reading.y, reading.x0, reading.y0, reading.ra,
                reading.dec), ljust=False)

    def _write_source_header(self):
        self._write_line("##   X        Y        X_0     Y_0          R.A.          DEC")

    def write_headers(self, observations, sys_header):
        """
        Writes the header part of the astrom file so that only the source
        data has to be filled in.
        """
        if self._header_written:
            raise AstromFormatError("Astrom file already has headers.")

        self._write_observation_list(observations)
        self._write_observation_headers(observations)
        self._write_sys_header(sys_header)
        self._write_source_header()

        self._header_written = True

    def flush(self):
        self.output_file.flush()

    def close(self):
        self.output_file.close()


class StreamingAstromWriter(BaseAstromWriter):
    """
    Use if you want to write out sources one-by-one as they are validated.
    See also BulkAstromWriter.
    """

    def __init__(self, filehandle, sys_header):
        super(StreamingAstromWriter, self).__init__(filehandle)
        self.sys_header = sys_header

        # Allow that the headers might have been written out in a previous
        # session by a different writer object.  In this case we just want
        # to be able to add more sources.
        self.output_file.seek(0)
        if re.match(OBS_LIST_PATTERN, self.output_file.read()):
            self._header_written = True

    def write_source(self, source):
        """
        Writes out data for a single source.
        """
        if not self._header_written:
            observations = [reading.get_observation() for reading in source.get_readings()]
            self.write_headers(observations, self.sys_header)

        self._write_source(source)


class BulkAstromWriter(BaseAstromWriter):
    """
    Use if you want to write out an entire AstromData structure at once.
    See also StreamingAstromWriter.
    """

    def __init__(self, filehandle):
        super(BulkAstromWriter, self).__init__(filehandle)

    def write_astrom_data(self, astrom_data):
        """
        Writes a full AstromData structure at once.
        """
        self.write_headers(astrom_data.observations, astrom_data.sys_header)
        self._write_source_data(astrom_data.sources)


class AstromData(object):
    """
    Encapsulates data extracted from an .astrom file.
    """

    def __init__(self, observations, sys_header, sources, discovery_only=False):
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
          discovery_only: bool
            should we only use the discovery images on the first pass?
        """
        self.observations = observations
        self.mpc_observations = {}
        self.sys_header = sys_header
        self.sources = [Source(reading_list, discovery_only=discovery_only) for reading_list in sources]

    def get_reading_count(self):
        count = 0
        for source in self.sources:
            count += source.num_readings()

        return count

    def get_sources(self):
        return self.sources

    def get_source_count(self):
        return len(self.get_sources())


class Source(object):
    """
    A collection of source readings.
    """

    def __init__(self, readings, provisional_name=None, discovery_only=False):
        self.readings = readings
        self.provisional_name = provisional_name
        self.discovery_only = discovery_only
        if 4 > self.num_readings() > 1:
            self.set_min_cutout()

    def set_min_cutout(self):
        x = []
        y = []
        for reading in self.readings:
            x.append(reading.x0)
            y.append(reading.y0)
        dx = max(x) - min(x)
        dy = max(y) - min(y)
        for reading in self.readings:
            reading.dx = dx+20
            reading.dy = dy+20

    def get_reading(self, index):
        return self.readings[index]

    def get_readings(self):
        return self.readings

    def num_readings(self):
        return len(self.readings)

    def has_provisional_name(self):
        return self.provisional_name is not None

    def get_provisional_name(self):
        return self.provisional_name

    def set_provisional_name(self, provisional_name):
        self.provisional_name = provisional_name


class Ellipse(object):
    """
    An ellipse region for use in DS9.

    a = semi-major axis
    b = semi-minor axis
    pa = position angle in degrees (East is 0).

    """

    def __init__(self, a, b, pa):
        """
        :param a: semi-major axis
        :type a: Quantity
        :param b: semi-minor axis
        :type b: Quantity
        :param pa: Position Angle (East is 0)
        :type pa: Quantity
        """
        assert isinstance(a, Quantity)
        assert isinstance(b, Quantity)
        assert isinstance(pa, Quantity)
        self._a = a
        self._b = b
        self._pa = pa

    def __str__(self):
        return '{}", {}", {}'.format(self.a.to(units.arcsec).value,
                                     self.b.to(units.arcsec).value,
                                     self.pa.to(units.degree).value + 90)

    @property
    def a(self):
        """
        The semi-major axis of the ellipse
        @return: Quantity
        """
        return self._a

    @a.setter
    def a(self, value):
        self._a = value

    @property
    def b(self):
        """
        The semi-minor axis of the ellipse.
        @return: Quantity
        """
        return self._b

    @b.setter
    def b(self, value):
        self._b = value

    @property
    def pa(self):
        """
        The orientation angle of the ellipse, counter-clock wise from due west.
        @return: Quantity
        """
        return self._pa

    @pa.setter
    def pa(self, value):
        self._pa = value


class SourceReading(object):
    """
    Data for a detected point source (which is a potential moving objects).
    """

    def __init__(self, x, y, x0, y0, ra, dec, xref, yref, obs, ssos=False, from_input_file=False,
                 null_observation=False, discovery=False, dx=0, dy=0, pa=0):
        """
        :param pa:
        :rtype : SourceReading
        :param obs: the Observation object associated with this measurement
        :type obs: Observation
        Args:
          x, y: the coordinates of the source in this reading.
          x0, y0: the coordinates of the source in this reading, but in
            the coordinate frame of the reference image.
          ra: right ascension
          dec: declination
          xref, yref: coordinates of the source in the reference image, in
            the reference image's coordinate frame.
          obs: the observation in which this reading was taken.
          naxis1, naxis2: the size of the FITS image where this detection is from.
        @param is_inverted:
        """
        # print x, y, x0, y0, ra, dec, xref, yref
        self._pix_coord = None
        if x is not None and y is not None:
            self.pix_coord = x, y
        self._ref_coord = None
        if x0 is not None and y0 is not None:
            self.ref_coord = x0, y0
        self._sky_coord = None
        self.sky_coord = ra, dec
        self.xref = xref
        self.yref = yref
        self._uncertainty_ellipse = None
        self._inverted = None
        self.uncertainty_ellipse = dx, dy, pa
        self._obs = None
        self.obs = obs
        self._ssos = None
        self.ssos = ssos
        self._from_input_file = None
        self.from_input_file = from_input_file
        self.null_observation = null_observation
        self._discovery = None
        self.discovery = discovery
        self.mpc_observation = None
        self.mpc_observations = {}
        self.reference_sky_coord = self.sky_coord
        self.min_cutout = 0.3 * units.arcminute

    def _original_frame(self, x, y):
        """
        Return x/y in the original frame, based on a guess as much as anything.
        :param x: x pixel coordinate
        :type x: float
        :param y: y pixel coordinate
        :type y: float
        :return: x,y
        :rtype: float, float
        """
        if self._inverted:
            return self.obs.naxis1 - x, self.obs.naxis2 - y
        return x, y

    @property
    def obs(self):
        """
        :return: The Observation that provide the current source reading.
        :rtype: Observation
        """
        return self._obs

    @obs.setter
    def obs(self, obs):
        """
        :param obs: The observation the provided this source reading.
        :type obs: Observation
        :return:
        """
        assert isinstance(obs, Observation)
        self._obs = obs

    @property
    def x_ref_offset(self):
        return self.x - self.x0

    @property
    def y_ref_offset(self):
        return self.y - self.y0

    @property
    def pix_coord(self):
        """

        :return: The x,y pixel location of the source in the current frame.
        :rtype: (Quantity, Quantity)
        """
        return self._pix_coord

    @pix_coord.setter
    def pix_coord(self, pix_coord):
        """
        :type pix_coord: list
        :param pix_coord: an x,y pixel coordinate, origin = 1
        """
        try:
            pix_coord = list(pix_coord)
        except:
            pass
        if not isinstance(pix_coord, list) or len(pix_coord) != 2:
            raise ValueError("pix_coord needs to be set with an (x,y) coordinate pair, got {}".format(pix_coord))
        x, y = pix_coord
        if not isinstance(x, Quantity):
            x = float(x) * units.pix
        if not isinstance(y, Quantity):
            y = float(y) * units.pix
        self._pix_coord = x, y

    @property
    def x(self):
        """
        :return: the x coordinate value
        :rtype: float
        """
        return self.pix_coord[0].value

    @property
    def y(self):
        """
        :return: the y coordinate value
        :rtype: float
        """
        return self.pix_coord[1].value

    @property
    def ref_coord(self):
        """

        :return: The x,y pixel location of the source in the reference frame.
        :rtype: (Quantity, Quantity)
        """
        return self._ref_coord

    @ref_coord.setter
    def ref_coord(self, pix_coord):
        """
        :type pix_coord: list
        :param pix_coord: an x,y pixel coordinate, origin = 1
        """
        try:
            pix_coord = list(pix_coord)
        except:
            pass
        if not isinstance(pix_coord, list) or len(pix_coord) != 2:
            raise ValueError("pix_coord needs to be set with an (x,y) coordinate pair, got {}".format(pix_coord))
        x, y = pix_coord
        if not isinstance(x, Quantity):
            x = float(x) * units.pix
        if not isinstance(y, Quantity):
            y = float(y) * units.pix
        self._ref_coord = x, y

    @property
    def x0(self):
        return self._ref_coord[0].value

    @property
    def y0(self):
        return self._ref_coord[1].value

    @property
    def sky_coord(self):
        """

        :return: the world coordinate longitude location.
        :rtype: SkyCoord
        """
        return self._sky_coord

    @property
    def ra(self):
        return self.sky_coord.ra.degree

    @property
    def dec(self):
        return self.sky_coord.dec.degree

    @sky_coord.setter
    def sky_coord(self, sky_coord):
        try:
            sky_coord = list(sky_coord)
        except:
            pass
        if isinstance(sky_coord, list):
            ra, dec = sky_coord
            if not isinstance(ra, Quantity):
                ra = float(ra)*units.degree
                dec = float(dec)*units.degree
            sky_coord = SkyCoord(ra, dec, 1)
        if not isinstance(sky_coord, SkyCoord):
            raise ValueError("Failed to initialize coordinate using {}".format(sky_coord))
        self._sky_coord = sky_coord

    @property
    def uncertainty_ellipse(self):
        """

        :return: The semi-major axis, semi-minor axis and position angle of the uncertainty ellipse
        :rtype: Ellipse
        """
        return self._uncertainty_ellipse

    @uncertainty_ellipse.setter
    def uncertainty_ellipse(self, ellipse):
        try:
            ellipse = list(ellipse)
        except:
            pass
        if not isinstance(ellipse, list) or len(ellipse) != 3:
            raise ValueError("Don't know how to set ellipse using: {}".format(ellipse))
        a, b, pa = ellipse
        if not isinstance(a, Quantity):
            a = float(a) * units.arcsecond
        if not isinstance(b, Quantity):
            b = float(b) * units.arcsecond
        if not isinstance(pa, Quantity):
            pa = float(pa) * units.degree
        self._uncertainty_ellipse = Ellipse(a, b, pa)

    @property
    def from_input_file(self):
        return self._from_input_file

    @from_input_file.setter
    def from_input_file(self, from_input_file):
        self._from_input_file = from_input_file

    def __repr__(self):
        return "<SourceReading x=%s, y=%s, x0=%s, y0=%s, ra=%s, dec=%s, obs=%s" % (
            self.x, self.y, self.x0, self.y0, self.ra, self.dec, self.obs)

    @property
    def source_point(self):
        return self.x, self.y

    @property
    def reference_source_point(self):
        """
        The location of the source in the reference image, in terms of the
        current image coordinates.
        """
        xref = isinstance(self.xref, Quantity) and self.xref.value or self.xref
        yref = isinstance(self.yref, Quantity) and self.yref.value or self.yref

        return xref + self.x_ref_offset, yref + self.y_ref_offset

    def get_observation_header(self):
        return self.obs.header

    @staticmethod
    def get_original_image_size():
        raise NotImplemented

    def get_exposure_number(self):
        return self.obs.expnum

    def get_observation(self):
        return self.obs

    def get_coordinate_offset(self, other_reading):
        """
        Calculates the offsets between readings' coordinate systems.

        Args:
          other_reading: ossos.astrom.SourceReading
            The reading to compare coordinate systems with.

        Returns:
          (offset_x, offset_y):
            The x and y offsets between this reading and the other reading's
            coordinate systems.
        """
        my_x, my_y = self.reference_source_point
        other_x, other_y = other_reading.reference_source_point
        return my_x - other_x, my_y - other_y

    def get_image_uri(self):
        return self.obs.get_image_uri()

    def get_apcor_uri(self):
        return self.obs.get_apcor_uri()

    def get_zmag_uri(self):
        return self.obs.get_zmag_uri()

    def get_ccd_num(self):
        """
        Returns:
          ccdnum: int
            The number of the CCD that the image is on.
        """
        return int(self.obs.ccdnum)

    def get_extension(self):
        """
        Returns:
          extension: str
            The FITS file extension.
        """
        if self.obs.is_fake():
            # We get the image from the CCD directory and it is not
            # multi-extension.
            return 0

        # NOTE: ccd number is the extension, BUT Fits file extensions start at 1
        # Therefore ccd n = extension n + 1
        return str(self.get_ccd_num() + 1)

    @property
    def inverted(self):
        if  self._inverted is None:
            self.inverted = self.compute_inverted()
        return self._inverted

    @inverted.setter
    def inverted(self, inverted):
        self._inverted = inverted

    def compute_inverted(self):
        """
        Returns:
          inverted: bool
            True if the stored image is inverted.
        """
        # astheader = storage.get_astheader(self.obs.expnum, self.obs.ccdnum, version=self.obs.ftype)
        # pvwcs = wcs.WCS(astheader)
        # (x, y) = pvwcs.sky2xy(self.ra, self.dec)
        # logger.debug("is_inverted: X,Y {},{}  -> wcs X,Y {},{}".format(self.x, self.y, x, y))
        # dr2 = ((x-self.x)**2 + (y-self.y)**2)
        # return dr2 > 2

        if self.ssos or self.obs.is_fake() or self.obs.ftype == 's':
            inverted = False
        else:
            inverted = True if self.get_ccd_num() - 1 in INVERTED_CCDS else False
        logger.debug("Got that {} is_inverted: {}".format(self.obs.rawname, inverted))
        return inverted


class Observation(object):
    """
    Stores data for a single observation (which may be associated with many
    point sources/readings).  The observation refers the frame taken at the telescope.
    """

    # Aliases for useful header keys
    HEADER_IMG_SIZE_X = NAX1
    HEADER_IMG_SIZE_Y = NAX2

    @staticmethod
    def from_parse_data(rawname, fk, expnum, ftype, ccdnum):
        assert rawname == fk + expnum + ftype + ccdnum
        return Observation(expnum, ftype, ccdnum, fk)

    @staticmethod
    def from_source_reference(expnum, ccd, x, y):
        """
        Given the location of a source in the image, create an Observation.
        """

        image_uri = storage.dbimages_uri(expnum=expnum,
                                         ccd=None,
                                         version='p',
                                         ext='.fits',
                                         subdir=None)
        logger.debug('Trying to access {}'.format(image_uri))

        if not storage.exists(image_uri, force=False):
            logger.warning('Image not in dbimages? Trying subdir.')
            image_uri = storage.dbimages_uri(expnum=expnum,
                                             ccd=ccd,
                                             version='p')

        if not storage.exists(image_uri, force=False):
            logger.warning("Image doesn't exist in ccd subdir. %s" % image_uri)
            return None

        if x == -9999 or y == -9999:
            logger.warning("Skipping {} as x/y not resolved.".format(image_uri))
            return None

        mopheader_uri = storage.dbimages_uri(expnum=expnum,
                                             ccd=ccd,
                                             version='p',
                                             ext='.mopheader')
        if not storage.exists(mopheader_uri, force=False):
            # ELEVATE! we need to know to go off and reprocess/include this image.
            logger.critical('Image exists but processing incomplete. Mopheader missing. {}'.format(image_uri))
            return None

        # Build astrom.Observation
        observation = Observation(expnum=str(expnum),
                                  ftype='p',
                                  ccdnum=str(ccd),
                                  fk="")

        # JJK commented this out, I think the following line is not true?
        # observation.rawname = os.path.splitext(os.path.basename(image_uri))[0]+str(ccd).zfill(2)

        return observation

    def __init__(self, expnum, ftype, ccdnum, fk=None, image_uri=None):
        self.expnum = expnum
        self.fk = fk is not None and fk or ""
        self._header = None
        self.ccdnum = ccdnum is not None and str(ccdnum) or None
        self.ftype = ftype is not None and str(ftype) or None
        self.rawname = "{}{}{}{}".format(self.fk, self.expnum, ftype is not None and ftype or "",
                                         ccdnum is not None and str(ccdnum).zfill(2) or "")
        logger.debug(self.rawname)
        if image_uri is None:
            self.image_uri = self.get_image_uri()

    def __repr__(self):
        return "<Observation rawname=%s>" % self.rawname

    def is_fake(self):
        return self.fk == FAKE_PREFIX or self.ftype == 's'

    # TODO Remove get_image_uri from here, use the storage methods.
    def get_image_uri(self):
        if self.ftype == 'p' and (self.fk is None or self.fk == ''):
            return storage.dbimages_uri(self.expnum)

        return storage.dbimages_uri(self.expnum,
                                    ccd=self.ccdnum,
                                    version=self.ftype,
                                    prefix=self.fk,
                                    ext='.fits')

    def get_object_planted_uri(self):
        dir = os.path.dirname(storage.dbimages_uri(self.expnum, ccd=self.ccdnum))
        return dir+"/Object.planted"

    def get_apcor_uri(self):
        return storage.dbimages_uri(self.expnum,
                                    ccd=self.ccdnum,
                                    version=self.ftype,
                                    prefix=self.fk,
                                    ext=storage.APCOR_EXT)

    def get_zmag_uri(self):
        return storage.dbimages_uri(self.expnum,
                                    ccd=self.ccdnum,
                                    version=self.ftype,
                                    prefix=self.fk,
                                    ext=storage.ZEROPOINT_USED_EXT)

    @property
    def astheader(self):
        return storage.get_astheader(self.expnum, self.ccdnum, self.ftype, self.fk)

    @property
    def naxis1(self):
        return int(self.header[self.HEADER_IMG_SIZE_X])

    @property
    def naxis2(self):
        return int(self.header[self.HEADER_IMG_SIZE_Y])

    @property
    def header(self):
        if self._header is None:
            try:
                self._header = storage.get_mopheader(self.expnum, self.ccdnum, self.ftype, self.fk)
            except Exception as ex:
                import traceback
                traceback.print_exc(file=sys.stdout)
                logger.error(str(ex))
                self._header = self.astheader
        return self._header

    def get_mpc_date(self):
        header = self.header
        if isinstance(header, list):
            extno = self.ccdnum - 1
            print("Reading extension {} looking for header of CCD {}".format(extno, self.ccdnum))
            header = header[extno]
        mpc_date = header.get('MJD_OBS_CENTER', None)
        if mpc_date is None and 'MJD-OBS' in header:
            mjd_obs = float(header.get('MJD-OBS'))
            exptime = float(header.get('EXPTIME'))
            mpc_date = Time(mjd_obs,
                            format='mjd',
                            scale='utc',
                            precision=6)
            mpc_date += TimeDelta(exptime * units.second) / 2.0
            mpc_date = mpc_date.mpc
        return mpc_date


class VettingWriter(BaseAstromWriter):
    """
    Write out the 'vetting' format when looking for slow moving 'stationary' objects
    """

    def _write_headers(self, observations, sys_header):
        pass

    def _write_source(self, source, comment=None, reject=False):

        readings = source.get_readings()
        line = "{} {} {}".format(readings[0].object_id,
                                 readings[0].ra,
                                 readings[0].dec)
        for reading in source.get_readings():
            line += " {}".format(reading.obs.rawname)
        if comment is not None:
            self._write_line("# {}".format(comment))
        if reject:
            line = "! {}".format(line)
        self._write_line(line)


class StreamingVettingWriter(VettingWriter):
    """
    Use if you want to write out sources one-by-one as they are validated.
    See also BulkAstromWriter.

    The Vetting writer doesn't have a header.
    """

    def __init__(self, filehandle, sys_header=None):
        super(StreamingVettingWriter, self).__init__(filehandle)
        self.sys_header = sys_header
        self._header_written = True

    def write_source(self, source, comment=None, reject=False):
        """
        Writes out data for a single source.
        """
        if not self._header_written:
            observations = [reading.get_observation() for reading in source.get_readings()]
            self.write_headers(observations, self.sys_header)

        self._write_source(source, comment=comment, reject=reject)

