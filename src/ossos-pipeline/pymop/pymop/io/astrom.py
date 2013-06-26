"""
Reads and writes .astrom files.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import os
import re

from pymop import tasks


HEADER_LINE_LENGTH = 80

## Observation header keys
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

        self.obs_list_regex = re.compile(
            "#\s+(?P<rawname>(?P<expnum>\d{7})(?P<ftype>[ops])(?P<ccdnum>\d+))"
        )

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

    def _parse_observation_list(self, filestr):
        matches = self.obs_list_regex.findall(filestr) # returns list of tuples
        return [Observation.from_parse_data(*match) for match in matches]

    def _parse_observation_headers(self, filestr, observations):
        obsnum = 0
        for match in self.obs_header_regex.finditer(filestr):
            obs = observations[obsnum]
            for header_key, header_val in match.groupdict().iteritems():
                obs.header[header_key] = header_val
            obsnum += 1

        assert obsnum == len(observations), ("Number of observations headers "
                                             "parsed doesn't match length of "
                                             "observation list")

    def _parse_system_header(self, filestr):
        sys_header_match = self.sys_header_regex.search(filestr)

        assert sys_header_match is not None, "Could not parse system header"

        return sys_header_match.groupdict()

    def _parse_source_data(self, filestr, observations):
        source_list_match = self.source_list_reg.search(filestr)

        assert source_list_match is not None, "Could not find the source list"

        raw_source_list = (source_list_match.group(1)).split("\n\n")

        sources = []
        for raw_source in raw_source_list:
            source = []

            source_obs = raw_source.strip().split("\n")
            assert len(source_obs) == len(
                observations), "Source doesn't have same number of observations (%d) as in observations list (%d)." % (
                len(source_obs), len(observations))

            x_ref = None
            y_ref = None
            for i, source_ob in enumerate(source_obs):
                fields = source_ob.split()

                if i == 0:
                    x_ref = fields[0]
                    y_ref = fields[1]

                fields.append(x_ref)
                fields.append(y_ref)

                # Find the observation corresponding to this reading
                fields.append(observations[i])

                source.append(SourceReading(*fields))

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
        with open(filename, "rb") as filehandle:
            filestr = filehandle.read()

        assert filestr is not None, "File contents are None"

        observations = self._parse_observation_list(filestr)

        self._parse_observation_headers(filestr, observations)

        sys_header = self._parse_system_header(filestr)

        sources = self._parse_source_data(filestr, observations)

        return AstromData(observations, sys_header, sources)


class BaseAstromWriter(object):
    """
    Provides base functionality for AstromWriters.  Use the subclass for your
    use case.
    """

    def __init__(self, filehandle):
        self.output_file = filehandle

        self._header_written = False

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

        for reading in source:
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


class StreamingAstromWriter(BaseAstromWriter):
    """
    Use if you want to write out sources one-by-one as they are validated.
    See also BulkAstromWriter.
    """

    def __init__(self, filehandle, sys_header):
        super(StreamingAstromWriter, self).__init__(filehandle)
        self.sys_header = sys_header

    def write_source(self, source):
        """
        Writes out data for a single source.
        """
        if not self._header_written:
            observations = [reading.get_observation() for reading in source]
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


class AstromWorkload(object):
    def __init__(self, working_directory, progress_manager, task):
        working_dir_files = tasks.listdir_for_task(working_directory, task)

        workload_filenames = []
        workload_fullpaths = []
        for filename in working_dir_files:
            full_path = os.path.join(working_directory, filename)

            if (filename not in progress_manager.get_done(task) and
                        os.stat(full_path).st_size > 0):
                workload_filenames.append(filename)
                workload_fullpaths.append(full_path)

        if len(workload_filenames) == 0:
            raise ValueError("Empty workload!")

        self.working_directory = working_directory
        self.progress_manager = progress_manager
        self.task = task
        self.workload_filenames = workload_filenames
        self.full_paths = workload_fullpaths

        parser = AstromParser()

        self.astrom_data_list = [parser.parse(filename) for filename in self.full_paths]

        self.current_astrom_data_index = 0
        self._lock_current_file()

    def __iter__(self):
        return iter(zip(self.workload_filenames, self.astrom_data_list))

    def next_file(self):
        self._unlock_current_file()
        self.current_astrom_data_index = (self.current_astrom_data_index + 1) % self.get_load_length()
        self._lock_current_file()

    def previous_file(self):
        self._unlock_current_file()
        self.current_astrom_data_index = (self.current_astrom_data_index - 1) % self.get_load_length()
        self._lock_current_file()

    def get_working_directory(self):
        return self.working_directory

    def get_current_astrom_data(self):
        return self.get_astrom_data(self.current_astrom_data_index)

    def get_current_filename(self):
        return self.get_filename(self.current_astrom_data_index)

    def get_astrom_data(self, index):
        return self.astrom_data_list[index]

    def get_filename(self, index):
        return self.workload_filenames[index]

    def get_load_length(self):
        return len(self.astrom_data_list)

    def get_sources(self):
        all_sources = []
        for astrom_data in self.astrom_data_list:
            all_sources.extend(astrom_data.sources)

        return all_sources

    def get_source_groups(self):
        source_groups = []
        for astrom_data in self.astrom_data_list:
            source_groups.append(astrom_data.get_sources())
        return source_groups

    def get_source_count(self):
        return len(self.get_sources())

    def get_reading_count(self):
        count = 0
        for astrom_data in self.astrom_data_list:
            count += astrom_data.get_reading_count()

        return count

    def record_current_file_done(self):
        self.progress_manager.record_done(self.get_current_filename())

    def get_current_processed_indices(self):
        return self.progress_manager.get_processed_indices(
            self.get_current_filename())

    def record_index(self, index):
        self.progress_manager.record_index(self.get_current_filename(), index)

    def _lock_current_file(self):
        self.progress_manager.lock(self.get_current_filename())

    def _unlock_current_file(self):
        self.progress_manager.unlock(self.get_current_filename())


class AstromData(object):
    """
    Encapsulates data extracted from an .astrom file.
    """

    def __init__(self, observations, sys_header, sources):
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
        self.observations = observations
        self.sys_header = sys_header
        self.sources = [Source(reading_list) for reading_list in sources]

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

    def __init__(self, readings):
        self.readings = readings

    def __iter__(self):
        return iter(self.readings)

    def get_reading(self, index):
        return self.readings[index]

    def get_readings(self):
        return self.readings

    def num_readings(self):
        return len(self.readings)


class SourceReading(object):
    """
    Data for a detected point source (which is a potential moving objects).
    """

    def __init__(self, x, y, x0, y0, ra, dec, xref, yref, obs):
        """
        Args:
          x, y: the coordinates of the source in this reading.
          x0, y0: the coordinates of the source in this reading, but in
            the coordinate frame of the reference image.
          ra: right ascension
          dec: declination
          xref, yref: coordinates of the source in the reference image, in
            the reference image's coordinate frame.
          obs: the observation in which this reading was taken.
        """
        self.x = float(x)
        self.y = float(y)
        self.x0 = float(x0)
        self.y0 = float(y0)
        self.ra = float(ra)
        self.dec = float(dec)
        self.xref = float(xref)
        self.yref = float(yref)

        self.x_ref_offset = self.x - self.x0
        self.y_ref_offset = self.y - self.y0

        self.obs = obs

        self._fitsimage = None

    def get_fits_image(self):
        return self._fitsimage

    def set_fits_image(self, fitsimage):
        self._fitsimage = fitsimage

    @property
    def source_point(self):
        return self.x, self.y

    @property
    def reference_source_point(self):
        """
        The location of the source in the reference image, in terms of the
        current image coordinates.
        """
        return self.xref + self.x_ref_offset, self.yref + self.y_ref_offset

    @property
    def image_source_point(self):
        """
        The location of the source point in the image, taking into account
        that the image may be a cutout.
        """
        assert self.get_fits_image() is not None, "No FITS image loaded"
        return self.get_fits_image().get_pixel_coordinates(self.source_point)

    def get_original_image_size(self):
        header = self.obs.header
        return (int(header[Observation.HEADER_IMG_SIZE_X]),
                int(header[Observation.HEADER_IMG_SIZE_Y]))

    def get_exposure_number(self):
        return self.obs.expnum

    def get_observation(self):
        return self.obs

    def __repr__(self):
        return "<SourceReading x=%s, y=%s, x0=%s, y0=%s, ra=%s, dec=%s, obs=%s" % (
            self.x, self.y, self.x0, self.y0, self.ra, self.dec, self.obs)


class Observation(object):
    """
    Stores data for a single observation (which may be associated with many
    point sources/readings).
    """

    # Aliases for useful header keys
    HEADER_IMG_SIZE_X = NAX1
    HEADER_IMG_SIZE_Y = NAX2

    @staticmethod
    def from_parse_data(rawname, expnum, ftype, ccdnum):
        assert rawname == expnum + ftype + ccdnum
        return Observation(expnum, ftype, ccdnum)

    def __init__(self, expnum, ftype, ccdnum):
        self.expnum = expnum
        self.ftype = ftype
        self.ccdnum = ccdnum

        self.rawname = expnum + ftype + ccdnum

        self.header = {}

    def __repr__(self):
        return "<Observation rawname=%s>" % self.rawname
