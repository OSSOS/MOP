"""
Parsers for reading input specification files.
"""

import re


class AstromParser(object):
    """
    Parses a .astrom file (our own format) which specifies exposure numbers,
    identified point sources, their x, y location, and much more.
    """

    def __init__(self):
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
            "(?P<CRVAL1>\d+\.\d+)\s+"
            "(?P<CRVAL2>\d+\.\d+)\s+"
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

            for source_ob in source_obs:
                fields = source_ob.split()
                source.append(SourceReading(*fields))

            sources.append(source)

        return sources

    def parse(self, filename):
        with open(filename, "rb") as filehandle:
            filestr = filehandle.read()

        assert filestr is not None, "File contents are None"

        observations = self._parse_observation_list(filestr)

        self._parse_observation_headers(filestr, observations)

        sys_header = self._parse_system_header(filestr)

        sources = self._parse_source_data(filestr, observations)

        return AstromData(observations, sys_header, sources)


class AstromData(object):
    """
    Encapsulates data extracted from an .astrom file.
    """

    def __init__(self, observations, sys_header, sources):
        self.observations = observations
        self.sys_header = sys_header
        self.sources = sources


class SourceReading(object):
    """
    Data for a detected point source.
    """

    def __init__(self, x, y, x0, y0, ra, dec):
        self.x = x
        self.y = y
        self.x0 = x0
        self.y0 = y0
        self.ra = ra
        self.dec = dec


class Observation(object):
    """
    Stores data for a single observation.
    """

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
