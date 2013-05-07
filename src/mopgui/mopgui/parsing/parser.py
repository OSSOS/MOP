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
        self.obs_regex = re.compile("#\s+(?P<rawname>(?P<expnum>\d{7})"
                                    "(?P<ftype>[ops])(?P<ccdnum>\d+))")
        self.obs_header_regex = re.compile(
            """##\s+MOPversion\s+#\s+(?P<MOPversion>\d+\.[\d\w]+)\s+##\s+MJD-OBS-CENTER\s+EXPTIME\s+THRES\s+FWHM\s+MAXCOUNT\s+CRVAL1\s+CRVAL2\s+EXPNUM\s+#\s+(?P<MJD_OBS_CENTER>\d{4} \d{2} \d+\.\d+)\s+(?P<EXPTIME>\d+\.\d+)\s+(?P<THRES>\d+\.\d+)\s+(?P<FWHM>\d+\.\d+)\s+(?P<MAXCOUNT>\d+\.\d+)\s+(?P<CRVAL1>\d+\.\d+)\s+(?P<CRVAL2>\d+\.\d+)\s+(?P<EXPNUM>\d+)\s+##\s+SCALE\s+CHIP\s+CRPIX1\s+CRPIX2\s+NAX1\s+NAX2\s+DETECTOR\s+PHADU\s+RDNOIS\s+#\s+(?P<SCALE>\d+\.\d+)\s+(?P<CHIP>\d+)\s+(?P<CRPIX1>-?\d+\.\d+)\s+(?P<CRPIX2>-?\d+\.\d+)\s+(?P<NAX1>\d+)\s+(?P<NAX2>\d+)\s+(?P<DETECTOR>\w+)\s+(?P<PHADU>\d+\.\d+)\s+(?P<RDNOIS>\d+\.\d+)""")
        self.sys_header_regex = re.compile("""##\s+RMIN\s+RMAX\s+ANGLE\s+AWIDTH\s+#\s+(?P<RMIN>\d+\.\d+)\s+(?P<RMAX>\d+\.\d+)\s+(?P<ANGLE>-?\d+\.\d+)\s+(?P<AWIDTH>\d+\.\d+)""")

    def parse(self, filename):
        with open(filename, "rb") as filehandle:
            filestr = filehandle.read()

        assert filestr is not None

        # Parse observation list
        matches = self.obs_regex.findall(filestr) # returns list of tuples
        observations = [Observation(*match) for match in matches]

        # Parse observation headers
        obsnum = 0
        for match in self.obs_header_regex.finditer(filestr):
            obs = observations[obsnum]
            for header_key, header_val in match.groupdict().iteritems():
                obs.header[header_key] = header_val
            obsnum += 1

        assert obsnum == len(observations), ("Number of observations headers "
                                             "parsed doesn't match length of "
                                             "observation list")

        # Parse system header
        sys_header_match = self.sys_header_regex.search(filestr)
        assert sys_header_match is not None, "Could not parse system header"
        sys_header = sys_header_match.groupdict()

        return AstromData(observations, sys_header)


class AstromData(object):
    """
    Encapsulates data extracted from an .astrom file.
    """

    def __init__(self, observations, sys_header):
        self.observations = observations
        self.sys_header = sys_header


class Observation(object):
    """
    Stores data for a single observation.
    """

    def __init__(self, rawname, expnum, ftype, ccdnum):
        assert rawname == expnum + ftype + ccdnum

        self.rawname = rawname
        self.expnum = expnum
        self.ftype = ftype
        self.ccdnum = ccdnum

        self.header = {}
