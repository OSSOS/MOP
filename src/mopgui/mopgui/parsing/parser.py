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

    def parse(self, filename):
        with open(filename, "rb") as filehandle:
            filestr = filehandle.read()

        assert filestr is not None

        matches = self.obs_regex.findall(filestr) # returns list of tuples

        observations = [Observation(*match) for match in matches]

        return AstromData(observations)


class AstromData(object):
    """
    Encapsulates data extracted from an .astrom file.
    """

    def __init__(self, observations):
        self.observations = observations


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

