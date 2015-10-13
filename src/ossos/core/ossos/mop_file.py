
from vos import vos
import math
import urlparse
import re
import sys
import logging
from ossos import util


VOS_SCHEME = 'vos'
FILE_SCHEME = 'file'
MJD_OFFSET = 2400000.5

class Parser(object):
    """Read in a MOP formatted file"""

    def __init__(self):
        """does nothing"""

        self.fobj = None
        self.keywords  = []
        self.values = []
        self.formats = {}
        self.header = {}
        self.cdata ={}
        
    def open(self, filename):
        """Open the file for reading"""

        # is this a vospace file reference?
        parts = urlparse.urlparse(filename)
        if parts.scheme == VOS_SCHEME:
            self.fobj = vos.Client().open(filename)
        elif parts.scheme == FILE_SCHEME or parts.scheme == '':
            self.fobj = open(filename)
        else:
            raise IOError("unknown scheme: %s" %(parts.scheme))

        return
    
    def parse(self, filename):
        """read in a file and return a MOPFile object."""
        self.open(filename)
        lines = self.fobj.read().split('\n')
        self.header = HeaderParser().parser(lines)
        return self

        
class HeaderParser(object):
    """A MOPFile Header object"""

    def __init__(self):

        self.keywords = {}
        self.file_ids = []
        self.column_names = []

    def __str__(self):
        return str(self.keywords)+'\n'+str(self.column_names)

    def parser(self, lines):
        """Given a set of lines parse the into a MOP Header"""
        while len(lines) > 0:
            if lines[0].startswith('##') and lines[1].startswith('# '):
                ## A keyword line/value pair starts here
                self._header_append(lines.pop(0),lines.pop(0))
            elif lines[0].startswith('# '):
                ## Filenames start here
                self._append_file_id(lines.pop(0))
            elif lines[0].startswith('##'):
                self._set_column_names(lines.pop(0))
            else:
                return self
        raise IOError("Failed trying to read header")

    def _compute_mjd(self, kw, val):
        """
        Sometimes that MJD-OBS-CENTER keyword maps to a three component string, instead of a single value.
        @param kw: a list of keywords
        @param val: a list of matching values
        @return: the MJD-OBS-CENTER as a julian date.
        """
        try:
            idx = kw.index('MJD-OBS-CENTER')
        except ValueError:
            return
        if len(val) == len(kw):
            return
        if len(val) - 2 != len(kw):
            raise ValueError("convert: keyword/value lengths don't match: {}/{}".format(kw, val))
        val.insert(idx, util.Time("{} {} {}".format(val.pop(idx), val.pop(idx), val.pop(idx)),
                             format='mpc',
                             scale='utc').mjd)
        logging.debug("Computed MJD: {}".format(val[idx]))

    def _header_append(self, kw_line, val_line):
        
        keywords = kw_line.strip().split()
        values = val_line.strip().split()
        self._compute_mjd(keywords, values)
        self._append_keywords(keywords, values)
        return

    def _depreicated_compute_mjd(self, year, month, day):
        """Look through the dictionary and create a new keyword list
        by replacing mjd-obs-center keyword value with actual mjd"""
        mjd = util.Time("{} {} {}".format(year, month, day), format='mpc', scale='utc').mjd
        return mjd

    def _append_keywords(self, keywords, values):

        if len(keywords) != len(values):
            raise ValueError("keyword/value lengths don't match: {}/{}".format(keywords, values))
        while len(keywords) > 0:
            keyword = keywords.pop()
            value = values.pop()
            if not self.keywords.has_key(keyword) :
                self.keywords[keyword] = []
            self.keywords[keyword].append(value)
        return

    def _append_file_id(self, line):
        self.file_ids.append(line.split()[1].strip())


    def _set_column_names(self, line):
        self.column_names = line[1:].split()
