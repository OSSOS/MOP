
import logging
import util
from astropy.table import Table, Column
import numpy
import wcs
import storage



VOS_SCHEME = 'vos'
FILE_SCHEME = 'file'
MJD_OFFSET = 2400000.5

class Parser(object):
    """Read in a MOP formatted file"""

    def __init__(self, expnum, ccd, extension, type='p', prefix=None):
        """does nothing"""

        self.expnum = expnum
        self.ccd = ccd
        self.extension = extension
        self.type = type
        self.prefix = prefix
        self.fobj = None
        self.keywords  = []
        self.values = []
        self.formats = {}
        self.header = {}
        self.cdata ={}
        

    def parse(self):
        """read in a file and return a MOPFile object."""
        self.filename =  storage.get_file(self.expnum,
                                       self.ccd,
                                       ext=self.extension,
                                       version=self.type,
                                       prefix=self.prefix)
        self.fobj = open(self.filename,'r')
        lines = self.fobj.read().split('\n')
        self.header = HeaderParser(self.extension).parser(lines)
        if 'matt' in self.extension:
            usecols=[0,1,2,3,4]
            data = numpy.genfromtxt(self.filename, usecols=usecols)
        else:
            data = numpy.genfromtxt(self.filename)
        self.data = Table(data, names=self.header.column_names[0:data.shape[1]])
        ast_header = storage.get_astheader(self.expnum, self.ccd)
        self.wcs = wcs.WCS(ast_header)
        flip_these_extensions = range(1,19)
        flip_these_extensions.append(37)
        flip_these_extensions.append(38)
        if self.ccd + 1 in flip_these_extensions:
            self.data['X'] = float(self.header.keywords['NAX1'][0])-self.data['X'] + 1
            self.data['Y'] = float(self.header.keywords['NAX2'][0])-self.data['Y'] + 1
        ra, dec = self.wcs.xy2sky(self.data['X'], self.data['Y'], usepv=True)

        self.data.add_columns([Column(ra, name='RA_J2000'), Column(dec, name='DE_J2000')])
        return self



        
class HeaderParser(object):
    """A MOPFile Header object"""

    def __init__(self, extension):

        self.keywords = {}
        self.extension = extension
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
                self._set_column_names(lines.pop(0)[2:], ext= self.extension)
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


    def _set_column_names(self, line, ext='obj.jmp'):
        if 'matt' in ext:
            self.column_names = line[1:].split()[0:5]
        else:
            self.column_names = line[1:].split()
        print ext, len(self.column_names), line
