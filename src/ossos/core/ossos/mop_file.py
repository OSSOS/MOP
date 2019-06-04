import logging
# noinspection PyUnresolvedReferences
from mp_ephem import time_mpc
from astropy.time import Time
from astropy.table import Table
from ossos import storage


class MOPFile(object):
    """Read in a MOP formatted file"""

    def __init__(self, basename=None, ccd=None, extension=None, type='p', prefix=None, filename=None, subfmt='jmp'):
        """does nothing"""

        self.basename = basename
        self.ccd = ccd
        self.extension = extension
        self.type = type
        self.prefix = prefix
        self._filename = filename
        self.subfmt = subfmt

        self.header = None
        self.data = None
        self._parse()

    @property
    def filename(self):
        """
        Name if the MOP formatted file to parse.
        @rtype: basestring
        @return: filename
        """
        if self._filename is None:
            self._filename = storage.get_file(self.basename,
                                              self.ccd,
                                              ext=self.extension,
                                              version=self.type,
                                              prefix=self.prefix)
        return self._filename

    @property
    def usecols(self):
        """
        The column indexies to read from the file.

        This is only needed for the 'matt' format files as they have a long standing format error.

        @rtype list
        @return: list of column indexies
        """
        if 'matt' in self.filename:
            return list(range(5))
        return None

    def _parse(self):
        """read in a file and return a MOPFile object."""
        with open(self.filename, 'r') as fobj:
            lines = fobj.read().split('\n')

        # Create a header object with content at start of file
        self.header = MOPHeader(self.subfmt).parser(lines)

        # Create a data attribute to hold an astropy.table.Table object
        self.data = MOPDataParser(self.header).parse(lines)


class MOPDataParser(object):
    """ A MOPFile Data object"""

    def __init__(self, header):
        """

        @param header: MOPHeader
        """
        self.header = header
        self._table = None

    @property
    def table(self):
        """
        The astropy.table.Table object that will contain the data result
        @rtype: Table
        @return: data table
        """
        if self._table is None:
            column_names = []
            for fileid in self.header.file_ids:
                for column_name in self.header.column_names:
                    column_names.append("{}_{}".format(column_name, fileid))
                column_names.append("ZP_{}".format(fileid))
            if len(column_names) > 0:
                self._table = Table(names=column_names)
            else:
                self._table = Table()
        return self._table

    def parse(self, lines):

        record = []
        while len(lines) > 0:
            line = lines.pop(0)
            if line.strip().startswith("#"):
                continue
            if len(line.strip()) != 0:
                record.append(line)
                continue
            if len(record) == 0:
                continue
            if len(record) != len(self.header.file_ids):
                logging.debug("record: {}".format(record))
                logging.debug("file_ids: {}".format(self.header.file_ids))
                raise ValueError("Wrong number of entries in record.")
            values = []
            record_number = 0
            for line in record:
                if len(line.split()) != len(self.header.column_names):
                    logging.debug("line: {}".format(line))
                    logging.debug("Wrong number of columns compared to: {}".format(self.header.column_names))
                    raise ValueError("column length mismatch")
                values.extend([ float(x) for x in line.split() ])
                try:
                    zp = float(open("{}.zeropoint.used".format(self.header.file_ids[record_number]), 'r').readline())
                except:
                    zp = 0
                values.append(zp)
                record_number += 1
            self.table.add_row(values)
            record = []
        return self.table


class MOPHeader(object):
    """A MOPFile Header object"""

    def __init__(self, subfmt):

        self.keywords = {}
        self.subfmt = subfmt
        self.file_ids = []
        self.column_names = []
        self._file_id_idx = 0

    def __str__(self):
        return str(self.keywords)+'\n'+str(self.column_names)

    def parser(self, lines):
        """Given a set of lines parse the into a MOP Header"""
        while len(lines) > 0:
            if lines[0].startswith('##') and lines[1].startswith('# '):
                # A two-line keyword/value line starts here.
                self._header_append(lines.pop(0), lines.pop(0))
            elif lines[0].startswith('# '):
                # Lines with single comments are exposure numbers unless preceeded by double comment line
                self._append_file_id(lines.pop(0))
            elif lines[0].startswith('##'):
                # Double comment lines without a single comment following are column headers for dataset.
                self._set_column_names(lines.pop(0)[2:])
            else:
                # Last line of the header reached.
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
        val.insert(idx, Time("{} {} {}".format(val.pop(idx), val.pop(idx), val.pop(idx)),
                             format='mpc',
                             scale='utc').mjd)
        logging.debug("Computed MJD: {}".format(val[idx]))

    def _header_append(self, kw_line, val_line):
        
        keywords = kw_line.strip().split()
        values = val_line.strip().split()
        self._compute_mjd(keywords, values)
        self._append_keywords(keywords, values)
        return

    def _append_keywords(self, keywords, values):

        if len(keywords) != len(values):
            raise ValueError("keyword/value lengths don't match: {}/{}".format(keywords, values))
        file_id = self.file_ids[self._file_id_idx]
        while len(keywords) > 0:
            keyword = keywords.pop()
            value = values.pop()
            if keyword not in self.keywords :
                self.keywords[keyword] = []
            self.keywords[keyword].append(value)
        return

    def _append_file_id(self, line):
        self.file_ids.append(line.split()[1].strip())

    def _set_column_names(self, line):
        if self.subfmt=='matt':
            self.column_names = line[1:].split()[0:5]
        else:
            self.column_names = line[1:].split()
