__author__ = "David Rusk <drusk@uvic.ca>"

import itertools
import re
import struct
import time
from datetime import datetime
import numpy

from astropy import coordinates
from astropy import units
from astropy.time import sofa_time
from astropy.time import TimeString
from astropy.time import Time


MPCNOTES = {'Note1':
                {" ": " ",
                 "": " ",
                 "*": "*",
                 "A": "earlier approximate position inferior",
                 "a": "sense of motion ambiguous",
                 "B": "bright sky/black or dark plate",
                 "b": "bad seeing",
                 "c": "crowded star field",
                 "D": "declination uncertain",
                 "d": "diffuse image",
                 "E": "at or near edge of plate",
                 "F": "faint image",
                 "f": "involved with emulsion or plate flaw",
                 "G": "poor guiding",
                 "g": "no guiding",
                 "H": "hand measurement of CCD image",
                 "I": "involved with star",
                 "i": "inkdot measured",
                 "J": "J2000.0 rereduction of previously-reported position",
                 "K": "stacked image",
                 "k": "stare-mode observation by scanning system",
                 "M": "measurement difficult",
                 "m": "image tracked on object motion",
                 "N": "near edge of plate, measurement uncertain",
                 "O": "image out of focus",
                 "o": "plate measured in one direction only",
                 "P": "position uncertain",
                 "p": "poor image",
                 "R": "right ascension uncertain",
                 "r": "poor distribution of reference stars",
                 "S": "poor sky",
                 "s": "streaked image",
                 "T": "time uncertain",
                 "t": "trailed image",
                 "U": "uncertain image",
                 "u": "unconfirmed image",
                 "V": "very faint image",
                 "W": "weak image",
                 "w": "weak solution"
                 },
            'Note2': {
                " ": " ",
                "": " ",
                "P": "Photographic",
                "e": "Encoder",
                "C": "CCD",
                "T": "Meridian or transit circle",
                "M": "Micrometer",
                "V": "'Roving Observer' observation",
                "R": "Radar observation",
                "S": "Satellite observation",
                "c": "Corrected-without-republication CCD observation",
                "E": "Occultation-derived observations",
                "O": "Offset observations (used only for observations of natural satellites)",
                "H": "Hipparcos geocentric observations",
                "N": "Normal place",
                "n": "Mini-normal place derived from averaging observations from video frames"
            }}


class MPCFormatError(Exception):
    """Base class for errors in MPC formatting."""


class MPCFieldFormatError(MPCFormatError):
    def __init__(self, field, requirement, actual):
        super(MPCFieldFormatError, self).__init__(
            "Field %s: %s; but was %s" % (field, requirement, actual))


def format_ra_dec(ra_deg, dec_deg):
    """
    Converts RA and DEC values from degrees into the formatting required
    by the Minor Planet Center:

    Formats:
      RA: 'HH MM SS.ddd'
      DEC: 'sDD MM SS.dd' (with 's' being the sign)

    (From: http://www.minorplanetcenter.net/iau/info/OpticalObs.html)

    Args:
      ra_deg: float
        Right ascension in degrees
      dec_deg: float
        Declination in degrees

    Returns:
      formatted_ra: str
      formatted_dec: str
    """
    coords = coordinates.ICRSCoordinates(ra=ra_deg, dec=dec_deg,
                                         unit=(units.degree, units.degree))

    # decimal=False results in using sexagesimal form
    formatted_ra = coords.ra.format(unit=units.hour, decimal=False,
                                    sep=" ", precision=3, alwayssign=False,
                                    pad=True)

    formatted_dec = coords.dec.format(unit=units.degree, decimal=False,
                                      sep=" ", precision=2, alwayssign=True,
                                      pad=True)

    return formatted_ra, formatted_dec


def _is_numeric(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


class MPCNote(object):
    """
    Alphabetic note shown with some of the observations. Non-alphabetic codes are used to differentiate between
    different programs at the same site and such codes will be defined in the headings for the individual
    observatories in the Minor Planet Circulars.
    """

    def __init__(self, code="C", note_type="Note2"):
        self.note_type = note_type
        self.code = code

    @property
    def note_type(self):
        """
        Note 1 or 2 from an MPC line.
        """
        return self._note_type

    @note_type.setter
    def note_type(self, note_type):
        if note_type not in MPCNOTES.keys():
            raise ValueError("Invalid note_type: expected one of %s got %s" % ( str(MPCNOTES.keys()), note_type))
        self._note_type = note_type

    @property
    def code(self):
        """
        The MPC note the denotes the type of detector system used
        """
        return self._code

    @code.setter
    def code(self, code):
        """

        :type code: str
        :param code: an MPC Note code. Either from the allow dictionary or 0-9
        """
        if code is None:
            _code = " "
        else:
            _code = str(code).strip()

        if _code.isdigit():
            if self.note_type != 'Note1':
                raise MPCFieldFormatError(self.note_type,
                                          "must be one of " + str(MPCNOTES[self.note_type]),
                                          _code)
            if _code not in range(10):
                raise MPCFieldFormatError(self.note_type,
                                          "integer values must be between 0 and 9",
                                          _code)
        else:
            if len(_code) > 1:
                raise MPCFieldFormatError(self.note_type,
                                          "must be 0 or 1 characters",
                                          _code)
            if _code not in MPCNOTES[self.note_type]:
                raise MPCFieldFormatError(self.note_type,
                                          "must one of " + str(MPCNOTES[self.note_type]),
                                          _code)
        self._code = _code

    def __str__(self):
        return str(self.code)

    @property
    def long(self):
        return MPCNOTES[self.note_type][self.code]


class Discovery(object):
    """
    Holds the discovery flag for an MPC Observation Line
    """

    def __init__(self, is_discovery=False):
        self.is_discovery = is_discovery

    def set_from_mpc_line(self, mpc_line):
        """
        Given an MPC line set the discovery object
        """
        mpc_line = str(mpc_line)
        if len(mpc_line) < 56:
            raise MPCFieldFormatError("mpc_line",
                                      "is too short",
                                      mpc_line)
        self.is_discovery = mpc_line[12]

    @property
    def is_discovery(self):
        return self._is_discovery

    @is_discovery.setter
    def is_discovery(self, is_discovery):
        if is_discovery not in ['*', ' ', '', True, False, None]:
            raise MPCFieldFormatError("discovery",
                                      "must be one of '',' ','*',True, False. Was: ",
                                      is_discovery)
        self._is_discovery = (( (is_discovery or False) == '*' or is_discovery == True ) and True ) or False

    def __str__(self):
        return (self.is_discovery and "*") or " "


class TimeMPC(TimeString):
    """
    Override the TimeString class to convert from MPC format string to astropy.time.Time object.

    usage:

    from astropy.time.core import Time
    Time.FORMATS[TimeMPC.name] = TimeMPC

    t = Time('2000 01 01.00001', format='mpc', scale='utc')

    str(t) == '2000 01 01.000001'
    """

    name = 'mpc'
    subfmts = (('mpc', '%Y %m %d', "{year:4d} {mon:02d} {day:02d}.{fracday:s}"),)

    ### need our own 'set_jds' function as the MPC Time string is not typical
    def set_jds(self, val1, val2):
        """

        Parse the time strings contained in val1 and set jd1, jd2

        :param val1: array of strings to parse into JD format
        :param val2: not used for string conversions but passed regardless
        """
        n_times = len(val1)  # val1,2 already checked to have same len
        iy = numpy.empty(n_times, dtype=numpy.intc)
        im = numpy.empty(n_times, dtype=numpy.intc)
        iday = numpy.empty(n_times, dtype=numpy.intc)
        ihr = numpy.empty(n_times, dtype=numpy.intc)
        imin = numpy.empty(n_times, dtype=numpy.intc)
        dsec = numpy.empty(n_times, dtype=numpy.double)

        # Select subformats based on current self.in_subfmt
        subfmts = self._select_subfmts(self.in_subfmt)

        for i, timestr in enumerate(val1):
            # Assume that anything following "." on the right side is a
            # floating fraction of a day.
            try:
                idot = timestr.rindex('.')
            except:
                fracday = 0.0
            else:
                timestr, fracday = timestr[:idot], timestr[idot:]
                fracday = float(fracday)

            for _, strptime_fmt, _ in subfmts:
                try:
                    tm = time.strptime(timestr, strptime_fmt)
                except ValueError:
                    pass
                else:
                    iy[i] = tm.tm_year
                    im[i] = tm.tm_mon
                    iday[i] = tm.tm_mday
                    ihr[i] = tm.tm_hour + int(24 * fracday)
                    imin[i] = tm.tm_min + int(60 * (24 * fracday - ihr[i]))
                    dsec[i] = tm.tm_sec + 60 * (60 * (24 * fracday - ihr[i]) - imin[i])
                    break
            else:
                raise ValueError('Time {0} does not match {1} format'
                .format(timestr, self.name))

        self.jd1, self.jd2 = sofa_time.dtf_jd(self.scale.upper().encode('utf8'),
                                              iy, im, iday, ihr, imin, dsec)
        return

    def str_kwargs(self):
        """                                                                                                                                           
        Generator that yields a dict of values corresponding to the                                                                                   
        calendar date and time for the internal JD values.

        Here we provide the additional 'fracday' element needed by 'mpc' format
        """
        iys, ims, ids, ihmsfs = sofa_time.jd_dtf(self.scale.upper()
                                                 .encode('utf8'),
                                                 6,
                                                 self.jd1, self.jd2)

        # Get the str_fmt element of the first allowed output subformat                                                                               
        _, _, str_fmt = self._select_subfmts(self.out_subfmt)[0]

        yday = None
        has_yday = '{yday:' in str_fmt or False

        for iy, im, iday, ihmsf in itertools.izip(iys, ims, ids, ihmsfs):
            ihr, imin, isec, ifracsec = ihmsf
            if has_yday:
                yday = datetime(iy, im, iday).timetuple().tm_yday

            # MPC uses day fraction as time part of datetime
            fracday = (((((ifracsec / 1000000.0 + isec) / 60.0 + imin) / 60.0) + ihr) / 24.0) * (10 ** 6)
            fracday = '{0:06g}'.format(fracday)[0:self.precision]
            #format_str = '{{0:g}}'.format(self.precision)
            #fracday = int(format_str.format(fracday))
            yield dict(year=int(iy), mon=int(im), day=int(iday), hour=int(ihr), min=int(imin), sec=int(isec),
                       fracsec=int(ifracsec), yday=yday, fracday=fracday)


Time.FORMATS[TimeMPC.name] = TimeMPC


class Observation(object):
    """
    An observation of an object, nominally generated by reading an MPC formatted file.
    """

    def __init__(self,
                 minor_planet_number=None,
                 provisional_name=None,
                 discovery=False,
                 note1=None,
                 note2=None,
                 date="2000 01 01.000001",
                 ra="00 00 00.000",
                 dec="+00 00 00.00",
                 mag=0.00,
                 band='r',
                 observatory_code=568,
                 comment=""):

        self.minor_planet_number = minor_planet_number
        self.provisional_name = provisional_name
        self.discovery = discovery
        self.note1 = note1
        self.note2 = note2
        self.date = date
        self.coordinate = (ra, dec)
        self.mag = mag
        self.band = band
        self.observatory_code = observatory_code
        self.comment = comment

    @classmethod
    def from_string(cls, mpc_line):
        """
        Given an MPC formatted line, returns an MPC Observation object.
        :param mpc_line: a line in the one-line roving observer format
        """
        mpc_format = '5s7s1s1s1s17s12s12s9x5s1s6x3s'
        mpc_line = mpc_line.strip('\n')
        comment = mpc_line[80:]
        mpc_line = mpc_line[0:80]
        return cls(*struct.unpack(mpc_format, mpc_line), comment=comment)

    def to_string(self):
        as_string = str(self)
        if self.comment != "":
            as_string += " " + self.comment
        return as_string

    def __str__(self):
        """
        Writes out data about accepted objects in the Minor Planet Center's 'Minor Planets'
        format as specified here:
        http://www.minorplanetcenter.net/iau/info/OpticalObs.html
        """
        if self.null_observation is True:
            mpc_str = "!" + 4*" "
        else:
            mpc_str = (self.minor_planet_number is None and 5 * " ") or "%5s" % (self.minor_planet_number)
        mpc_str += (self.provisional_name is None and 7 * " ") or "%7s" % (self.provisional_name)
        mpc_str += str(self.discovery)
        mpc_str += '{0:1s}{1:1s}'.format(str(self.note1), str(self.note2))
        mpc_str += '{0:<17s}'.format(str(self.date))
        mpc_str += '{0:<12s}{1:<12s}'.format(str(self.ra), str(self.dec))
        mpc_str += 9 * " "
        mag_format = '{0:<5.' + str(self._mag_precision) + 'f}{1:1s}'
        mag_str = ( self.mag is None and 6 * " ") or mag_format.format(self.mag, self.band)
        if len(mag_str) != 6:
            raise MPCFieldFormatError("mag",
                                      "length of mag string should be exactly 6 characters, got->",
                                      mag_str)
        mpc_str += mag_str
        mpc_str += 6 * " "
        mpc_str += "%3s" % self.observatory_code
        return mpc_str

    @property
    def minor_planet_number(self):
        return self._minor_planet_number

    @minor_planet_number.setter
    def minor_planet_number(self, minor_planet_number=None):
        """

        :type minor_planet_number: int
        :param minor_planet_number: the number of the minor planet
        """
        if not len(str(minor_planet_number)) < 6:
            raise MPCFieldFormatError("Minor Planet Number",
                                      "must be 5 or less characters",
                                      minor_planet_number)
        self._minor_planet_number = minor_planet_number
        self.null_observation = False
        if len(str(minor_planet_number)) > 0 and str(minor_planet_number)[0] == "!":
            self.null_observation = True
        return


    @property
    def null_observation(self):
        return self._null_observation

    @null_observation.setter
    def null_observation(self, null_observation):
        if type(null_observation) != bool:
            raise MPCFieldFormatError("null_observation",
                                      "should be True/False",
                                      null_observation)
        self._null_observation = null_observation

    @property
    def provisional_name(self):
        return self._provisional_name

    @provisional_name.setter
    def provisional_name(self, provisional_name=None):
        if provisional_name is None:
            provisional_name = " " * 7
        else:
            if not 0 < len(provisional_name) <= 7:
                raise MPCFieldFormatError("Provisional name",
                                          "must be 7 characters or less",
                                          provisional_name)

            if not provisional_name[0].isalpha():
                raise MPCFieldFormatError("Provisional name",
                                          "must start with a letter",
                                          provisional_name)

        self._provisional_name = provisional_name

    @property
    def discovery(self):
        """
        Is this a discovery observation?

        :return True/False
        """
        return self._discovery

    @discovery.setter
    def discovery(self, is_discovery):
        """

        :type is_discovery: bool
        :param is_discovery: indicates if observation was a discovery
        """
        self._discovery = Discovery(is_discovery=is_discovery)

    @property
    def note1(self):
        return self._note1

    @note1.setter
    def note1(self, note1):
        self._note1 = MPCNote(code=note1, note_type="Note1")

    @property
    def note2(self):
        return self._note2

    @note2.setter
    def note2(self, code):
        self._note2 = MPCNote(code=code, note_type="Note2")

    @property
    def date(self):
        return self._date

    @date.setter
    def date(self, datestr):
        self._date_precision = self._compute_precision(datestr)
        try:
            self._date = Time(datestr, format='mpc', scale='utc', precision=self._date_precision)
        except:
            raise MPCFieldFormatError("Observation Date",
                                      "does not match expected format",
                                      datestr)

    @property
    def ra(self):
        return self.coordinate.ra.format(unit=units.hour, decimal=False,
                                         sep=" ", precision=self._ra_precision, alwayssign=False,
                                         pad=True)

    @property
    def dec(self):
        return self.coordinate.dec.format(unit=units.degree, decimal=False,
                                          sep=" ", precision=self._dec_precision, alwayssign=True,
                                          pad=True)

    @property
    def comment(self):
        return self._comment

    @comment.setter
    def comment(self, comment):
        if comment is None:
            self._comment = ""
        else:
            self._comment = comment

    @property
    def coordinate(self):
        return self._coordinate

    def _compute_precision(self, coord):
        """
        Compute the number of decimal places in the last part of a sexagesimal string
        """
        parts = str(coord).replace('/', ' ').split('.')
        if len(parts) < 2:
            return 0
        else:
            return len(parts[-1])

    @coordinate.setter
    def coordinate(self, coord_pair):
        """

        :param coord_pair: RA/DEC pair [as a tuple or single string]
        """

        if type(coord_pair) in [list, tuple] and len(coord_pair) == 2:
            val1 = coord_pair[0]
            val2 = coord_pair[1]
        else:
            raise MPCFieldFormatError("RA/DEC",
                                      "Expected a pair of coordinates got: ",
                                      coord_pair)

        self._ra_precision = 3
        self._dec_precision = 2
        try:
            ra = float(val1)
            dec = float(val2)
            self._coordinate = coordinates.ICRSCoordinates(ra, dec, unit=(units.degree, units.degree))
        except:
            try:
                self._ra_precision = self._compute_precision(val1)
                self._dec_precision = self._compute_precision(val2)
                self._coordinate = coordinates.ICRSCoordinates(val1, val2, unit=(units.hour, units.degree))
            except Exception as e:
                raise MPCFieldFormatError("coord_pair",
                                          "must be [ra_deg, dec_deg] or HH MM SS.S[+-]dd mm ss.ss",
                                          coord_pair)

    @property
    def mag(self):
        return self._mag

    @mag.setter
    def mag(self, mag):
        if mag is None or len(str(str(mag).strip(' ')))==0 :
            self._mag_precision = 0
            self._mag = None
        else:
            self._mag = float(mag)
            self._mag_precision = min(2,self._compute_precision(str(mag)))

    @property
    def band(self):
        return self._band

    @band.setter
    def band(self, band):
        band = str(band.strip(' '))
        self._band = ( len(band) > 0 and str(band)[0] ) or None

    @property
    def observatory_code(self):
        return self._observatory_code

    @observatory_code.setter
    def observatory_code(self, observatory_code):
        observatory_code = str(observatory_code)
        if not len(observatory_code) <= 3:
            raise MPCFieldFormatError("Observatory code",
                                      "must be 3 characters or less",
                                      observatory_code)
        self._observatory_code = str(observatory_code)


class MPCWriter(object):
    """
    Writes out data about accepted objects in the Minor Planet Center's
    format as specified here:
    http://www.minorplanetcenter.net/iau/info/OpticalObs.html

    Note that we assume objects fall under the Minor Planet category.

    Format reproduced below for convenience:

        Columns     Format   Use
        1 -  5        A5     Minor planet number
        6 - 12        A7     Provisional or temporary designation
        13            A1     Discovery asterisk
        14            A1     Note 1
        15            A1     Note 2
        16 - 32      A17     Date of observation
        33 - 44      A12     Observed RA (J2000.0)
        45 - 56      A12     Observed Decl. (J2000.0)
        57 - 65       9X     Must be blank
        66 - 71    F5.2,A1   Observed magnitude and band
                               (or nuclear/total flag for comets)
        72 - 77       6X     Must be blank
        78 - 80       A3     Observatory code
    """

    def __init__(self, filehandle, auto_flush=True):
        self.filehandle = filehandle
        self.buffer = ""
        self.auto_flush = auto_flush
        self.date_regex = re.compile("\d{4} \d{2} \d{2}\.\d{5,6}")

    def get_filename(self):
        return self.filehandle.name

    def write(self, mpc_observation):
        """
        Writes a single entry in the Minor Planet Center's format.

        Minor planet number can be left empty ("").  All other fields
        should be provided.
        """
        line = mpc_observation.to_string()

        self.buffer += line + "\n"

        if self.auto_flush:
            self.flush()

    def flush(self):
        self.filehandle.write(self.buffer)
        self.filehandle.flush()
        self.buffer = ""

    def close(self):
        self.filehandle.close()
