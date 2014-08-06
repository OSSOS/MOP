__author__ = 'jjk, mtb55'

import itertools
import os
import struct
import time
import logging

from datetime import datetime
from astropy import coordinates
from astropy.time import sofa_time
from astropy.time import TimeString
from astropy.time import Time
from astropy import units
import numpy

import storage


NULL_OBSERVATION_CHARACTERS = ["!", "-", "#"]
DEFAULT_OBSERVERS = ['M. T. Bannister', 'J. J. Kavelaars']
DEFAULT_TELESCOPE = "CFHT 3.6m + CCD"
DEFAULT_ASTROMETRIC_NETWORK = "UCAC4"

MPCNOTES = {"Note1": {" ": " ",
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
                      "w": "weak solution"},
            "Note2": {" ": " ",
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
                      "n": "Mini-normal place derived from averaging observations from video frames"},
            'PhotometryNote': {" ": " ",
                               "": " ",
                               "L": "Photometry uncertainty lacking",
                               "Y": "Photometry measured successfully",
                               "Z": "Photometry measurement failed."}}


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


class MPCNote(object):
    """
    Alphabetic note shown with some of the observations. Non-alphabetic codes are used to differentiate between
    different programs at the same site and such codes will be defined in the headings for the individual
    observatories in the Minor Planet Circulars.
    """

    def __init__(self, code="C", note_type="Note2"):
        self._note_type = None
        self._code = None
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
            raise ValueError("Invalid note_type: expected one of %s got %s" % (str(MPCNOTES.keys()), note_type))
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
                logging.debug("code {}".format(_code))
            if _code not in range(10):
                logging.debug("code {}".format(_code))
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
        self._is_discovery = False
        self._is_initial_discovery = False
        self.is_discovery = is_discovery
        self.is_initial_discovery = is_discovery

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
    def is_initial_discovery(self):
        return self._is_initial_discovery

    @property
    def is_discovery(self):
        return self._is_discovery

    @is_discovery.setter
    def is_discovery(self, is_discovery):
        if is_discovery not in ['*', '&', ' ', '', True, False, None]:
            raise MPCFieldFormatError("discovery",
                                      "must be one of '',' ','&', '*',True, False. Was: ",
                                      is_discovery)
        self._is_discovery = (is_discovery in ['*', '&', True] and True) or False

    @is_initial_discovery.setter
    def is_initial_discovery(self, is_discovery):
        """
        Is this MPC line the initial discovery line?
        @param is_discovery: the code for the discovery setting "*" or True or False
        """
        self._is_initial_discovery = (is_discovery in ["*", True] and True) or False

    def __str__(self):
        if self.is_initial_discovery:
            return "*"
        if self.is_discovery:
            return "&"
        return " "


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

    # ## need our own 'set_jds' function as the MPC Time string is not typical
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
                raise ValueError("Time {0} does not match {1} format".format(timestr, self.name))

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
            yield dict(year=int(iy), mon=int(im), day=int(iday), hour=int(ihr), min=int(imin), sec=int(isec),
                       fracsec=int(ifracsec), yday=yday, fracday=fracday)


Time.FORMATS[TimeMPC.name] = TimeMPC


def _compute_precision(coord):
    """
    Returns the number of digits after the last '.' in a given number or string.



    """
    coord = str(coord).strip(' ')
    idx = coord.rfind('.')
    if idx < 0:
        return 0
    else:
        return len(coord) - idx - 1


def get_date(date_string):
    """
    Given an MPC formatted time string return a Time object.

    :rtype : Time
    :param date_string: a string in MPC date format
    """
    _date_precision = _compute_precision(date_string)
    return Time(date_string, format='mpc', scale='utc', precision=_date_precision)


class Observation(object):
    """
    An observation of an object, nominally generated by reading an MPC formatted file.
    """

    def __init__(self,
                 null_observation=False,
                 provisional_name=None,
                 discovery=False,
                 note1=None,
                 note2=None,
                 date="2000 01 01.000001",
                 ra="00 00 00.000",
                 dec="+00 00 00.00",
                 mag=-1,
                 band='r',
                 observatory_code=568,
                 comment=None,
                 mag_err=-1,
                 xpos=None,
                 ypos=None,
                 frame=None,
                 plate_uncertainty=None):

        """

        :param provisional_name:
        :param discovery:
        :param note1:
        :param note2:
        :param date:
        :param ra:
        :param dec:
        :param mag:
        :param band:
        :param observatory_code:
        :param comment: A comment about this observation, not sent
        :param mag_err:
        :param xpos:
        :param ypos:
        :param frame:
        :param plate_uncertainty:
        :param null_observation:

        :type comment MPCComment
        """
        self.null_observation_character = "!"
        self._null_observation = False
        self.null_observation = null_observation
        self._provisional_name = ""
        self.provisional_name = provisional_name
        self.discovery = discovery
        self.note1 = note1
        self.note2 = note2
        self.date = date
        self.coordinate = (ra, dec)
        self._mag = None
        self._mag_err = None
        self._mag_precision = None
        self.mag = mag
        self.mag_err = mag_err
        self.band = band
        self.observatory_code = observatory_code
        self.comment = MPCComment(source_name=provisional_name,
                                  frame=frame,
                                  MPCNote=self.note1,
                                  X=xpos,
                                  Y=ypos,
                                  mag_uncertainty=mag_err,
                                  magnitude=mag,
                                  plate_uncertainty=plate_uncertainty,
                                  comment=comment)

    @classmethod
    def from_string(cls, mpc_line):
        """
        Given an MPC formatted line, returns an MPC Observation object.
        :param mpc_line: a line in the one-line roving observer format
        """
        mpc_format = '1s11s1s1s1s17s12s12s9x5s1s6x3s'
        mpc_line = mpc_line.strip('\n')
        comment = mpc_line[81:]
        mpc_line = mpc_line[0:80]
        if len(mpc_line) > 0 and mpc_line[0] == '#':
            return MPCComment.from_string(mpc_line[1:])
        if len(mpc_line) != 80:
            return None
        obsrec = cls(*struct.unpack(mpc_format, mpc_line))
        obsrec.comment = MPCComment.from_string(comment)
        # TODO set the 'discovery' flags from the binary flags in TNODB style ast lines.
        return obsrec

    def to_string(self):
        as_string = str(self)
        if self.comment is not None and str(self.comment) != "":
            as_string += " " + str(self.comment)
        return as_string

    def __str__(self):
        """
        Writes out data about accepted objects in the Minor Planet Center's 'Minor Planets'
        format as specified here:
        http://www.minorplanetcenter.net/iau/info/OpticalObs.html
        """
        # MOP/OSSOS allows the provisional name to take up the full space allocated to the MinorPlanetNumber AND
        # the provisional name.

        padding = " " * min(4, 11 - len(self.provisional_name))
        mpc_str = "%-12s" % (str(self.null_observation) + padding + self.provisional_name)

        mpc_str += str(self.discovery)
        mpc_str += '{0:1s}{1:1s}'.format(str(self.note1), str(self.note2))
        mpc_str += '{0:<17s}'.format(str(self.date))
        mpc_str += '{0:<12s}{1:<12s}'.format(str(self.ra), str(self.dec))
        mpc_str += 9 * " "
        mag_format = '{0:<5.' + str(self._mag_precision) + 'f}{1:1s}'
        mag_str = (self.mag is None and 6 * " ") or mag_format.format(self.mag, self.band)
        if len(mag_str) != 6:
            raise MPCFieldFormatError("mag",
                                      "length of mag string should be exactly 6 characters, got->",
                                      mag_str)
        mpc_str += mag_str
        mpc_str += 6 * " "
        mpc_str += "%3s" % self.observatory_code
        return mpc_str

    def to_tnodb(self):
        """
        provide string representation of observation in a format used for OSSOS database input.
        """

        # O indicates OSSOS survey
        comment_line = ('#O ' + str(self.comment))[:80].rstrip('\n')

        if self.mag == -1:  # write no mag and no filter for where photometry couldn't be measured
            self.mag = None
        else:
            # set mag precision back to 0.1 mags regardless of how good it actually is
            self._mag_precision = 1

        # set the null observation character to the tnodb value
        self.null_observation_character = "-"
        mpc_observation = str(self)

        return comment_line + '\n' + mpc_observation

    def to_mpc(self):
        self.null_observation_character = "#"
        return str(self)

    @property
    def null_observation(self):
        return self._null_observation

    @null_observation.setter
    def null_observation(self, null_observation=False):
        self._null_observation = _NullObservation(null_observation, self.null_observation_character)

    @null_observation.getter
    def null_observation(self, null_observation=False):
        self._null_observation = _NullObservation(null_observation, self.null_observation_character)

    @property
    def provisional_name(self):
        return self._provisional_name

    @provisional_name.setter
    def provisional_name(self, provisional_name=None):
        if provisional_name is None:
            provisional_name = " " * 7
        else:
            provisional_name = provisional_name.strip()
            # if not provisional_name[0].isalpha():
            # logging.warning("Provisional Name should not be a number: {}".format(provisional_name))
            # if not len(provisional_name) <= 7:
            #     logging.warning("Provisional Name too long {}".format(provisional_name))
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
        Returns the number of digits after the last '.' in a given number or string.

        """
        coord = str(coord).strip(' ')
        idx = coord.rfind('.')
        if idx < 0:
            return 0
        else:
            return len(coord) - idx - 1

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
        if mag is None or len(str(str(mag).strip(' '))) == 0:
            self._mag_precision = 0
            self._mag = None
        else:
            self._mag = float(mag)
            self._mag_precision = min(2, self._compute_precision(str(mag)))

    @property
    def mag_err(self):
        return self._mag_err

    @mag_err.setter
    def mag_err(self, mag_err):
        if mag_err is None or len(str(mag_err).strip('')) == 0 or self.mag is None:
            self._mag_err = None
        else:
            self._mag_err = mag_err


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


class _NullObservation(object):
    def __init__(self, null_observation, null_observation_character):
        if null_observation_character is None:
            null_observation_character = "-"
        self.null_observation_character = null_observation_character

        if isinstance(null_observation, basestring):
            # True if the first character of the null_observation argument is a '!', '-', or '#'
            self._null_observation = null_observation[0] in NULL_OBSERVATION_CHARACTERS
        elif isinstance(null_observation, bool):
            self._null_observation = null_observation
        else:
            self._null_observation = False
        assert type(self._null_observation) == bool

    def __str__(self):
        retval = self.null_observation_character if self._null_observation else " "
        assert isinstance(retval, basestring)
        return retval

    def __bool__(self):
        return self._null_observation


class MPCComment(object):
    """
    Parses an OSSOS observation's metadata into a format that can be stored in the 
    an Observation.comment and written out in the same MPC line.

    Specification: '1s11s1s1s1s17s12s12s9x5s1s6x3s'
    5.2f
    """

    def __init__(self,
                 frame,
                 source_name,
                 MPCNote,
                 X,
                 Y,
                 magnitude=None,
                 PNote=None,
                 mag_uncertainty=None,
                 plate_uncertainty=None,
                 comment=None):

        self.frame = frame
        self.source_name = source_name
        self.PNote = PNote
        self.MPCNote = MPCNote
        self.X = X
        self.Y = Y
        self.mag = magnitude
        self.mag_uncertainty = mag_uncertainty
        self.plate_uncertainty = plate_uncertainty
        self.comment = comment

    @classmethod
    def from_string(cls, comment):
        """
        Build an MPC Comment from a string.
        """
        comment_format = '1s10s1s11s1s3s6f1s6f'  # 1s4f1s3f1s4s1s'  # is this right...?
        values = comment.split('%')[0]
        try:
            retval = cls(*struct.unpack(comment_format, values))
            retval.comment = comment.split('%')[1]  # comment length is not confined
            return retval
        except:
            if len(values) < 6:  # something is just weird
                logging.debug("non-OSSOS format MPC line read: {}".format(comment))
        try:
            values = values.split()
            if values[3] == 'L':
                return comment
            if values[3] == 'O':
                values = values[4:]
            retval = MPCComment(frame=values[0],
                                source_name=values[1],
                                PNote=values[2][0],
                                MPCNote=values[2][1:],
                                X=values[3],
                                Y=values[4],
                                comment=comment.split('%')[1].lstrip(' '))
            if len(values) > 7:  # a line can have up to 8 values when mag/mag_uncertainty are set
                retval.mag = values[5]
                retval.mag_uncertainty = values[6]
                retval.plate_uncertainty = values[7]
            else:
                retval.plate_uncertainty = values[-1]
            return retval
        except:
            return comment


    @property
    def mag(self):
        return self._mag

    @mag.setter
    def mag(self, mag):
        try:
            if float(mag) > 0.:
                self._mag = "{:5.2f}".format(float(mag))
                self.PNote = "Y"
            else:
                self._mag = " " * 5
                self.PNote = "Z"
        except:
            self.PNote = "Z"
            self._mag = " " * 5

    @property
    def mag_uncertainty(self):
        return self._mag_uncertainty

    @mag_uncertainty.setter
    def mag_uncertainty(self, mag_uncertainty):
        try:
            if float(mag_uncertainty) > 0:
                self._mag_uncertainty = "{:4.2f}".format(float(mag_uncertainty))
            else:
                self._mag_uncertainty = " " * 4
                if str(self.mag).isdigit():
                    self.PNote = "L"
                else:
                    self.PNote = "Z"
        except:
            self._mag_uncertainty = " " * 4
            self.PNote = "Z"

    @property
    def PNote(self):
        return self._PNote

    @PNote.setter
    def PNote(self, PNote):
        self._PNote = PNote

    @property
    def X(self):
        return self._X

    @X.setter
    def X(self, X):
        try:
            self._X = "{:6.1f}".format(float(X))
        except:
            self._X = "X" * 6

    @property
    def Y(self):
        return self._Y

    @Y.setter
    def Y(self, Y):
        try:
            self._Y = "{:6.1f}".format(float(Y))
        except:
            self._Y = "Y" * 6

    @property
    def plate_uncertainty(self):
        return self._plate_uncertainty

    @plate_uncertainty.setter
    def plate_uncertainty(self, plate_uncertainty):
        try:
            self._plate_uncertainty = "{:4.2}".format(float(plate_uncertainty))
        except:
            self._plate_uncertainty = "U" * 4

    @property
    def comment(self):
        return self._comment

    @comment.setter
    def comment(self, comment):
        if comment is not None:
            try:
                self._comment = str(comment)
            except:
                self._comment = ''
        else:
            self._comment = ''

    def __str__(self):
        """
        Format comment as required for storing OSSOS metadata
        odonum p ccd object_name MPCnotes X Y mag mag_uncertainty plate_uncertainty % comment
        """
        # The astrometric uncertainty should be set to higher when hand measurements are made.

        comm = '{}'.format(self.frame)
        comm += ' {}'.format(self.source_name)
        comm += ' {}{:1s}'.format(self.PNote, str(self.MPCNote))
        comm += ' {} {}'.format(self.X, self.Y)
        comm += ' {} {}'.format(self.mag, self.mag_uncertainty)
        comm += ' {}'.format(self.plate_uncertainty)
        comm += ' % {}'.format(self.comment)  # % denotes comment start

        return comm


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

    def __init__(self, file_handle, auto_flush=True, include_comments=True,
                 auto_discovery=True, formatter=None):
        self.filehandle = file_handle
        self.auto_flush = auto_flush
        self.include_comments = include_comments

        # Holds observations that have not yet been flushed
        self.buffer = {}
        self._written_mpc_observations = []

        self.auto_discovery = auto_discovery
        self._discovery_written = False
        if formatter is None:
            if self.include_comments:
                self.formatter = Observation.to_string
            else:
                self.formatter = Observation.__str__
        else:
            self.formatter = formatter

    def get_filename(self):
        return self.filehandle.name

    def write(self, mpc_observation):
        """
        Writes a single entry in the Minor Planet Center's format.
        :param mpc_observation:
        """
        assert isinstance(mpc_observation, Observation)
        key = mpc_observation.date.mjd
        self.buffer[key] = mpc_observation

        if self.auto_flush:
            self.flush()

    def flush(self):
        for obs in self.get_chronological_buffered_observations():
            self._flush_observation(obs)

        self.filehandle.flush()

    def _flush_observation(self, obs):
        isinstance(obs, Observation)
        if (self.auto_discovery and
                not obs.null_observation and
                not self._discovery_written):
            obs.discovery = True

        if obs.discovery and self._discovery_written:
            obs.discovery.is_initial_discovery = False
        else:
            self._discovery_written = True

        if obs.date.jd not in self._written_mpc_observations:
            self._written_mpc_observations.append(obs.date.jd)
            line = self.formatter(obs)
            self.filehandle.write(line + "\n")

    def close(self):
        self.filehandle.close()

    def get_chronological_buffered_observations(self):
        jds = self.buffer.keys()
        jds.sort()
        sorted_obs = []
        for jd in jds:
            sorted_obs.append(self.buffer[jd])
        return sorted_obs


def make_tnodb_header(observations, observatory_code=None, observers=DEFAULT_OBSERVERS,
                      telescope=DEFAULT_TELESCOPE, astrometric_network=DEFAULT_ASTROMETRIC_NETWORK):
    """
    Write a header appropriate for a tnodb style of file.
    """
    observatory_code = observatory_code is None and observations[0].observatory_code or observatory_code

    odates = [obs.date for obs in observations]
    mindate = min(odates).iso.replace('-', '')[0:8]
    maxdate = max(odates).iso.replace('-', '')[0:8]

    header = "COD {}\n".format(observatory_code)

    sep = ""
    header += "OBS "
    for observer in observers[:-1]:
        header += "{}{}".format(sep, observer)
        sep = ", "
    if len(observers) > 1:
        header += " and {}".format(observers[-1])

    header += "\n"
    header += "TEL {}\n".format(telescope)
    header += "NET {}\n".format(astrometric_network)
    header += "{:s} {:s}\n".format('STD', mindate)
    header += "{:s} {:s}\n".format('END', maxdate)

    return header


class MPCReader(object):
    """
    Takes the filename of either a .mpc or .ast format file and parses that file
    to instantiate an array of mpc.Observation objects.
    """

    def __init__(self, filename):
        self.mpc_observations = []
        filehandle = storage.open_vos_or_local(filename, "rb")
        filestr = filehandle.read()
        filehandle.close()

        input_mpc_lines = filestr.split('\n')

        next_comment = None
        for line in input_mpc_lines:
            mpc_observation = Observation.from_string(line)
            if isinstance(mpc_observation, MPCComment):
                next_comment = mpc_observation
                continue
            if isinstance(mpc_observation, Observation):
                if next_comment is not None:
                    mpc_observation.comment = next_comment
                    next_comment = None

                if filename.endswith('ast'):  # then it has an OSSOS designation: set that in preference
                    mpc_observation.provisional_name = filename.rsplit('/')[-1].rstrip('.ast').split('.')[0]
                self.mpc_observations.append(mpc_observation)

        self.mpc_observations.sort(key=lambda obs: obs.date.jd)


class Index(object):
    """
    MOP/OSSOS name mapping index.
    """
    MAX_NAME_LENGTH = 10

    def __init__(self, idx_filename):
        self.names = {}
        self.index = {}
        with open(idx_filename, 'r') as idx_handle:
            for line in idx_handle.readlines():
                master_name = line[0:Index.MAX_NAME_LENGTH]
                master_name = master_name.strip()
                self.names[master_name] = master_name
                self.index[master_name] = [master_name]
                for i in range(Index.MAX_NAME_LENGTH, len(line), Index.MAX_NAME_LENGTH):
                    this_name = line[i:i + Index.MAX_NAME_LENGTH].strip()
                    self.index[master_name].append(this_name)
                    self.names[this_name] = master_name

    def __str__(self):
        result = ""
        for name in self.index:
            result += "{0:<{1}s}".format(name, Index.MAX_NAME_LENGTH)
            for alias in self.get_aliases(name):
                result += "{0:<{1}s}".format(alias, Index.MAX_NAME_LENGTH)
            result += "\n"
        return result

    def get_aliases(self, name):
        """
        get all names associated with a given name.
        :rtype : list
        :param name: object to get alias names of.
        """
        if name not in self.names:
            return name
        return self.index[self.names[name]]

    def is_same(self, name1, name2):
        """
        Do name1 and name2 refer to the same object?

        :param name1: name of object 1
        :param name2: name of object 2
        :return: Bool
        """
        return name2 in self.get_aliases(name1)


class MPCConverter(object):
    """
    Converts an MPC formatted file to a TNOdb one.
    :param mpc_file The input filename, of MPC lines.
    :param output   if required; else will use root of provided MPC file.

    batch_convert is factory method that will write out a series of input files given an input path.
    """

    def __init__(self, mpc_file, output=None):

        if output is None:
            output = mpc_file.rpartition('.')[0] + '.tnodb'

        self.mpc_file = mpc_file
        self.outfile = open(output, 'w')
        self.write_header = True

    def convert(self):
        with open(self.mpc_file, 'r') as infile:
            observations = []
            for line in infile.readlines():
                obs = Observation().from_string(line)
                observations.append(obs)

            if self.write_header:
                self.outfile.write(make_tnodb_header(observations))
                self.write_header = False

            for obs in observations:
                self.outfile.write(obs.to_tnodb() + '\n')

    @classmethod
    def batch_convert(cls, path):
        for fn in os.listdir(path):
            if fn.endswith('.mpc') or fn.endswith('.track') or fn.endswith('.checkup') or fn.endswith('.nailing'):
                cls(path + fn).convert()

