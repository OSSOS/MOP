__author__ = "David Rusk <drusk@uvic.ca>"

import re

from astropy import coordinates
from astropy import units


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

    def __init__(self, filehandle):
        self.filehandle = filehandle
        self.date_regex = re.compile("\d{4} \d{2} \d{2}\.\d{5,6}")

    def write_line(self,
                   minor_planet_number,
                   provisional_name,
                   discovery_asterisk,
                   note1,
                   note2,
                   date_of_obs,
                   ra,
                   dec,
                   obs_mag,
                   band,
                   observatory_code="568"):
        """
        Writes a single entry in the Minor Planet Center's format.

        Minor planet number can be left empty ("").  All other fields
        should be provided.
        """
        # Convert some fields to strings for convenience and perform
        # filling/justification
        minor_planet_number = str(minor_planet_number).ljust(5)
        date_of_obs = date_of_obs.ljust(17)
        discovery_asterisk = discovery_asterisk.ljust(1)
        note1 = note1.ljust(1)
        note2 = note2.ljust(1)
        observatory_code = str(observatory_code).zfill(3)
        obs_mag = str(round(float(obs_mag), 2)).ljust(5)

        # Check for invalid values
        if len(minor_planet_number) > 5:
            raise MPCFieldFormatError("Minor planet number",
                                      "must be 5 characters or less",
                                      minor_planet_number)

        if not 0 < len(provisional_name) <= 7:
            raise MPCFieldFormatError("Provisional name",
                                      "must be 7 characters or less",
                                      provisional_name)

        if not provisional_name[0].isalpha():
            raise MPCFieldFormatError("Provisional name",
                                      "must start with a letter",
                                      provisional_name)

        if not discovery_asterisk in ["", " ", "*"]:
            raise MPCFieldFormatError("Discovery asterisk",
                                      "must be one of ['', ' ', '*']",
                                      discovery_asterisk)

        # has already been justified
        if not len(note1) == 1:
            raise MPCFieldFormatError("Note1",
                                      "must be 0 or 1 characters",
                                      note1)

        # has already been justified
        if not len(note2) == 1:
            raise MPCFieldFormatError("Note2",
                                      "must be 0 or 1 characters",
                                      note2)

        if not self.date_regex.match(date_of_obs):
            raise MPCFieldFormatError("Date of observation",
                                      "must match regex: %s" % self.date_regex.pattern,
                                      date_of_obs)

        if not _is_numeric(ra):
            raise MPCFieldFormatError("RA",
                                      "must be numeric (can be in string form)",
                                      ra)

        if not _is_numeric(dec):
            raise MPCFieldFormatError("DEC",
                                      "must be numeric (can be in string form)",
                                      dec)

        if not _is_numeric(obs_mag) or not len(obs_mag) <= 5:
            raise MPCFieldFormatError("Observed magnitude",
                                      "must be numeric (can be in string form) "
                                      "and less than or equal to 5 characters "
                                      "after being rounded to 2 decimal places",
                                      obs_mag)

        if not len(band) == 1:
            raise MPCFieldFormatError("Band",
                                      "must be exactly 1 character",
                                      band)

        if not len(observatory_code) <= 3:
            raise MPCFieldFormatError("Observatory code",
                                      "must be 3 characters or less",
                                      observatory_code)

        formatted_ra, formatted_dec = format_ra_dec(ra, dec)

        line = (minor_planet_number + provisional_name + discovery_asterisk +
                note1 + note2 + date_of_obs + formatted_ra + formatted_dec +
                " " * 9 + obs_mag + band + " " * 6 + observatory_code + "\n")

        # subtract 1 because of newline character
        line_len = len(line) - 1
        if line_len != 80:
            raise MPCFormatError("MPC line must be 80 characters but was: %d" % line_len)

        self.filehandle.write(line)
        self.filehandle.flush()


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
