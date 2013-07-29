__author__ = "David Rusk <drusk@uvic.ca>"

import collections
import re

ALPHABET_BASE_26 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET_BASE_36 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET_BASE_62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


def base26encode(number):
    return encode(number, ALPHABET_BASE_26)


def base62encode(number):
    """
    Converts an integer to a base62 string (i.e. using the upper and lower
    case alphabet and the digits 0-9).
    """
    return encode(number, ALPHABET_BASE_36)


def encode(number, alphabet):
    """
    Converts an integer to a base n string where n is the length of the
    provided alphabet.

    Modified from http://en.wikipedia.org/wiki/Base_36
    """
    if not isinstance(number, (int, long)):
        raise TypeError("Number must be an integer.")

    base_n = ""
    sign = ""

    if number < 0:
        sign = "-"
        number = -number

    if 0 <= number < len(alphabet):
        return sign + alphabet[number]

    while number != 0:
        number, i = divmod(number, len(alphabet))
        base_n = alphabet[i] + base_n

    return sign + base_n


class ProvisionalNameGenerator(object):
    """
    Creates provisional names for a new sources.

    These names must be stored in a 7-character packed form:
    http://www.minorplanetcenter.net/iau/info/PackedDes.html
    """

    def __init__(self):
        self.valid_expnum_reg = re.compile("^\d{7}$")
        self._processed_expnums = collections.defaultdict(int)
        self._processed_sources = {}

    def name_source(self, source):
        """
        Generates a name for a source.
        """
        if source in self._processed_sources:
            return self._processed_sources[source]

        exposure_numbers = [int(reading.get_exposure_number())
                            for reading in source.get_readings()]
        min_expnum = min(exposure_numbers)

        base = self.compress_exposure_number(min_expnum)
        extension = self._get_extension(min_expnum)
        name = base + extension

        self._processed_expnums[min_expnum] += 1
        self._processed_sources[source] = name

        return name

    def compress_exposure_number(self, exposure_number):
        """
        Compresses an exposure number for use as part of naming a source.
        In order to compress it to the required number of characters, some
        assumptions have to be made about the range that exposure_number is
        in.  A ValueError will be raised if the exposure number does not
        meet these expectations.
        """
        exp_str = str(exposure_number)

        if not self.valid_expnum_reg.match(exp_str):
            raise ValueError(
                "Exposure number (%s) does not meet required format (%s)" % (
                    exp_str, self.valid_expnum_reg.pattern))

        return base26encode(int(exposure_number))

    def _get_extension(self, min_expnum):
        ext = self._processed_expnums[min_expnum]
        ext_str = "%02d" % ext

        if len(ext_str) > 2:
            raise ValueError("Source with min exposure number %d has had %s "
                             "names generated, which has length > 2." % (
                                 min_expnum, ext_str))

        return ext_str
