__author__ = "David Rusk <drusk@uvic.ca>"

import collections
import re


def to_base26(number):
    if number < 0:
        raise ValueError("Must be a number >= 0")

    converted = ""

    should_continue = True
    while should_continue:
        remainder = number % 26
        converted = chr(remainder + ord('A')) + converted
        number = (number - remainder) / 26

        if number <= 0:
            should_continue = False

    return converted


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

        exposure_numbers = [int(reading.get_exposure_number()) for reading in source.get_readings()]
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

        return to_base26(int(exposure_number))

    def _get_extension(self, min_expnum):
        ext = self._processed_expnums[min_expnum]
        ext_str = "%02d" % ext

        if len(ext_str) > 2:
            raise ValueError("Source with min exposure number %d has had %s "
                             "names generated, which has length > 2." % (
                                 min_expnum, ext_str))

        return ext_str
