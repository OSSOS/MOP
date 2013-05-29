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

    def generate_name(self, exposure_number):
        """
        Generates a name from an exposure number.  In order to compress it
        to the required number of characters, some assumptions have to be
        made about the range that exposure_number is in.  A ValueError will
        be raised if the exposure number does not meet these expectations.
        """
        exp_str = str(exposure_number)

        if not self.valid_expnum_reg.match(exp_str):
            raise ValueError(
                "Exposure number (%s) does not meet required format (%s)" % (
                    exp_str, self.valid_expnum_reg.pattern))

        exposure_number = int(exposure_number)

        encoded_expnum = to_base26(exposure_number)

        return encoded_expnum + self._get_obsnum_str(exposure_number)

    def _get_obsnum_str(self, expnum):
        """
        Get the suffix part of the name that gets incremented if we need
        to generate a name for the same exposure number multiple times.
        """
        obsnum = self._processed_expnums[expnum]
        obsnum_str = "%02d" % obsnum

        if len(obsnum_str) > 2:
            raise ValueError("Observation number for specified exposure "
                             "number (%d) has length greater than 2: %d" % (
                                 expnum, obsnum))

        self._processed_expnums[expnum] += 1

        return obsnum_str

