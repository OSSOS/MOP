__author__ = "David Rusk <drusk@uvic.ca>"


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

    def generate_name(self, exposure_number):
        """
        Generates a name from an exposure number.  In order to compress it
        to the required number of characters, some assumptions have to be
        made about the range that exposure_number is in.  A ValueError will
        be raised if the exposure number does not meet these expectations.
        """
        exp_str = str(exposure_number)

        if len(exp_str) != 7:
            raise ValueError("Expected 7 digit exposure number but was: %d" % exposure_number)

        # TODO


class AcceptRejectResultsWriter(object):
    """
    A simplified output that just writes the source name and whether it
    was accepted or rejected.
    """

    def __init__(self, outputfilename):
        self.filehandle = open(outputfilename, "wb")

    def write_result(self, sourcename, status):
        self.filehandle.write("%s: %s" % (sourcename, status))

    def close(self):
        self.filehandle.close()
