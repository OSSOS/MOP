__author__ = "David Rusk <drusk@uvic.ca>"

from ossos import astrom
from ossos import storage


class ProvisionalNameGenerator(object):
    """
    Creates provisional names for a new sources.

    These names must be stored in a 7-character packed form:
    http://www.minorplanetcenter.net/iau/info/PackedDes.html
    """

    def generate_name(self, astrom_header, fits_header):
        """
        Generates a name for an object given the information in its astrom
        observation header and FITS header.
        """
        # Format: "YYYY MM DD.dddddd"
        date = astrom_header[astrom.MJD_OBS_CENTER]
        year, month, _ = date.split()

        semester = "A" if 2 <= int(month) <= 7 else "B"

        epoch = year[-2:] + semester

        object_header = fits_header["OBJECT"]

        if object_header.startswith(epoch):
            field = object_header[len(epoch)]
        else:
            field = object_header[0]

        epoch_field = "O" + epoch + field

        count = storage.increment_object_counter(storage.MEASURE3, epoch_field)

        return epoch_field + count


class DryRunNameGenerator(object):
    """
    Generate a fake name for dry runs so we don't increment counters.
    """
    def __init__(self):
        self.counter = 0

    def generate_name(self, astrom_header, fits_header):
        base = "DRY"
        count = str(self.counter).zfill(7 - len(base))

        self.counter += 1

        return base + count
