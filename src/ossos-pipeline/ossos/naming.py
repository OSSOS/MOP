__author__ = "David Rusk <drusk@uvic.ca>"

from ossos import astrom
from ossos import coding
from ossos import storage


class ProvisionalNameGenerator(object):
    """
    Creates provisional names for a new sources.

    These names must be stored in a 7-character packed form:
    http://www.minorplanetcenter.net/iau/info/PackedDes.html
    """

    def name_source(self, source):
        """
        Generates a name for a source.
        """
        reading0 = source.get_reading(0)
        name = (coding.base52encode(int(reading0.get_exposure_number())) +
                coding.base62encode(int(reading0.x + reading0.y)))

        # Pad with 0's if short
        name = name.ljust(7, "0")

        if len(name) > 7:
            raise ValueError("Name is more than 7 characters: %s" % name)

        return name

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

        current_count = storage.get_object_counter(storage.MEASURE3, epoch_field)
        new_count = coding.base36encode(coding.base36decode(current_count) + 1, pad_length=2)
        storage.increment_object_counter(storage.MEASURE3, epoch_field)

        return epoch_field + new_count

