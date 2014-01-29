from ossos import coding

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
        ef = self.get_epoch_field(astrom_header, fits_header)
        epoch_field = ef[0]+ef[1]
        count = storage.increment_object_counter(storage.MEASURE3, epoch_field)
        return ef[1] + count

    def get_epoch_field(self, astrom_header, fits_header):
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


        return (epoch, field)


class DryRunNameGenerator(ProvisionalNameGenerator):
    """
    Generate a fake name for dry runs so we don't increment counters.
    """
    def generate_name(self, astrom_header, fits_header):
        epoch_field = self.get_epoch_field(astrom_header, fits_header)
        count = storage.increment_object_counter(storage.MEASURE3,
                                                 epoch_field,
                                                 dry_run=True)

        base = "DRY"
        return base + count.zfill(7 - len(base))
