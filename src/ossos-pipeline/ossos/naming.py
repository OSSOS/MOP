from multiprocessing import Process, Queue

from ossos import astrom
from ossos import storage

__author__ = "David Rusk <drusk@uvic.ca>"


def _generate_provisional_name(q, astrom_header, fits_header):
    """
    Generates a name for an object given the information in its astrom
    observation header and FITS header.
    :param q: a queue of provisional names to return.
    :type q: Queue
    :param astrom_header:
    :param fits_header:
    """
    while True:
        ef = get_epoch_field(astrom_header, fits_header)
        epoch_field = ef[0] + ef[1]
        count = storage.increment_object_counter(storage.MEASURE3, epoch_field)
        try:
            q.put(ef[1] + count)
        except:
            break


def get_epoch_field(astrom_header, fits_header):
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

    return epoch, field


class ProvisionalNameGenerator(object):
    """
    Creates provisional names for a new sources.

    These names must be stored in a 7-character packed form:
    http://www.minorplanetcenter.net/iau/info/PackedDes.html
    """

    def __init__(self):
        self._astrom_header = None
        self._fits_header = None
        self.provisional_name_queue = None
        self.p = None

    def generate_name(self, astrom_header, fits_header):
        if not self._astrom_header or self._astrom_header != astrom_header:
            self._astrom_header = astrom_header
            self._fits_header = fits_header
            if self.provisional_name_queue is not None:
                self.provisional_name_queue.close()
            if self.p is not None:
                self.p.terminate()
            self.provisional_name_queue = Queue(1)
            self.p = Process(target=_generate_provisional_name, args=(self.provisional_name_queue,
                                                                 self._astrom_header,
                                                                 self._fits_header))
            self.p.start()
        return self.provisional_name_queue.get()


class DryRunNameGenerator(ProvisionalNameGenerator):
    """
    Generate a fake name for dry runs so we don't increment counters.
    """
    def generate_name(self, astrom_header, fits_header):
        epoch_field = get_epoch_field(astrom_header, fits_header)
        count = storage.increment_object_counter(storage.MEASURE3,
                                                 epoch_field,
                                                 dry_run=True)

        base = "DRY"
        return base + count.zfill(7 - len(base))
