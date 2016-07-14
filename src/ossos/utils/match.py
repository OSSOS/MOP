import os
from astropy.io import ascii
import numpy
from ossos import util
from ossos import storage

BRIGHT_LIMIT = 23.0
OBJECT_PLANTED = "Object.planted"
MINIMUM_BRIGHT_DETECTIONS = 5
MINIMUM_BRIGHT_FRACTION = 0.5


class PlantedObject(object):

    def __init__(self, fk_candidate_filename, planted_filename=OBJECT_PLANTED):

        # Now get the Object.planted file, either from the local FS or from VOSpace.
        self.uri = planted_filename
        if not os.access(self.uri, os.F_OK):
            self.uri = fk_candidate_filename.observations[0].get_object_planted_uri()
        self._table = None

    @property
    def table(self):
        if self._table is not None:
            return self._table
        lines = storage.open_vos_or_local(self.uri).read()

        # format the file for easy loading.
        new_lines = lines.replace("pix rate", "pix_rate")
        new_lines = new_lines.replace("""''/h rate""", "sky_rate")
        self._table = ascii.read(new_lines, header_start=-1, data_start=0)
        return self._table


def planted(fk_candidate_observations, planted_objects, tolerance=10):
    """
    Using the fk_candidate_observations as input get the Object.planted file from VOSpace and match
    planted sources with found sources.

    The Object.planted list is pulled from VOSpace based on the standard file-layout and name of the
    first exposure as read from the .astrom file.

    :param fk_candidate_observations: name of the fk*reals.astrom file to check against Object.planted
    :param planted_objects: object containing the planted object information.

    """

    found_pos = []
    detections = fk_candidate_observations.get_sources()
    for detection in detections:
        reading = detection.get_reading(0)
        # create a list of positions, to be used later by match_lists
        found_pos.append([reading.x, reading.y])

    # The match_list method expects a list that contains a position, not an x and a y vector, so we transpose.
    planted_objects_table = planted_objects.table
    planted_pos = numpy.transpose([planted_objects_table['x'].data, planted_objects_table['y'].data])

    # match_idx is an order list.  The list is in the order of the first list of positions and each entry
    # is the index of the matching position from the second list.
    (match_idx, match_fnd) = util.match_lists(numpy.array(planted_pos), numpy.array(found_pos), tolerance=tolerance)
    return match_fnd, match_idx
