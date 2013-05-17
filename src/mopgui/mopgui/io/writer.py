__author__ = "David Rusk <drusk@uvic.ca>"


class AcceptRejectResultsWriter(object):
    """
    A simplified output that just writes the source name and whether it
    was accepted or rejected.  It just does formatting, it must be provided
    with all required data.
    """

    def __init__(self, filehandle, name_generator):
        self.name_generator = name_generator
        self.filehandle= filehandle

    def write_result(self, sourcename, status):
        self.filehandle.write("%s: %s\n" % (sourcename, status))

    def close(self):
        self.filehandle.close()