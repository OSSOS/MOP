__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.gui.fitsviewer.baseviewer import MPLFitsViewer


class TripletViewer(MPLFitsViewer):
    """
    Displays a single FITS image at a time.
    """

    def __init__(self, parent):
        super(TripletViewer, self).__init__(parent)

    def display(self, triplets):
        """
        Display the triplet data.
        """
        pass
