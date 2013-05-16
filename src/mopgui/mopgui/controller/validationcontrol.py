__author__ = "David Rusk <drusk@uvic.ca>"


class SourceValidationController(object):
    def on_accept(self, event):
        print "Source accepted"

    def on_reject(self, event):
        print "Source rejected"
