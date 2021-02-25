__author__ = "David Rusk <drusk@uvic.ca>"

import copy

from astropy.io.fits.hdu.hdulist import HDUList
from mock import Mock, MagicMock


class CopyingMock(Mock):
    """
    Useful when you want to make assertions about mutable call parameters.
    """

    def __call__(self, *args, **kwargs):
        args = copy.deepcopy(args)
        kwargs = copy.deepcopy(kwargs)
        return super(CopyingMock, self).__call__(*args, **kwargs)


class Dummy(object):
    """
    Create a dummy object that can be used as a substitute in test code.
    When any method gets called on it, it will print out the fact that the
    method was called.
    """

    def __init__(self, dummyname=None):
        if dummyname is not None:
            self._dummyname = dummyname
        else:
            self._dummyname = "Dummy"

    def __getattribute__(self, name):
        if name.startswith("_"):
            # This attribute is private to the Dummy class, access it
            # normally.
            attr = object.__getattribute__(self, name)
            if hasattr(attr, "__call__"):
                return attr()
            else:
                return attr

        def dummy_function(*args, **kwargs):
            print("Called %s on %s" % (name, self._dummyname))

        return dummy_function


def mock_hdulist():
    return MagicMock(spec=HDUList)
