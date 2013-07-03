__author__ = "David Rusk <drusk@uvic.ca>"


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
            print "Called %s on %s" % (name, self._dummyname)

        return dummy_function
