__author__ = "David Rusk <drusk@uvic.ca>"


class Singleton(type):
    """
    Meta-class for creating singletons.
    """

    def __init__(cls, name, bases, dict):
        super(Singleton, cls).__init__(name, bases, dict)
        cls.instance = None

    def __call__(cls, *args, **kw):
        if not cls.instance:
            # No instance created yet
            obj = super(Singleton, cls).__call__(*args, **kw)
            cls.instance = obj

        return cls.instance
