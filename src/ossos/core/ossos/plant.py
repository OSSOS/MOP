import fcntl
import os
from numpy import random
from astropy.table import Table
import numpy
from . import storage
from scipy import interpolate


class MatchFile(object):

    def __init__(self, prefix, field, ccd, version, ext):
        self.field = field
        self.ccd = ccd
        self.version = version
        self.prefix = prefix
        self.ext = "measure3.{}.astrom".format(ext)
        self.match_filename = 'test.txt'

    @property
    def filename(self):
        return os.path.basename(storage.get_cands_uri(self.field,
                                                      ccd=self.ccd,
                                                      version=self.version,
                                                      prefix=self.prefix,
                                                      ext="measure3.{}.match".format(self.ccd)))

    def __enter__(self):
        """Acquire a lock on the output file, prevents collisions between multiple runs."""
        self.fd = open(self.filename, 'a')
        fcntl.lockf(self.fd, fcntl.LOCK_EX)
        return self.fd

    def __exit__(self):
        self.fd.close()


class Range(object):
    """A custom object that is initialized with a range and when called returns a random value in that range."""

    def __init__(self, minimum, maximum=None, seed=None, func=None):
        random.seed(seed)
        if maximum is None:
            if len(minimum) == 2:
                maximum = minimum[1]
                minimum = minimum[0]
            else:
                raise ValueError("Range should be initialize with a tuple or two arguments giving min and max.")

        self.min = minimum
        self.max = maximum
        self.value = None
        self._dist = None
        self.func = func

    @property
    def dist(self):
        """
        How should the values in range be sampled, uniformly or via some function.
        :return:
        """
        if self.func is None:
            return random.uniform(self.min, self.max)
        if self._dist is None:
            x = numpy.arange(self.min, self.max, (self.max-self.min)/1000.0)
            p = self.func(x).cumsum()
            p -= p.min()
            p /= p.max()
            self._dist = interpolate.interp1d(p, x)
        return self._dist(random.random())

    def __call__(self, new=True):
        if new or self.value is None:
            self.value = self.dist
        return self.value

    def __eq__(self, other):
        return float(self) == float(other)

    def __cmp__(self, other):
        return cmp(float(self), float(other))

    def __float__(self):
        return self.value

    def __str__(self):
        if self.value is None:
            self.__call__()
        return str(self.value)


class KBOGenerator(object):

    def __init__(self, n, rate, angle, mag, x, y, id0=None):

        self.rate = rate
        self.angle = angle
        self.mag = mag
        self.x = x
        self.y = y
        self._id = id0
        self._n = n

    def __iter__(self):
        return self

    @property
    def id(self):
        if self._id is None:
            self._id = 0
        self._id += 1
        return self._id

    def __next__(self):
        """
        :return: a set of values that can be used for an planted object builder.
        """
        #        x          y        mag   pix rate      angle  ''/h rate     id
        # 912.48     991.06      22.01      57.32     -45.23      10.60      0
        self._n -= 1
        if self._n < 0:
            raise StopIteration()
        return {'x': self.x(), 'y': self.y(), 'mag': self.mag(), 'sky_rate': self.rate(), 'angle': self.angle(),
                'id': self.id}

    @classmethod
    def _step(cls, mag):
        low = mag * 0 + 0.3
        high = mag * 0 + 0.7
        g = [mag < 23.3] * low + [mag >= 23.3] * high
        return g[0]

    @classmethod
    def get_kbos(cls, n, rate, angle, mag, x, y, filename=None):

        kbos = Table(names=('x', 'y', 'mag', 'sky_rate', 'angle', 'id'))

        # generate the KBOs.
        for kbo in cls(n,
                       rate=Range(rate, func=lambda value: value**0.25),
                       angle=Range(angle),
                       mag=Range(mag, func=cls._step),
                       x=Range(x),
                       y=Range(y)):
            kbos.add_row(kbo)

        # Write to a local file if filename given.
        if filename is not None:
            fd = open(filename, 'w')
            fd.write("#K {:10s} {:10s} {:10s} {:10s} {:10s}\n".format("rate", "angle", "mag", "x_range", "y_range"))
            fd.write("#V {} {} {} {} {}\n".format(rate, angle, mag, x, y))
            fd.write("# ")
            kbos.write(fd, format='ascii.fixed_width', delimiter=None)
            fd.close()

        return kbos
