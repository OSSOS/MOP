import math
import Polygon
import logging

from astropy import units
from astropy.coordinates import SkyCoord


class Camera:
    """The Field of View of a direct imager"""

    _geometry = {
        "MP_CCD": [
            {"ra": 0., "dec": 0., "dra": 0.1052, "ddec": 0.2344}
        ],
        "GMOS-S": [
            {"ra": 0., "dec": 0., 'dra': 330.0/3600.0, 'ddec': 330.0/3600.0}
            ],
        "HSC": [
            {"ra": 0.0, "dec": 0.0, "rad": 0.75}
        ],
        "MEGACAM_00": [
            {"ra": 0, "dec": 0, "dra": 2112 * 0.185 / 3600.0, "ddec": 4640 * 0.185 / 3600.0}
        ],
        "LBT": [{'dra': 1.3000992865659029, 'ddec': 0.42162904764019515, 'dec': 0.00019679877439671145,
                 'ra': 0.0042193345733494425, 'name': 'LBC_BLUE'},
#                {'dra': 1.3092201073853289, 'ddec': 0.42897343292189305, 'dec': -0.00019679877439671145,
#                 'ra': -0.0042193345733494425, 'name': 'LBC-RED'}
                ],
        "MEGACAM_40": [
            {"ra": +0.440 + 0.111, "dec": -0.105, "ddec": 0.241, "dra": 0.111},
            {"ra": +0.438 + 0.111, "dec": +0.128, "ddec": 0.241, "dra": 0.111},
            {"ra": -0.438 - 0.111, "dec": -0.111, "ddec": 0.241, "dra": 0.111},
            {"ra": -0.438 - 0.111, "dec": +0.128, "ddec": 0.241, "dra": 0.111},
            {"ra": 0.435, "dec": 0.394, "dra": 0.111, "ddec": 0.238},
            {"ra": 0.327, "dec": 0.394, "dra": 0.112, "ddec": 0.238},
            {"ra": 0.218, "dec": 0.394, "dra": 0.112, "ddec": 0.239},
            {"ra": 0.108, "dec": 0.393, "dra": 0.112, "ddec": 0.239},
            {"ra": -0.001, "dec": 0.392, "dra": 0.111, "ddec": 0.240},
            {"ra": -0.111, "dec": 0.391, "dra": 0.111, "ddec": 0.240},
            {"ra": -0.221, "dec": 0.390, "dra": 0.110, "ddec": 0.240},
            {"ra": -0.330, "dec": 0.389, "dra": 0.109, "ddec": 0.240},
            {"ra": -0.438, "dec": 0.387, "dra": 0.108, "ddec": 0.239},
            {"ra": 0.439, "dec": 0.134, "dra": 0.110, "ddec": 0.240},
            {"ra": 0.330, "dec": 0.134, "dra": 0.111, "ddec": 0.241},
            {"ra": 0.220, "dec": 0.133, "dra": 0.111, "ddec": 0.241},
            {"ra": 0.110, "dec": 0.133, "dra": 0.111, "ddec": 0.242},
            {"ra": 0.000, "dec": 0.132, "dra": 0.112, "ddec": 0.242},
            {"ra": -0.110, "dec": 0.131, "dra": 0.111, "ddec": 0.242},
            {"ra": -0.219, "dec": 0.130, "dra": 0.111, "ddec": 0.242},
            {"ra": -0.329, "dec": 0.129, "dra": 0.110, "ddec": 0.242},
            {"ra": -0.438, "dec": 0.128, "dra": 0.109, "ddec": 0.241},
            {"ra": 0.440, "dec": -0.105, "dra": 0.109, "ddec": 0.241},
            {"ra": 0.331, "dec": -0.106, "dra": 0.110, "ddec": 0.241},
            {"ra": 0.222, "dec": -0.107, "dra": 0.111, "ddec": 0.242},
            {"ra": 0.112, "dec": -0.108, "dra": 0.111, "ddec": 0.242},
            {"ra": 0.002, "dec": -0.109, "dra": 0.112, "ddec": 0.242},
            {"ra": -0.108, "dec": -0.109, "dra": 0.112, "ddec": 0.242},
            {"ra": -0.218, "dec": -0.110, "dra": 0.112, "ddec": 0.241},
            {"ra": -0.327, "dec": -0.111, "dra": 0.111, "ddec": 0.241},
            {"ra": -0.436, "dec": -0.111, "dra": 0.111, "ddec": 0.240},
            {"ra": 0.441, "dec": -0.364, "dra": 0.108, "ddec": 0.239},
            {"ra": 0.332, "dec": -0.365, "dra": 0.109, "ddec": 0.240},
            {"ra": 0.223, "dec": -0.367, "dra": 0.110, "ddec": 0.240},
            {"ra": 0.114, "dec": -0.368, "dra": 0.111, "ddec": 0.240},
            {"ra": 0.004, "dec": -0.369, "dra": 0.111, "ddec": 0.240},
            {"ra": -0.105, "dec": -0.370, "dra": 0.112, "ddec": 0.239},
            {"ra": -0.215, "dec": -0.370, "dra": 0.112, "ddec": 0.239},
            {"ra": -0.324, "dec": -0.370, "dra": 0.112, "ddec": 0.238},
            {"ra": -0.433, "dec": -0.370, "dra": 0.112, "ddec": 0.238}
        ],
        "LFCNS": [
            {"ra": -0.0512, "dec": 0.1045, "dra": .1023, "ddec": 0.2047},
            {"ra": 0.0512, "dec": 0.1045, "dra": .1023, "ddec": 0.2047},
            {"ra": -0.0512, "dec": -0.1045, "dra": .1023, "ddec": 0.2047},
            {"ra": 0.0512, "dec": -0.1045, "dra": .1023, "ddec": 0.2047},
            {"ra": 0.1536, "dec": 0., "dra": .1023, "ddec": 0.2047},
            {"ra": -0.1536, "dec": 0., "dra": .1023, "ddec": 0.2047}
        ],
        "LFCEW": [
            {"dec": -0.0532, "ra": 0.1045, "ddec": .1023, "dra": 0.2047},
            {"dec": 0.0532, "ra": 0.1045, "ddec": .1023, "dra": 0.2047},
            {"dec": -0.0532, "ra": -0.1045, "ddec": .1023, "dra": 0.2047},
            {"dec": 0.0532, "ra": -0.1045, "ddec": .1023, "dra": 0.2047},
            {"dec": 0.1598, "ra": 0., "ddec": .1023, "dra": 0.2047},
            {"dec": -0.1598, "ra": 0., "ddec": .1023, "dra": 0.2047}
        ],
        "MEGACAM_1": [
            {"ra": 0, "dec": 0, "dra": 0.98, "ddec": 0.98}
        ],
        "MMCAM": [
            {'ra': -0.162500, 'dec': -0.189444, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.162500, 'dec': -0.141944, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.162500, 'dec': -0.095000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.162500, 'dec': -0.047500, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.162500, 'dec': 0.000000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.162500, 'dec': 0.047500, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.162500, 'dec': 0.095000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.162500, 'dec': 0.141944, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.162500, 'dec': 0.189444, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': -0.189444, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': -0.141944, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': -0.095000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': -0.047500, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': 0.000000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': 0.047500, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': 0.095000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': 0.141944, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': -0.051389, 'dec': 0.189444, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': -0.189444, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': -0.141944, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': -0.095000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': -0.047500, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': 0.000000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': 0.047500, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': 0.095000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': 0.141944, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.051389, 'dec': 0.189444, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': -0.189444, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': -0.141944, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': -0.095000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': -0.047500, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': 0.000000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': 0.047500, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': 0.095000, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': 0.141944, 'dra': 0.102222, 'ddec': 0.045333},
            {'ra': 0.162500, 'dec': 0.189444, 'dra': 0.102222, 'ddec': 0.045333}
        ],
        "MEGACAM_2": [
            {"ra": 0, "dec": -0.252, "dra": 0.98, "ddec": 0.478},
            {"ra": 0, "dec": +0.235, "dra": 0.98, "ddec": 0.478}],
        "EW-MOSAIC": [
            {"dec": 1.5 * 0.1479 + 0.0036, "ra": -0.1479 - 0.0019, "dra": 2.0 * 0.1479, "ddec": 0.1479, },
            {"dec": 0.5 * 0.1479 + 0.0011, "ra": -0.1479 - 0.0019, "dra": 2.0 * 0.1479, "ddec": 0.1479, },
            {"dec": -0.5 * 0.1479 - 0.0011, "ra": -0.1479 - 0.0019, "dra": 2.0 * 0.1479, "ddec": 0.1479, },
            {"dec": -1.5 * 0.1479 - 0.0036, "ra": -0.1479 - 0.0019, "dra": 2.0 * 0.1479, "ddec": 0.1479, },
            {"dec": 1.5 * 0.1479 + 0.0036, "ra": 0.1479 + 0.0019, "dra": 2.0 * 0.1479, "ddec": 0.1479, },
            {"dec": 0.5 * 0.1479 + 0.0011, "ra": 0.1479 + 0.0019, "dra": 2.0 * 0.1479, "ddec": 0.1479, },
            {"dec": -0.5 * 0.1479 - 0.0011, "ra": 0.1479 + 0.0019, "dra": 2.0 * 0.1479, "ddec": 0.1479, },
            {"dec": -1.5 * 0.1479 - 0.0036, "ra": 0.1479 + 0.0019, "dra": 2.0 * 0.1479, "ddec": 0.1479, },
        ],
        "NS-MOSAIC": [
            {"ra": 1.5 * 0.1479 + 0.0036, "dec": -0.1479 - 0.0019, "ddec": 2.0 * 0.1479, "dra": 0.1479, },
            {"ra": 0.5 * 0.1479 + 0.0011, "dec": -0.1479 - 0.0019, "ddec": 2.0 * 0.1479, "dra": 0.1479, },
            {"ra": -0.5 * 0.1479 - 0.0011, "dec": -0.1479 - 0.0019, "ddec": 2.0 * 0.1479, "dra": 0.1479, },
            {"ra": -1.5 * 0.1479 - 0.0036, "dec": -0.1479 - 0.0019, "ddec": 2.0 * 0.1479, "dra": 0.1479, },
            {"ra": 1.5 * 0.1479 + 0.0036, "dec": 0.1479 + 0.0019, "ddec": 2.0 * 0.1479, "dra": 0.1479, },
            {"ra": 0.5 * 0.1479 + 0.0011, "dec": 0.1479 + 0.0019, "ddec": 2.0 * 0.1479, "dra": 0.1479, },
            {"ra": -0.5 * 0.1479 - 0.0011, "dec": 0.1479 + 0.0019, "ddec": 2.0 * 0.1479, "dra": 0.1479, },
            {"ra": -1.5 * 0.1479 - 0.0036, "dec": 0.1479 + 0.0019, "ddec": 2.0 * 0.1479, "dra": 0.1479, },
        ],
        "L2": [
            {"ra": 0, "dec": +0.98 * 0.5, "ddec": 1.0 * 0.98, "dra": 8.0 * 0.98, },
            {"ra": 0, "dec": -0.98 * 0.5, "ddec": 1.0 * 0.98, "dra": 8.0 * 0.98, },
        ],
        "SSC": [
            {"ra": -2 * (0.20 * 2048 + 15.0) / 3600.0, "dec": 0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": -1 * (0.20 * 2048 + 15.0) / 3600.0, "dec": 0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": -0 * (0.20 * 2048 + 15.0) / 3600.0, "dec": 0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": 1 * (0.20 * 2048 + 15.0) / 3600.0, "dec": 0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": 2 * (0.20 * 2048 + 15.0) / 3600.0, "dec": 0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": -2 * (0.20 * 2048 + 15.0) / 3600.0, "dec": -0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": -1 * (0.20 * 2048 + 15.0) / 3600.0, "dec": -0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": -0 * (0.20 * 2048 + 15.0) / 3600.0, "dec": -0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": 1 * (0.20 * 2048 + 15.0) / 3600.0, "dec": -0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
            {"ra": 2 * (0.20 * 2048 + 15.0) / 3600.0, "dec": -0.5 * (4096 * 0.20 + 15.0) / 3600.0,
             "ddec": 0.20 * 4096 / 3600.0, "dra": 0.20 * 2048 / 3600.0},
        ],
        "SSC1": [
            {"ra": 0.0, "dec": 0.0, "dra": 5 * (0.20 * 2048 / 3600.0 + 15.0 / 3600.0) - 15.0 / 3600.0,
             "ddec": 2 * 0.20 * 4096 / 3600 + 15.0 / 3600.0},
        ]
    }

    def __init__(self,  ra, dec=None, camera="MEGACAM_40",):
        self._origin = None
        if dec is None:
            if isinstance(ra, SkyCoord):
                self.origin = ra
            else:
                raise NotImplementedError("Can't do that.")
        else:
            self.set_origin((ra, dec))
        if camera == '':
            camera = "MEGACAM_40"
        self.camera = camera
        self._coordinate = None

    def __str__(self):

        return str(self.coord)

    @property
    def names(self):
        names = {'one': 0}
        if self.camera == "MEGACAM_40":
            names = {'ccd38': 0, 'ccd36': 1, 'ccd37': 2, 'ccd39': 3}
            for i in range(36):
                names['ccd{:02d}'.format(i)] = i+4
        elif self.camera == "LBT":
            names = {'red': 0, 'blue': 1}
        return names

    def offset(self, index=0):
        """Offset the camera pointing to be centred on a particular CCD."""
        eta = self._geometry[self.camera][index]["ra"]
        xi = self._geometry[self.camera][index]["dec"]
        ra = self.origin.ra - (eta/math.cos(self.dec.radian))*units.degree
        dec = self.origin.dec - xi * units.degree + 45 * units.arcsec
        self._coordinate = SkyCoord(ra, dec)

    @property
    def coordinate(self):
        return self.coord

    @property
    def coord(self):
        """The center of the camera pointing in sky coordinates"""
        if self._coordinate is None:
            self._coordinate = SkyCoord(self.origin.ra, self.origin.dec + 45 * units.arcsec)
        return self._coordinate

    @property
    def ra(self):
        return self.coord.ra

    @property
    def dec(self):
        return self.coord.dec

    @property
    def origin(self):
        """
        Origin of the pointing
        @return: SkyCoord
        """
        return self._origin

    @origin.setter
    def origin(self, coord):
        """
        Set the origin of the field.
        @param coord: SkyCoord
        @return:
        """
        self._origin = coord

    def set_origin(self, p):
        ra = p[0]
        dec = p[1]
        try:
            self._origin = SkyCoord(ra, dec)
        except Exception as ex:
            logging.error(str(ex))
            self._origin = SkyCoord(ra, dec, unit=('degree', 'degree'))

    @property
    def polygon(self):
        ccds = self.geometry
        p = None
        for ccd in ccds:
            vertices = ((ccd[0], ccd[1]),
                        (ccd[0], ccd[3]),
                        (ccd[2], ccd[3]),
                        (ccd[2], ccd[1]),
                        (ccd[0], ccd[1]))
            if p is None:
                p = Polygon.Polygon(vertices)
            else:
                p.addContour(vertices)
        return p

    @property
    def geometry(self):
        """Return an array of rectangles that represent the 'ra,dec' corners of the FOV
        @rtype: list

        """

        if self.coord is None:
            return None
        ra = self.coord.ra.degree
        dec = self.coord.dec.degree
        ccds = []

        for geo in self._geometry[self.camera]:
            ycen = geo["dec"] + dec
            xcen = geo["ra"] / math.cos(math.radians(ycen)) + ra
            try:
                dy = geo["ddec"]
                dx = geo["dra"] / math.cos(math.radians(ycen))
                ccds.append([xcen - dx / 2.0, ycen - dy / 2.0, xcen + dx / 2.0, ycen + dy / 2.0])
            except:
                rad = geo["rad"]
                ccds.append([xcen, ycen, rad])
        return ccds

    def separation(self, ra, dec):
        """Compute the separation between self and (ra,dec)"""
        if self.coord is None:
            return None
        return self.coord.separation(SkyCoord(ra, dec, unit=('degree', 'degree')))
