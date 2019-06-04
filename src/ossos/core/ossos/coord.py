#!/usr/bin/python
import sys
import math
import string

D2PI = 6.2831853071795864769252867665590057683943387987502
DS2R = 7.2722052166430399038487115353692196393452995355905e-5
MJD0 = 51544.5
DPY = 365.25


def mjd2gmst(mjd):
    """Convert Modfied Juian Date (JD = 2400000.5) to GMST
    
    Taken from P.T. Walace routines.

    """

    tu = (mjd - MJD0) / (100*DPY)

    st = math.fmod(mjd, 1.0) * D2PI + (24110.54841 + (8640184.812866 + (0.093104 - 6.2e-6 * tu) * tu) * tu) * DS2R

    w = math.fmod(st, D2PI)
    if w >= 0.0:
        return w
    else:
        return w + D2PI


class Coord:
    """A class to manipulate astronomical coordinates.

    """

    def __init__(self, xxx_todo_changeme, system='ICRS'):
        """Assign the RA/DEC of the coordinate (initialization).

        """
        (l, b) = xxx_todo_changeme
        self.system = system
        if system == 'ecliptic':
            self.el = l
            self.eb = b
            self.ec2eq()
        elif system == 'ICRS':
            self.ra = l
            self.dec = b
            self.eq2ec()
        else:
            sys.stderr.write("Don't know this system: {}".format(system))
            sys.stderr.write("Use ICRS or ecliptic")
            sys.exit(-1)

    def set_system(self, system):
        self.system = system

    def zd(self, phi):
        """Compute the Zennith Distance of this coord at given OBS.

        """

        return math.sin(phi)

    def __segs__(self, ang, precision=1):
        """given an angle, convert it to a segisdecimal string"""
        sign = "+"
        if ang < 0:
            sign = "-"
            ang = ang * -1.0000000000000
        ang += 1E-11

        _d = math.floor(ang)
        _mf = (ang - _d) * 60.00
        _m = math.floor(_mf)
        _s = (_mf - _m) * 60.00
        _is = math.floor(_s)
        _fs = math.floor((_s - _is) * 10.00 * precision)

        _d = "%.0f" % ( _d)
        _m = "%.0f" % ( _m)
        _is = "%.0f" % (_is)
        _fs = int(_fs)

        _sd = string.zfill(_d, 2)
        _sm = string.zfill(_m, 2)
        _ss = string.zfill(_is, 2)
        _sf = string.zfill(_fs, precision)

        s = sign + "%s:%s:%s.%s" % ( _sd, _sm, _ss, _sf)
        return s

    def __str__(self):
        """String representation of coordinate.

        """

        (l, b) = (0, 0)
        if self.system == 'ICRS':
            (l, b) = (self.ra, self.dec)
        elif self.system == 'ecliptic':
            (l, b) = (self.el, self.eb)
        l = math.degrees(l / 15.E0)
        b = math.degrees(b)
        return self.__segs__(l, 2) + " " + self.__segs__(b, 1)

    def ec2eq(self):
        """Convert ecliptic coordinates to equatorial coordinates"""

        sin = math.sin
        cos = math.cos
        arcsin = math.asin
        arccos = math.acos

        eb = self.eb
        el = self.el
        ob = math.radians(23.439281)

        dec = arcsin(sin(eb) * cos(ob) + cos(eb) * sin(ob) * sin(el))
        sra = (sin(dec) * cos(ob) - sin(eb)) / (cos(dec) * sin(ob))

        cra = cos(el) * cos(eb) / cos(dec)
        if sra < 1 and sra > -1:
            sa = arcsin(sra)
        else:
            sa = 0
        ca = arccos(cra)
        tsa = sa
        tca = ca
        if tsa < 0:
            ca = 2.0 * math.pi - ca
        if tca >= math.pi / 2.0:
            sa = math.pi - sa
        if ca >= math.pi * 2.0:
            ca = ca - math.pi * 2.0
        self.tsa = sra
        self.tca = cra
        self.ra = ca
        self.dec = dec

    def eq2ec(self):

        sin = math.sin
        cos = math.cos
        arcsin = math.asin
        arccos = math.acos

        ra = self.ra
        dec = self.dec
        ob = math.radians(23.439281000000000000)

        eb = arcsin(sin(dec) * cos(ob) - cos(dec) * sin(ra) * sin(ob))
        sl = (sin(dec) - sin(eb) * cos(ob)) / (cos(eb) * sin(ob))
        cl = cos(ra) * cos(dec) / cos(eb)
        if sl < -1: sl = float(-1)
        if sl > 1: sl = float(1)
        els = arcsin(sl)
        elc = arccos(cl)
        ela = 0
        elb = 0
        if sl < 0 and cl < 0:
            ela = math.pi * 1 - els
            elb = 2.0 * math.pi - elc
            rule = 0
        if sl < 0 and cl > 0:
            ela = 2.0 * math.pi + els
            elb = 2.0 * math.pi - elc
            rule = 1
        if sl > 0 and cl > 0:
            ela = els
            elb = elc
            rule = 2
        if sl > 0 and cl < 0:
            ela = math.pi - els
            elb = elc
            rule = 3

        self.sl = els
        self.cl = elc
        self.eb = eb
        self.el = ela


if __name__ == '__main__':

    c = 0
    for a in range(0, 360, 9):
        j = Coord((math.radians(a), math.radians(33.0)))
        k = Coord((j.el, j.eb), system='ecliptic')
        k.set_system('ICRS')
        if str(j) != str(k):
            c = c + 1

    if c == 0:
        print("Passed")
