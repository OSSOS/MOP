import string, copy, os

import numarray as N
from math import *


# Convenience definitions...
yes = True
no = False

#################
#
#
#               Generic Functions
#
#
#################
def DEGTORAD(deg):
    return (deg * N.pi / 180. )	

def RADTODEG(rad):	
    return (rad * 180. / N.pi )

def DIVMOD(num,val):
    if isinstance(num,N.NumArray):
        # Treat number as numarray object
        _num = N.remainder(num,val)
    else:
        _num = divmod(num,val)[1]
    return _num
					

# History
#
# Hack up of the stsci wcsutil module in pydrizzle
#




__version__ = '0.0.1 (27-Oct-2005)'

def help():
    print 'wcsutil Version '+str(__version__)+':\n'
    print WCSObject.__doc__
#################
#
#
#               Coordinate Transformation Functions
#
#
#################


def ddtohms(xsky,ysky,verbose=no):

    """ Convert sky position(s) from decimal degrees to HMS format."""

    xskyh = xsky /15.
    xskym = (xskyh - N.floor(xskyh)) * 60.
    xskys = (xskym - N.floor(xskym)) * 60.

    yskym = (N.abs(ysky) - N.floor(N.abs(ysky))) * 60.
    yskys = (yskym - N.floor(yskym)) * 60.

    if isinstance(xskyh,N.NumArray):
        rah,dech = [],[]
        for i in xrange(len(xskyh)):
            rastr = repr(int(xskyh[i]))+':'+repr(int(xskym[i]))+':'+repr(xskys[i])
            decstr = repr(int(ysky[i]))+':'+repr(int(yskym[i]))+':'+repr(yskys[i])
            rah.append(rastr)
            dech.append(decstr)
            if verbose:
                print 'RA = ',rastr,', Dec = ',decstr
    else:
        rastr = repr(int(xskyh))+':'+repr(int(xskym))+':'+repr(xskys)
        decstr = repr(int(ysky))+':'+repr(int(yskym))+':'+repr(yskys)
        rah = rastr
        dech = decstr
        if verbose:
            print 'RA = ',rastr,', Dec = ',decstr

    return rah,dech


#################
#
#
#               Coordinate System Class
#
#
#################

class WCSObject:
    """ Easy WCS computer.  Does TAN rd/xy conversion and that's it.


        Syntax:
            wcs = wcsutil.WCSObject(rootname,header=None,shape=None,
                                    pa_key='PA_V3',new=no,prefix=None)
        Parameters:
            rootname: filename in a format supported by IRAF, specifically:
                filename.hhh[group] -or-
                filename.fits[ext] -or-
                filename.fits[extname,extver]
            header:   PyFITS header object from which WCS keywords can be read
            pa_key:   name of keyword to read in telescopy orientation
            new:      specify a new object rather than creating one by
                        reading in keywords from an existing image
            prefix:   string to use as prefix for creating archived versions
                        of WCS keywords, if such keywords do not already exist

            Setting 'new=yes' will create a WCSObject from scratch
                regardless of any input rootname.  This avoids unexpected
                filename collisions.
        Methods:

            xy2rd(pos)                 - compute RA/Dec position for given (x,y) tuple
            rd2xy(skypos,hour=no)      - compute X,Y position for given (RA,Dec)
            copy(deep=True)            - create a copy of the WCSObject.
            help()                     - prints out this help message

    """
    def __init__(self, wcsDict):

        # Initialize wcs dictionaries:
        #   wcsdef - default values for new images
        #   wcstrans - translation table from header keyword to attribute
        #   wcskeys  - keywords in the order they should appear in the header
        self.wcsdef = {'crpix1':0.0,'crpix2':0.0,'crval1':0.0,'crval2':0.0,'cd11':1.0,
                'cd12':1.0,'cd21':1.0,'cd22':1.0,'orient':1.0,'naxis1':0,'naxis2':0,'pscale':1.0,
                'postarg1':0.0,'postarg2':0.0,'pa_obs':0.0,
                'ctype1':'RA---TAN','ctype2':'DEC--TAN'}
        self.wcstrans = {'CRPIX1':'crpix1','CRPIX2':'crpix2','CRVAL1':'crval1','CRVAL2':'crval2',
            'CD1_1':'cd11','CD1_2':'cd12','CD2_1':'cd21','CD2_2':'cd22',
            'ORIENTAT':'orient', 'NAXIS1':'naxis1','NAXIS2':'naxis2',
            'pixel scale':'pscale','CTYPE1':'ctype1','CTYPE2':'ctype2'}
        self.wcskeys = ['NAXIS1','NAXIS2','CRPIX1','CRPIX2',
                        'CRVAL1','CRVAL2','CTYPE1','CTYPE2',
                        'CD1_1','CD1_2','CD2_1','CD2_2',
                        'ORIENTAT']
        # Now, read in the CRPIX1/2, CRVAL1/2, CD1/2_1/2 keywords.
        # Simplistic, but easy to understand what you are asking for.

        # Initialize WCS object with keyword values...
        for key in self.wcskeys:
            self.__dict__[self.wcstrans[key]] = wcsDict.get(key,self.wcsdef[self.wcstrans[key]])

        # attribute to define format for printing WCS
        self.__format__=yes

    # You never know when you want to print out the WCS keywords...
    def __str__(self):
        block = 'WCS Keywords : \n'
        if not self.__format__:
            for key in self.wcstrans.keys():
                _dkey = self.wcstrans[key]
                strn = string.upper(key) + " = " + repr(self.__dict__[_dkey]) + '\n'
                block += strn
            block += 'PA_V3: '+repr(self.pa_obs)+'\n'

        else:
            block += 'CD_11  CD_12: '+repr(self.cd11)+'  '+repr(self.cd12) +'\n'
            block += 'CD_21  CD_22: '+repr(self.cd21)+'  '+repr(self.cd22) +'\n'
            block += 'CRVAL       : '+repr(self.crval1)+'  '+repr(self.crval2) + '\n'
            block += 'CRPIX       : '+repr(self.crpix1)+'  '+repr(self.crpix2) + '\n'
            block += 'NAXIS       : '+repr(int(self.naxis1))+'  '+repr(int(self.naxis2)) + '\n'
            block += 'CTYPE       : '+repr(self.ctype1)+'  '+repr(self.ctype2)+'\n'

        return block

    def __repr__(self):
        return repr(self.__dict__)

    def xy2rd(self,pos):
        """
        This method would apply the WCS keywords to a position to
        generate a new sky position.

        The algorithm comes directly from 'imgtools.xy2rd'

        translate (x,y) to (ra, dec)
        """
        if self.ctype1.find('TAN') < 0 or self.ctype2.find('TAN') < 0:
            print 'XY2RD only supported for TAN projections.'
            raise TypeError

        if isinstance(pos,N.NumArray):
            # If we are working with an array of positions,
            # point to just X and Y values
            posx = pos[:,0]
            posy = pos[:,1]
        else:
            # Otherwise, we are working with a single X,Y tuple
            posx = pos[0]
            posy = pos[1]

        xi = self.cd11 * (posx - self.crpix1) + self.cd12 * (posy - self.crpix2)
        eta = self.cd21 * (posx - self.crpix1) + self.cd22 * (posy - self.crpix2)

        xi = DEGTORAD(xi)
        eta = DEGTORAD(eta)
        ra0 = DEGTORAD(self.crval1)
        dec0 = DEGTORAD(self.crval2)

        ra = N.arctan((xi / (N.cos(dec0)-eta*N.sin(dec0)))) + ra0
        dec = N.arctan( ((eta*N.cos(dec0)+N.sin(dec0)) /
                (N.sqrt((N.cos(dec0)-eta*N.sin(dec0))**2 + xi**2))) )

        ra = RADTODEG(ra)
        dec = RADTODEG(dec)
        ra = DIVMOD(ra, 360.)

        # Otherwise, just return the RA,Dec tuple.
        return ra,dec


    def rd2xy(self,skypos,hour=no):
        """
        This method would use the WCS keywords to compute the XY position
        from a given RA/Dec tuple (in deg).

        NOTE: Investigate how to let this function accept arrays as well
        as single positions. WJH 27Mar03

        """
        if self.ctype1.find('TAN') < 0 or self.ctype2.find('TAN') < 0:
            print 'RD2XY only supported for TAN projections.'
            raise TypeError

        det = self.cd11*self.cd22 - self.cd12*self.cd21

        if det == 0.0:
            raise ArithmeticError,"singular CD matrix!"

        cdinv11 = self.cd22 / det
        cdinv12 = -self.cd12 / det
        cdinv21 = -self.cd21 / det
        cdinv22 = self.cd11 / det

        # translate (ra, dec) to (x, y)

        ra0 = DEGTORAD(self.crval1)
        dec0 = DEGTORAD(self.crval2)
        if hour:
            skypos[0] = skypos[0] * 15.
        ra = DEGTORAD(skypos[0])
        dec = DEGTORAD(skypos[1])

        bottom = float(N.sin(dec)*N.sin(dec0) + N.cos(dec)*N.cos(dec0)*N.cos(ra-ra0))
        if bottom == 0.0:
            raise ArithmeticError,"Unreasonable RA/Dec range!"

        xi = RADTODEG((N.cos(dec) * N.sin(ra-ra0) / bottom))
        eta = RADTODEG((N.sin(dec)*N.cos(dec0) - N.cos(dec)*N.sin(dec0)*N.cos(ra-ra0)) / bottom)

        x = cdinv11 * xi + cdinv12 * eta + self.crpix1
        y = cdinv21 * xi + cdinv22 * eta + self.crpix2

        return x,y

    def recenter(self):
        """
        Reset the reference position values to correspond to the center
        of the reference frame.
        Algorithm used here developed by Colin Cox - 27-Jan-2004.
        """
        if self.ctype1.find('TAN') < 0 or self.ctype2.find('TAN') < 0:
            print 'WCS.recenter() only supported for TAN projections.'
            raise TypeError

        # Check to see if WCS is already centered...
        if self.crpix1 == self.naxis1/2. and self.crpix2 == self.naxis2/2.:
            # No recentering necessary... return without changing WCS.
            return

        # This offset aligns the WCS to the center of the pixel, in accordance
        # with the 'align=center' option used by 'drizzle'.
        #_drz_off = -0.5
        _drz_off = 0.
        _cen = (self.naxis1/2.+ _drz_off,self.naxis2/2. + _drz_off)

        # Compute the RA and Dec for center pixel
        _cenrd = self.xy2rd(_cen)
        _cd = N.array([[self.cd11,self.cd12],[self.cd21,self.cd22]],type=N.Float64)
        _ra0 = DEGTORAD(self.crval1)
        _dec0 = DEGTORAD(self.crval2)
        _ra = DEGTORAD(_cenrd[0])
        _dec = DEGTORAD(_cenrd[1])

        # Set up some terms for use in the final result
        _dx = self.naxis1/2. - self.crpix1
        _dy = self.naxis2/2. - self.crpix2

        _dE,_dN = DEGTORAD(N.dot(_cd,(_dx,_dy)))
        _dE_dN = 1 + N.power(_dE,2) + N.power(_dN,2)
        _cosdec = N.cos(_dec)
        _sindec = N.sin(_dec)
        _cosdec0 = N.cos(_dec0)
        _sindec0 = N.sin(_dec0)

        _n1 = N.power(_cosdec,2) + _dE*_dE + _dN*_dN*N.power(_sindec,2)
        _dra_dE = (_cosdec0 - _dN*_sindec0)/_n1
        _dra_dN = _dE*_sindec0 /_n1

        _ddec_dE = -_dE*N.tan(_dec) / _dE_dN
        _ddec_dN = (1/_cosdec) * ((_cosdec0 / N.sqrt(_dE_dN)) - (_dN*N.sin(_dec) / _dE_dN))

        # Compute new CD matrix values now...
        _cd11n = _cosdec * (self.cd11*_dra_dE + self.cd21 * _dra_dN)
        _cd12n = _cosdec * (self.cd12*_dra_dE + self.cd22 * _dra_dN)
        _cd21n = self.cd11 * _ddec_dE + self.cd21 * _ddec_dN
        _cd22n = self.cd12 * _ddec_dE + self.cd22 * _ddec_dN

        _new_orient = RADTODEG(N.arctan2(_cd12n,_cd22n))

        # Update the values now...
        self.crpix1 = _cen[0]
        self.crpix2 = _cen[1]
        self.crval1 = RADTODEG(_ra)
        self.crval2 = RADTODEG(_dec)

        # Keep the same plate scale, only change the orientation
        self.rotateCD(_new_orient)

        # These would update the CD matrix with the new rotation
        # ALONG with the new plate scale which we do not want.
        self.cd11 = _cd11n
        self.cd12 = _cd12n
        self.cd21 = _cd21n
        self.cd22 = _cd22n

    def _buildNewKeyname(self,key,prepend):
        """ Builds a new keyword based on original keyword name and
            a prepend string.
        """

        if len(prepend+key) <= 8: _new_key = prepend+key
        else: _new_key = str(prepend+key)[:8]

        return _new_key


    def copy(self,deep=yes):
        """ Makes a (deep)copy of this object for use by other objects.
        """
        if deep:
            return copy.deepcopy(self)
        else:
            return copy.copy(self)
    def help(self):
        """ Prints out help message."""
        print 'wcsutil Version '+str(__version__)+':\n'
        print self.__doc__
