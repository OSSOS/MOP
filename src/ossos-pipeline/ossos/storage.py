"""OSSOS VOSpace storage convenience package"""


import subprocess
import os
import vos

_CERTFILE=os.path.join(os.getenv('HOME'),
                       '.ssl',
                       'cadcproxy.pem')

_dbimages='vos:OSSOS/dbimages'


def dbimages_uri(expnum, ccd, version, ext='fits'):
    '''build the uri for an OSSOS image stored in the dbimages containerNode.

    expnum: CFHT exposure number
    ccd: CCD in the mosaic [0-35]
    version: one of p,s,o etc.
    dbimages: dbimages containerNode
    '''

    ## test that the exposure number mades some sense.

    if int(str(expnum)) != expnum or expnum < 1500000 or expnum > 2000000:
        raise ValueError("expnum out of range: %s" % (str(expnum)))

    ## do same for ccd value
    if ccd < 0 or ccd > 35:
        raise ValueError("ccd outside range 0 - 35: %s" % (str(ccd)))

    ccd = str(ccd).zfill(2)
    sexpnum = str(expnum)

    return os.path.join(_dbimages,
                        sexpnum,
                        'ccd%s' % (ccd),
                        '%s%s%s.%s' % (expnum,
                                       version,
                                       ccd,
                                       ext))


def get_image(expnum, ccd, version):
    '''Get a FITS file for this expnum/ccd  from VOSpace.
    
    expnum:  CFHT exposure number (int)
    ccd: CCD in the mosaic (int)
    version:  [p, s, o]  (char)
    dbimages:  VOSpace containerNode

    '''

    uri = dbimages_uri(expnum, ccd, version)
    filename = "%s%s%s.fits" % (str(expnum),
                                version,
                                str(ccd).zfill(2))
    copy(uri, filename)

    return filename


def get_fwhm(expnum, ccd):
    '''Get the FWHM computed for the given expnum/ccd combo.'''

    uri = dbimages_uri(expnum, ccd, 'p', ext='fwhm')
    vospace=vos.Client(certFile=_CERTFILE)
    return float(vospace.open(uri,view='data').read().strip())


def copy(source, dest):
    '''use the vospace service to get a file.

    '''

    vospace = vos.Client(certFile=_CERTFILE)
    return vospace.copy(source, dest)
