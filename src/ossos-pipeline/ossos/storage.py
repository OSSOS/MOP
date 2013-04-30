"""OSSOS VOSpace storage convenience package"""


import subprocess
import os
import vos
import logging

_CERTFILE=os.path.join(os.getenv('HOME'),
                       '.ssl',
                       'cadcproxy.pem')

_dbimages='vos:OSSOS/dbimages'

def dbimages_uri(expnum, ccd=None, version='p', ext='fits'):
    '''build the uri for an OSSOS image stored in the dbimages containerNode.

    expnum: CFHT exposure number
    ccd: CCD in the mosaic [0-35]
    version: one of p,s,o etc.
    dbimages: dbimages containerNode
    '''

    sexpnum = str(expnum)
    uri = os.path.join(_dbimages, sexpnum)

    # if ccd is None then we send uri for the MEF
    if ccd is not None:
        ccd = str(ccd).zfill(2)
        uri = os.path.join(uri,
                           'ccd%s' % (ccd),
                           '%s%s%s.%s' % (sexpnum,
                                          version,
                                          ccd,
                                          ext))
    else:
        uri = os.path.join(uri,
                           '%s%s.%s' % (sexpnum,
                                        version,
                                        ext))
    return uri

def get_image(expnum, ccd=None, version='p', ext='fits'):
    '''Get a FITS file for this expnum/ccd  from VOSpace.
    
    expnum:  CFHT exposure number (int)
    ccd: CCD in the mosaic (int)
    version:  [p, s, o]  (char)
    dbimages:  VOSpace containerNode

    '''

    uri = dbimages_uri(expnum, ccd, version, ext=ext)
    filename = os.path.basename(uri)
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
    logging.info("%s -> %s" % ( source, dest))
    return vospace.copy(source, dest)
