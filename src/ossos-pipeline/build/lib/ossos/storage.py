"""OSSOS VOSpace storage convenience package"""


import subprocess
import os
import vos
import logging
import urllib
import errno
from astropy.io import fits

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

def set_tag(expnum, key, value):
    '''Assign a key/value pair tag to the given expnum containerNode'''

    uri = os.path.join(_dbimages, str(expnum))
    vospace = vos.Client(certFile=_CERTFILE)
    node = vospace.getNode(uri)
    node.changeProp(key, value)
    vospace.addProps(node)
    return

def get_tag(expnum, key):
    '''given a key, return the vospace tag value.'''

    uri = os.path.join(_dbimages, str(expnum))
    vospace = vos.Client(certFile=_CERTFILE)
    node = vospace.getNode(uri)
    return node.props.get(key, None)

def get_status(expnum, ccd, program):
    '''Report back status of the given program'''
    key = "%s_%s" % ( program, str(ccd).zfill(2))
    status = get_tag(expnum, key)
    logging.info('%s: %s' %(key, status))
    return status == 'success'

def set_status(expnum, ccd, program, status):
    '''set the processing status of the given program'''
    key = "%s_%s" % ( program, str(ccd).zfill(2))
    return set_tag(expnum, key, status)

def get_image(expnum, ccd=None, version='p', ext='fits'):
    '''Get a FITS file for this expnum/ccd  from VOSpace.
    
    expnum:  CFHT exposure number (int)
    ccd: CCD in the mosaic (int)
    version:  [p, s, o]  (char)
    dbimages:  VOSpace containerNode

    '''

    uri = dbimages_uri(expnum, ccd, version, ext=ext)
    filename = os.path.basename(uri)
    try:
        copy(uri, filename)
    except Exception as e:
        if e.errno != errno.ENOENT or ccd is None:
            raise e
        ## try doing a cutout from MEF in VOSpace
        uri = dbimages_uri(expnum, version=version, ext=ext)
        cutout = urllib.urlencode({'cutout':'[%d]' % (int(ccd)+1)})
        vospace=vos.Client(certFile=_CERTFILE)
        url = vospace.getNodeURL(uri,view='cutout', limit=None)+'&'+cutout
        fin = vospace.open(uri, URL=url)
        fout = open(filename, 'w')
        buff = fin.read(2**16)
        while len(buff)>0:
            fout.write(buff)
            buff = fin.read(2**16)
        fout.close()
        fin.close()
        hdu_list = fits.open(filename,'update',do_not_scale_image_data=True)
        hdu_list[0].header['BZERO']=hdu_list[0].header.get('BZERO',32768)
        hdu_list[0].header['BSCALE']=hdu_list[0].header.get('BSCALE',1)
        hdu_list.flush()
        hdu_list.close()
        
    return filename


def get_fwhm(expnum, ccd):
    '''Get the FWHM computed for the given expnum/ccd combo.'''

    uri = dbimages_uri(expnum, ccd, 'p', ext='fwhm')
    vospace=vos.Client(certFile=_CERTFILE)
    return float(vospace.open(uri,view='data').read().strip())

def mkdir(root):
    '''make directory tree in vospace.'''
    dir_list = []
    vospace = vos.Client(certFile=_CERTFILE)
    while not vospace.isdir(root):
        dir_list.append(root)
        root = os.path.dirname(root)
    while len(dir_list)>0:
        logging.info("Creating directory: %s" % (dir_list[-1]))
        vospace.mkdir(dir_list.pop())
    return 

def copy(source, dest):
    '''use the vospace service to get a file.

    '''

    vospace = vos.Client(certFile=_CERTFILE)
    logging.info("%s -> %s" % ( source, dest))

    return vospace.copy(source, dest)
