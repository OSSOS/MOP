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

def get_uri(expnum, ccd=None, version='p', ext='fits', subdir=None, prefix=None):
    '''build the uri for an OSSOS image stored in the dbimages containerNode.

    expnum: CFHT exposure number
    ccd: CCD in the mosaic [0-35]
    version: one of p,s,o etc.
    dbimages: dbimages containerNode
    '''
    if subdir is None:
        subdir = str(expnum)
    if prefix is None:
        prefix = ''
    uri = os.path.join(_dbimages, subdir)


    # if ccd is None then we send uri for the MEF
    if ccd is not None:
        ccd = str(ccd).zfill(2)
        uri = os.path.join(uri,
                           'ccd%s' % (ccd),
                           '%s%s%s%s.%s' % (prefix, str(expnum),
                                          version,
                                          ccd,
                                          ext))
    else:
        uri = os.path.join(uri,
                           '%s%s%s.%s' % (prefix, str(expnum),
                                        version,
                                        ext))
    return uri

dbimages_uri = get_uri



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

def get_image(expnum, ccd=None, version='p', ext='fits', subdir=None, rescale=True, prefix=None):
    '''Get a FITS file for this expnum/ccd  from VOSpace.
    
    expnum:  CFHT exposure number (int)
    ccd: CCD in the mosaic (int)
    version:  [p, s, o]  (char)
    dbimages:  VOSpace containerNode

    '''

    if not subdir:
        subdir = str(expnum)

    uri = get_uri(expnum, ccd, version, ext=ext, subdir=subdir, prefix=prefix)
    filename = os.path.basename(uri)
    
    if os.access(filename, os.F_OK):
        logging.info("File already on disk: %s" % ( filename))
        return filename

    try:
        copy(uri, filename)
    except Exception as e:
        if e.errno != errno.ENOENT or ccd is None:
            raise e
        ## try doing a cutout from MEF in VOSpace
        uri = get_uri(expnum,
                      version=version,
                      ext=ext,
                      subdir=subdir)
        logging.debug("Using uri: %s" % ( uri))
        cutout="[%d]" % ( int(ccd)+1)
        if ccd < 18 :
            cutout += "[-*,-*]"
        cutout = urllib.urlencode({'cutout': cutout})
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
        if not rescale:
            return filename
        hdu_list = fits.open(filename,'update',do_not_scale_image_data=True)
        hdu_list[0].header['BZERO']=hdu_list[0].header.get('BZERO',32768)
        hdu_list[0].header['BSCALE']=hdu_list[0].header.get('BSCALE',1)
        hdu_list.flush()
        hdu_list.close()
        
    return filename


def get_fwhm(expnum, ccd, prefix=None, version='p'):
    '''Get the FWHM computed for the given expnum/ccd combo.'''

    uri = get_uri(expnum, ccd, version, ext='fwhm', prefix=prefix)
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

def vlink(s_expnum, s_ccd, s_version, s_ext,
          l_expnum, l_ccd, l_version, l_ext, s_prefix=None, l_prefix=None):
    '''make a link between two version of a file'''
    source_uri = get_uri(s_expnum, ccd=s_ccd, version=s_version, ext=s_ext, prefix=s_prefix)
    link_uri = get_uri(l_expnum, ccd=l_ccd, version=l_version, ext=s_ext, prefix=l_prefix)
    vospace = vos.Client(certFile=_CERTFILE)
    return vospace.link(source_uri, link_uri)

def delete(expnum, ccd, version, ext, prefix=None):
    '''delete a file, no error on does not exist'''
    uri = get_uri(expnum, ccd=ccd, version=version, ext=ext, prefix=prefix)
    vospace = vos.Client(certFile=_CERTFILE)
    try:
        vospace.delete(uri)
    except IOError as e:
        if e.errno != errno.ENOENT:
            raise e
        
