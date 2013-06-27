"""OSSOS VOSpace storage convenience package"""


import subprocess
import os
import vos
import logging
import urllib
import errno
from astropy.io import fits
import urlparse

CERTFILE=os.path.join(os.getenv('HOME'),
                      '.ssl',
                      'cadcproxy.pem')

DBIMAGES='vos:OSSOS/dbimages'
DATA_WEB_SERVICE='https://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/'
OSSOS_TAG_URI_BASE='ivo://canfar.uvic.ca/ossos'
vospace = vos.Client(certFile=CERTFILE)
SUCCESS = 'success' 

def populate(dataset_name,
             data_web_service_url = DATA_WEB_SERVICE+"CFHT"):

    """Given a dataset_name created the desired dbimages directories
    and links to the raw data files stored at CADC."""

    data_dest = get_uri(dataset_name,version='o',ext='fits.fz')
    data_source = "%s/%so.fits.fz" % (data_web_service_url,dataset_name)

    c = vospace

    try:
        c.mkdir(os.path.dirname(data_dest))
    except IOError as e:
        print e
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    try: 
        c.link(data_source, data_dest)
    except IOError as e:
        print e
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    header_dest = get_uri(dataset_name,version='o',ext='head')
    header_source = "%s/%so.fits.fz?cutout=[0]" % (
        data_web_service_url, dataset_name) 
    try:
        c.link(header_source, header_dest)
    except IOError as e:
        print e
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    return True
        


def get_uri(expnum, ccd=None,
            version='p', ext='fits',
            subdir=None, prefix=None):
    """
    Build the uri for an OSSOS image stored in the dbimages
    containerNode.

    expnum: CFHT exposure number
    ccd: CCD in the mosaic [0-35]
    version: one of p,s,o etc.
    dbimages: dbimages containerNode.
    """
    if subdir is None:
        subdir = str(expnum)
    if prefix is None:
        prefix = ''
    uri = os.path.join(DBIMAGES, subdir)

    if ext is None:
        ext = ''
    elif len(ext) > 0 and ext[0] != '.':
        ext = '.'+ext

    # if ccd is None then we send uri for the MEF
    if ccd is not None:
        ccd = str(ccd).zfill(2)
        uri = os.path.join(uri,
                           'ccd%s' % (ccd),
                           '%s%s%s%s%s' % (prefix, str(expnum),
                                            version,
                                            ccd,
                                            ext))
    else:
        uri = os.path.join(uri,
                           '%s%s%s%s' % (prefix, str(expnum),
                                          version,
                                          ext))
    logging.info("got uri: "+uri)
    return uri

dbimages_uri = get_uri



def set_tag(expnum, key, value):
    '''Assign a key/value pair tag to the given expnum containerNode'''

    uri = os.path.join(DBIMAGES, str(expnum))
    node = vospace.getNode(uri)
    uri = tag_uri(key)
    # for now we delete and then set, some issue with the props
    # updating on vospace (2013/06/23, JJK)
    node.props[uri] = None
    vospace.addProps(node)
    node.props[uri] = value
    return vospace.addProps(node)

def tag_uri(key):
    """Build the uri for a given tag key. 

    key: (str) eg 'mkpsf_00'

    """
    if OSSOS_TAG_URI_BASE in key:
        return key
    return OSSOS_TAG_URI_BASE+"#"+key.strip()


def get_tag(expnum, key):
    '''given a key, return the vospace tag value.'''

    uri = os.path.join(DBIMAGES, str(expnum))

    node = vospace.getNode(uri)
    if tag_uri(key) not in node.props.keys():
        node = vospace.getNode(uri, force=True)
    logging.debug("%s # %s -> %s"  % ( uri, tag_uri(key), node.props.get(tag_uri(key), None)))
    return node.props.get(tag_uri(key), None)

def get_process_tag(program, ccd):
    """make a process tag have a suffix indicating which ccd its for"""
    return "%s_%s" % ( program, str(ccd).zfill(2))

def get_status(expnum, ccd, program, return_message=False):
    '''Report back status of the given program'''
    key = get_process_tag(program, ccd)
    status = get_tag(expnum, key)
    logging.info('%s: %s' %(key, status))
    if return_message:
        return status
    else:
        return status == SUCCESS

def set_status(expnum, ccd, program, status):
    '''set the processing status of the given program'''
    
    return set_tag(expnum, get_process_tag(program,ccd), status)

def get_image(expnum, ccd=None, version='p', ext='fits',
              subdir=None, rescale=True, prefix=None):
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
        logging.info("trying to get file %s" % ( uri))
        copy(uri, filename)
    except Exception as e:
        logging.info(str(e))
        if e.errno != errno.ENOENT or ccd is None:
            raise e
        ## try doing a cutout from MEF in VOSpace
        uri = get_uri(expnum,
                      version=version,
                      ext=ext,
                      subdir=subdir)
        logging.info("Using uri: %s" % ( uri))
        cutout="[%d]" % ( int(ccd)+1)
        if ccd < 18 :
            cutout += "[-*,-*]"

        logging.info(uri)
        url = vospace.getNodeURL(uri,view='cutout', limit=None, cutout=cutout)
        logging.info(url)
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

    return float(vospace.open(uri,view='data').read().strip())

def get_zeropoint(expnum, ccd, prefix=None, version='p'):
    '''Get the zeropoint for this exposure'''

    return float(vospace.
                 open(
        uri=get_uri(expnum, ccd, version, ext='zeropoint.used', prefix=prefix),
        view='data').
                 read().
                 strip()
                 )

def mkdir(root):
    '''make directory tree in vospace.'''
    dir_list = []

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


    logging.info("%s -> %s" % ( source, dest))

    return vospace.copy(source, dest)

def vlink(s_expnum, s_ccd, s_version, s_ext,
          l_expnum, l_ccd, l_version, l_ext, s_prefix=None, l_prefix=None):
    '''make a link between two version of a file'''
    source_uri = get_uri(s_expnum, ccd=s_ccd, version=s_version, ext=s_ext, prefix=s_prefix)
    link_uri = get_uri(l_expnum, ccd=l_ccd, version=l_version, ext=s_ext, prefix=l_prefix)

    return vospace.link(source_uri, link_uri)

def delete(expnum, ccd, version, ext, prefix=None):
    '''delete a file, no error on does not exist'''
    uri = get_uri(expnum, ccd=ccd, version=version, ext=ext, prefix=prefix)

    try:
        vospace.delete(uri)
    except IOError as e:
        if e.errno != errno.ENOENT:
            raise e
        

def list_dbimages():
    return vospace.listdir(DBIMAGES)
