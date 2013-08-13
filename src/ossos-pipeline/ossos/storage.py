"""OSSOS VOSpace storage convenience package"""


import subprocess
import os
import tempfile
import vos
import logging
import urllib
import errno
from astropy.io import fits
import urlparse

from ossos import coding

CERTFILE=os.path.join(os.getenv('HOME'),
                      '.ssl',
                      'cadcproxy.pem')

DBIMAGES='vos:OSSOS/dbimages'
MEASURE3='vos:OSSOS/measure3'

DATA_WEB_SERVICE='https://www.canfar.phys.uvic.ca/data/pub/'

OSSOS_TAG_URI_BASE='ivo://canfar.uvic.ca/ossos'
OBJECT_COUNT = "object_count"

vospace = vos.Client(cadc_short_cut=True, certFile=CERTFILE)

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
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    try: 
        c.link(data_source, data_dest)
    except IOError as e:
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
    logging.debug("got uri: "+uri)
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

    if tag_uri(key) not in get_tags(expnum):
        get_tags(expnum, force=True)
    logging.debug("%s # %s -> %s"  % (
        expnum, tag_uri(key), get_tags(expnum).get(tag_uri(key), None)))
    return get_tags(expnum).get(tag_uri(key), None)

def get_process_tag(program, ccd):
    """make a process tag have a suffix indicating which ccd its for"""
    return "%s_%s" % ( program, str(ccd).zfill(2))

def get_tags(expnum, force=False):
    uri = os.path.join(DBIMAGES,str(expnum))
    return vospace.getNode(uri, force=force).props

def get_status(expnum, ccd, program, return_message=False):
    '''Report back status of the given program'''
    key = get_process_tag(program, ccd)
    status = get_tag(expnum, key)
    logging.debug('%s: %s' %(key, status))
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
        logging.debug("File already on disk: %s" % ( filename))
        return filename

    try:
        logging.debug("trying to get file %s" % ( uri))
        copy(uri, filename)
    except Exception as e:
        logging.debug(str(e))
        if getattr(e,'errno',0) != errno.ENOENT or ccd is None:
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

        logging.debug(uri)
        url = vospace.getNodeURL(uri,view='cutout', limit=None, cutout=cutout)
        logging.debug(url)
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
        logging.debug("Creating directory: %s" % (dir_list[-1]))
        vospace.mkdir(dir_list.pop())
    return


def vofile(filename, mode):
    """Open and return a handle on a VOSpace data connection"""
    return vospace.open(filename, view='data', mode=mode)


def open_vos_or_local(path, mode="rb"):
    """
    Opens a file which can either be in VOSpace or the local filesystem.
    """
    if path.startswith("vos:"):
        primary_mode = mode[0]
        if primary_mode == "r":
            vofile_mode = os.O_RDONLY
        elif primary_mode == "w":
            vofile_mode = os.O_WRONLY
        elif primary_mode == "a":
            vofile_mode = os.O_APPEND
        else:
            raise ValueError("Can't open with mode %s" % mode)

        return vofile(path, vofile_mode)
    else:
        return open(path, mode)


def copy(source, dest):
    '''use the vospace service to get a file.

    '''


    logging.debug("%s -> %s" % ( source, dest))

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


def listdir(directory, force=False):
    return vospace.listdir(directory, force=force)


def list_dbimages():
    return listdir(DBIMAGES)


def exists(uri):
    return vospace.access(uri)


def move(old_uri, new_uri):
    vospace.move(old_uri, new_uri)


def delete_uri(uri):
    vospace.delete(uri)


def has_property(node_uri, property_name, ossos_base=True):
    """
    Checks if a node in VOSpace has the specified property.
    """
    if get_property(node_uri, property_name, ossos_base) is None:
        return False
    else:
        return True


def get_property(node_uri, property_name, ossos_base=True):
    """
    Retrieves the value associated with a property on a node in VOSpace.
    """
    # Must use force or we could have a cached copy of the node from before
    # properties of interest were set/updated.
    node = vospace.getNode(node_uri, force=True)
    property_uri = tag_uri(property_name) if ossos_base else property_name

    if property_uri not in node.props:
        return None

    return node.props[property_uri]


def set_property(node_uri, property_name, property_value, ossos_base=True):
    """
    Sets the value of a property on a node in VOSpace.  If the property
    already has a value then it is first cleared and then set.
    """
    node = vospace.getNode(node_uri)
    property_uri = tag_uri(property_name) if ossos_base else property_name

    # If there is an existing value, clear it first
    if property_uri in node.props:
        node.props[property_uri] = None
        vospace.addProps(node)

    node.props[property_uri] = property_value
    vospace.addProps(node)


def build_counter_tag(epoch_field, dryrun=False):
    """
    Builds the tag for the counter of a given epoch/field,
    without the OSSOS base.
    """
    tag = epoch_field + "-" + OBJECT_COUNT

    if dryrun:
        tag += "-DRYRUN"

    return tag


def read_object_counter(node_uri, epoch_field, dryrun=False):
    """
    Reads the object counter for the given epoch/field on the specified
    node.

    Returns:
      count: str
        The current object count.
    """
    return get_property(node_uri, build_counter_tag(epoch_field, dryrun),
                        ossos_base=True)


def increment_object_counter(node_uri, epoch_field, dryrun=False):
    """
    Increments the object counter for the given epoch/field on the specified
    node.

    Returns:
      new_count: str
        The object count AFTER incrementing.
    """
    current_count = read_object_counter(node_uri, epoch_field, dryrun=dryrun)

    if current_count is None:
        new_count = "01"
    else:
        new_count = coding.base36encode(coding.base36decode(current_count) + 1,
                                        pad_length=2)

    set_property(node_uri,
                 build_counter_tag(epoch_field, dryrun=dryrun),
                 new_count,
                 ossos_base=True)

    return new_count
