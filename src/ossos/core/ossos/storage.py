"""OSSOS VOSpace storage convenience package"""
from six import StringIO, BytesIO
import errno
import fnmatch
from glob import glob
import os
import re
import tempfile
import logging
import warnings
import time
from astropy.coordinates import SkyCoord
from astropy.io import ascii
from astropy import units
from astropy.units import Quantity
from astropy.utils.exceptions import AstropyUserWarning
from astropy.io import fits
import requests as requests_module
from .downloads.cutouts.calculator import CoordinateConverter
from . import coding
from . import util
from astropy.time import Time
from .gui import logger
from .wcs import WCS
import vos

client = vos.Client()
# from .gui.errorhandling import DownloadErrorHandler

# Try and turn off warnings, only works for some releases of requests.

MAXCOUNT = 30000
_TARGET = "TARGET"

DBIMAGES = 'vos:OSSOS/dbimages'
MEASURE3 = 'vos:OSSOS/measure3'
POSTAGE_STAMPS = 'vos:OSSOS/postage_stamps'
TRIPLETS = 'vos:OSSOS/triplets'
ASTROM_RELEASES = 'vos:OSSOS/0_OSSOSreleases'

DATA_WEB_SERVICE = 'https://www.canfar.phys.uvic.ca/data/pub/'
VOSPACE_WEB_SERVICE = 'https://www.canfar.phys.uvic.ca/vospace/nodes/'
TAP_WEB_SERVICE = 'http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/tap/sync'

OSSOS_TAG_URI_BASE = 'ivo://canfar.uvic.ca/ossos'
OBJECT_COUNT = "object_count"


class Wrapper(object):

    def __init__(self, wrapped_class, *args, **kargs):
        self.wrapped_class = wrapped_class(*args, **kargs)

    def __getattr__(self, attr):
        orig_attr = self.wrapped_class.__getattribute__(attr)
        if callable(orig_attr):
            def hooked(*args, **kwargs):
                print(("-->", orig_attr, args, kwargs))
                result = orig_attr(*args, **kwargs)
                print(("<--", type(result)))
                return result
            return hooked
        else:
            return orig_attr

SUCCESS = 'success'

# cache holders.
mopheaders = {}
astheaders = {}
sgheaders = {}
fwhm = {}
zmag = {}
tags = {}

APCOR_EXT = "apcor"
ZEROPOINT_USED_EXT = "zeropoint.used"
PSF_EXT = "psf.fits"
FITS_EXT = ".fits.fz"
FITS_EXT = ".fits"


class MyRequests(object):

    def __init__(self):

        self.requests = requests_module

    def get(self, *args, **kwargs):
        resp = self.requests.get(*args, **kwargs)
        resp.raise_for_status()
        return resp


requests = MyRequests()


def get_ccdlist(expnum):
    if int(expnum) < 1785619:
        # Last exposures with 36 CCD Megaprime
        ccdlist = list(range(0, 36))
    else:
        # First exposrues with 40 CCD Megaprime
        ccdlist = list(range(0, 40))
    return ccdlist


def get_apcor(expnum, ccd, version='p', prefix=None):
    """
    retrieve the aperture correction for this exposure
    @param expnum:
    @param ccd:
    @param version:
    @param prefix:
    @return:
    """
    uri = get_uri(expnum, ccd, ext=APCOR_EXT, version=version, prefix=prefix)
    apcor_file_name = tempfile.NamedTemporaryFile()
    client.copy(uri, apcor_file_name.name)
    apcor_file_name.seek(0)
    return [float(x) for x in apcor_file_name.readline().split()]


def get_detections(release='current'):
    """

    @param release: the release to retrieve from VOSpace
    @return: astropy.table.Table
    """
    detection_path = "{}/{}/OSSOS*.detections".format(ASTROM_RELEASES, release)
    filenames = client.glob(detection_path)
    if not len(filenames) > 0:
        raise IOError("No detection file found using: {}".format(detection_path))

    return ascii.read(open_vos_or_local(filenames[0]).read(), header_start=-1)


def cone_search(ra, dec, dra=0.01, ddec=0.01, mjdate=None, calibration_level=2, use_ssos=True,
                collection='CFHTMEGAPIPE'):
    """Do a QUERY on the TAP service for all observations that are part of OSSOS (*P05/*P016)
    where taken after mjd and have calibration 'observable'.

    @param ra: RA center of search cont
    @type ra: Quantity
    @param dec: float degrees
    @type dec: Quantity
    @param dra: float degrees
    @type dra: Quantity
    @param ddec: float degrees
    @type ddec: Quantity
    @param calibration_level: What calibration level must the found image have,
    @param mjdate: what data must the observation be to
    @param collection: name of the data collection to be searched.
    @param use_ssos: USE the SSOIS server to find comparison?  False -> Use CAOM2 TAP query.
    ke on.
    this is a CAOM2 parameter of CADC, 2 means calibrated data.
    """

    if use_ssos:
        ssois_server = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/ssos/fixedssos.pl"
        params = dict(pos="{0:f},{1:f}".format(ra.to(units.degree).value, dec.to(units.degree).value))
        result = requests.get(ssois_server, params=params)
        table = ascii.read(result.text, format='tab')
        table = table[table['Telescope/Instrument'] == 'CFHT/MegaCam']
        column_name_mapping = {'Image': 'collectionID',
                               'Filter': 'filter',
                               'Exptime': 'exptime'}
        # rename the columns
        for key in column_name_mapping:
            table[key].name = column_name_mapping[key]
        table['collectionID'] = [x[:-1] for x in table['collectionID']]
        # compute the mjdate from the time string.
        table['mjdate'] = Time(table['Date/Time']).mjd
        return table

    data = dict(QUERY=(" SELECT Observation.observationID as collectionID, "
                       " Plane.time_bounds_lower AS mjdate, "
                       " Plane.time_exposure AS exptime, "
                       " Plane.energy_bandpassName as filter"
                       " FROM caom2.Observation AS Observation "
                       " JOIN caom2.Plane AS Plane "
                       " ON Observation.obsID = Plane.obsID "
                       " WHERE  ( Observation.collection = '{}' ) "
                       " AND Plane.calibrationLevel > {} "
                       " AND ( Plane.energy_bandpassName LIKE 'r.%' OR Plane.energy_bandpassName LIKE 'gri.%' ) "
                       " AND ( Observation.proposal_id LIKE '%P05' or Observation.proposal_id LIKE '%P06' )"
                       " AND Observation.target_name NOT LIKE 'WP%'"),
                REQUEST="doQuery",
                LANG="ADQL",
                FORMAT="tsv")

    data["QUERY"] = data["QUERY"].format(calibration_level, collection)
    data["QUERY"] += (" AND  "
                      " CONTAINS( BOX('ICRS', {}, {}, {}, {}), "
                      " Plane.position_bounds ) = 1 ").format(ra.to(units.degree).value, dec.to(units.degree).value,
                                                              dra.to(units.degree).value, ddec.to(units.degree).value)
    if mjdate is not None:
        data["QUERY"] += " AND Plane.time_bounds_lower < {} AND Plane.time_bounds_cval2 > {} ".format(
            mjdate + 1.0 / 24.0,
            mjdate - 1 / 24.0)

    result = requests.get(TAP_WEB_SERVICE, params=data, verify=False)

    logger.debug("Doing TAP Query using url: %s" % (str(result.url)))

    table_reader = ascii.get_reader(Reader=ascii.Basic)
    table_reader.header.splitter.delimiter = '\t'
    table_reader.data.splitter.delimiter = '\t'
    table = table_reader.read(result.text)

    logger.debug(str(table))
    return table


def populate(dataset_name, data_web_service_url=DATA_WEB_SERVICE + "CFHT"):
    """Given a dataset_name created the desired dbimages directories
    and links to the raw data files stored at CADC.
    @param dataset_name: the name of the CFHT dataset to make a link to.
    @param data_web_servica_url: the URL of the data web service run by CADC.
    """

    data_dest = get_uri(dataset_name, version='o', ext=FITS_EXT)
    data_source = "%s/%so.{}" % (data_web_service_url, dataset_name, FITS_EXT)

    mkdir(os.path.dirname(data_dest))

    try:
        client.link(data_source, data_dest)
    except IOError as e:
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    header_dest = get_uri(dataset_name, version='o', ext='head')
    header_source = "%s/%so.fits.fz?cutout=[0]" % (
        data_web_service_url, dataset_name)
    try:
        client.link(header_source, header_dest)
    except IOError as e:
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    header_dest = get_uri(dataset_name, version='p', ext='head')
    header_source = "%s/%s/%sp.head" % (
        'http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub', 'CFHTSG', dataset_name)
    try:
        client.link(header_source, header_dest)
    except IOError as e:
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    return True


def get_cands_uri(field, ccd, version='p', ext='measure3.cands.astrom', prefix=None,
                  block=None):
    """
        return the nominal URI for a candidate file.

    @param field: the OSSOS field name
    @param ccd: which CCD are the candidates on
    @param version: either the 'p', or 's' (scrambled) candidates.
    @param ext: Perhaps we'll change this one day.
    @param prefix: if this is a 'fake' dataset then add 'fk'
    @param block: Which BLOCK of the field are we looking at? eg. 15BS+1+1
    @return:
    """

    if prefix is None:
        prefix = ""
    if len(prefix) > 0:
        prefix += "_"
    if len(field) > 0:
        field += "_"

    if ext is None:
        ext = ""
    if len(ext) > 0 and ext[0] != ".":
        ext = ".{}".format(ext)

    measure3_dir = MEASURE3
    if block is not None:
        measure3_dir + "/{}".format(block)
    return "{}/{}{}{}{}{}".format(measure3_dir, prefix, field, version, ccd, ext)


def get_uri(expnum, ccd=None,
            version='p', ext=FITS_EXT,
            subdir=None, prefix=None):
    """
    Build the uri for an OSSOS image stored in the dbimages
    containerNode.

    :rtype : basestring
    expnum: CFHT exposure number
    ccd: CCD in the mosaic [0-35]
    version: one of p,s,o etc.
    dbimages: dbimages containerNode.
    @type subdir: str
    @param expnum: int
    @param ccd:
    @param version:
    @param ext:
    @param subdir:
    @param prefix:
    """
    if subdir is None:
        subdir = str(expnum)
    if prefix is None:
        prefix = ''
    uri = os.path.join(DBIMAGES, subdir)

    if ext is None:
        ext = ''
    elif len(ext) > 0 and ext[0] != '.':
        ext = '.' + ext

    if version is None:
        version = ''

    if ccd is None:
        uri = os.path.join(uri,
                           '%s%s%s%s' % (prefix, str(expnum),
                                         version,
                                         ext))
    else:
        ccd = str(ccd).zfill(2)
        uri = os.path.join(uri,
                           'ccd{}'.format(ccd),
                           '%s%s%s%s%s' % (prefix, str(expnum),
                                           version,
                                           ccd,
                                           ext))

    return uri


dbimages_uri = get_uri


def set_tags_on_uri(uri, keys, values=None):
    node = client.get_node(uri)
    if values is None:
        values = []
        for idx in range(len(keys)):
            values.append(None)
    assert (len(values) == len(keys))
    for idx in range(len(keys)):
        key = keys[idx]
        value = values[idx]
        tag = tag_uri(key)
        node.props[tag] = value
    client.add_props(node)
    return client.get_node(uri, force=True)


def _set_tags(expnum, keys, values=None):
    uri = os.path.join(DBIMAGES, str(expnum))
    node = client.get_node(uri, force=True)
    if values is None:
        values = []
        for idx in range(len(keys)):
            values.append(None)
    assert (len(values) == len(keys))
    for idx in range(len(keys)):
        key = keys[idx]
        tag = tag_uri(key)
        value = values[idx]
        node.props[tag] = value
    client.add_props(node)
    return client.get_node(uri, force=True)


def set_tags(expnum, props):
    """Assign the key/value pairs in props as tags on on the given expnum.

    @param expnum: str
    @param props: dict
    @return: success
    """
    # now set all the props
    return _set_tags(expnum, list(props.keys()), list(props.values()))


def set_tag(expnum, key, value):
    """Assign a key/value pair tag to the given expnum containerNode.

    @param expnum:  str
    @param key: str
    @param value: str
    @return: success
    """

    return set_tags(expnum, {key: value})


def tag_uri(key):
    """Build the uri for a given tag key. 

    @param key: what is the key that we need a stadanrd uri tag for? eg 'mkpsf_00'
    @type key: str
    """
    if OSSOS_TAG_URI_BASE in key:
        return key
    return OSSOS_TAG_URI_BASE + "#" + key.strip()


def get_tag(expnum, key):
    """given a key, return the vospace tag value.

    @param expnum: Number of the CFHT exposure that a tag value is needed for
    @param key: The process tag (such as mkpsf_00) that is being looked up.
    @return: the value of the tag
    @rtype: str
    """

    uri = tag_uri(key)
    force = uri not in get_tags(expnum)
    value = get_tags(expnum, force=force).get(uri, None)
    return value


def get_process_tag(program, ccd, version='p'):
    """
    make a process tag have a suffix indicating which ccd its for.
    @param program: Name of the process that a tag is built for.
    @param ccd: the CCD number that this process ran on.
    @param version: The version of the exposure (s, p, o) that the process ran on.
    @return: The string that represents the processing tag.
    """
    return "%s_%s%s" % (program, str(version), str(ccd).zfill(2))


def get_tags(expnum, force=False):
    """

    @param expnum:
    @param force:
    @return: dict
    @rtype: dict
    """
    uri = os.path.join(DBIMAGES, str(expnum))
    return client.get_node(uri, force=force).props


class Task(object):
    """
    A task within the OSSOS pipeline work-flow.
    """

    def __init__(self, executable, dependency=None):
        self.executable = executable
        self.name = os.path.splitext(self.executable)[0]
        self._target = None
        self._status = None
        self._dependency = None
        self.dependency = dependency

    def __str__(self):
        return self.name

    @property
    def tag(self):
        """
        Get the string representation of the tag used to annotate the status in VOSpace.
        @return: str
        """
        return "{}{}_{}{:02d}".format(self.target.prefix,
                                      self,
                                      self.target.version,
                                      self.target.ccd)

    @property
    def target(self):
        """

        @return: The target that this task is set to run on.
        @rtype: Target
        """
        return self._target

    @target.setter
    def target(self, target):
        assert isinstance(Target, target)
        self._target = target

    @property
    def dependency(self):
        """
        @rtype: Task
        """
        raise NotImplementedError()

    @dependency.setter
    def dependency(self, dependency):
        if dependency is None:
            self._dependency = dependency
        else:
            assert isinstance(Task, dependency)

    @property
    def status(self):
        """

        @return: The status of running this task on the given target.
        @rtype: str
        """
        return get_tag(self.target.expnum, self.tag)

    @status.setter
    def status(self, status):
        status += Time.now().iso
        set_tag(self.target.expnum, self.tag, status)

    @property
    def finished(self):
        """
        @rtype: bool
        """
        return self.status.startswith(SUCCESS)

    @property
    def ready(self):
        if self.dependency is None:
            return True
        else:
            return self.dependency.finished


class Target(object):
    """
    The target that a task will act on.
    """

    def __init__(self, prefix, expnum, version, ccd):
        """

        @param prefix: that is prefixed to the base exposure
        @type prefix: str
        @param expnum: the number of the CFHT exposure that is the target
        @type expnum: str
        @param version: Which version of the exposure (o, p, s) is the target.
        @type version: str
        @param ccd: which CCD of the exposure is the target.
        @type ccd: int
        """

        self.prefix = prefix
        self.expnum = expnum
        self.version = version
        self.ccd = ccd

    @property
    def name(self):
        return "{}{}{}{:02d}".format(self.prefix, self.expnum, self.version, self.ccd)

    @property
    def tags(self):
        """

        @rtype: list [str]
        """
        return get_tags(self.expnum)


def get_status(task, prefix, expnum, version, ccd, return_message=False):
    """
    Report back status of the given program by looking up the associated VOSpace annotation.

    @param task:  name of the process or task that will be checked.
    @param prefix: prefix of the file that was processed (often fk or None)
    @param expnum: which exposure number (or base filename)
    @param version: which version of that exposure (p, s, o)
    @param ccd: which CCD within the exposure.
    @param return_message: Return what did the TAG said or just /True/False/ for Success/Failure?
    @return: the status of the processing based on the annotation value.
    """
    key = get_process_tag(prefix+task, ccd, version)
    status = get_tag(expnum, key)
    logger.debug('%s: %s' % (key, status))
    if return_message:
        return status
    else:
        return status == SUCCESS


def set_status(task, prefix, expnum, version, ccd, status):
    """
    set the processing status of the given program.
    @param task: name of the processing task
    @param prefix: was there a prefix on the exposure number processed?
    @param expnum: exposure number processed.
    @param version: which version of the exposure? (p, s, o)
    @param ccd: the number of the CCD processing.
    @param status: What status to record:  "SUCCESS" we hope.
    @return: Success?
    """
    return set_tag(expnum, get_process_tag(prefix+task, ccd, version), status)


def get_file(expnum, ccd=None, version='p', ext=FITS_EXT, subdir=None, prefix=None):
    uri = get_uri(expnum=expnum, ccd=ccd, version=version, ext=ext, subdir=subdir, prefix=prefix)
    filename = os.path.basename(uri)

    if not os.access(filename, os.F_OK):
        copy(uri, filename)

    return filename


def decompose_content_decomposition(content_decomposition):
    """

    :param content_decomposition:
    :return:
    """
    # check for '-*' in the cutout string and replace is naxis:1
    content_decomposition = re.findall('(\d+)__(\d*)_(\d*)_(\d*)_(\d*)', content_decomposition)
    if len(content_decomposition) == 0:
        content_decomposition = [(0, 1, -1, 1, -1)]
    return content_decomposition


def _cutout_expnum(observation, sky_coord, radius):
    """
    Get a cutout from an exposure based on the RA/DEC location.
    @param observation: The Observation object that contains the expusre number information.
    @type observation: Observation
    @param sky_coord: which RA/DEC is needed,
    @type sky_coord:  SkyCoord
    @param radius:
    @type radius: Quantity
    @return: HDUList containing the cutout image.
    @rtype: list(HDUList)
    """

    uri = observation.get_image_uri()
    cutout_filehandle = tempfile.NamedTemporaryFile()
    disposition_filename = client.copy(uri + "({},{},{})".format(sky_coord.ra.to('degree').value,
                                                                 sky_coord.dec.to('degree').value,
                                                                 radius.to('degree').value),
                                       cutout_filehandle.name,
                                       disposition=True)
    cutouts = decompose_content_decomposition(disposition_filename)

    cutout_filehandle.seek(0)
    hdulist = fits.open(cutout_filehandle, mode='update')
    hdulist.verify('silentfix+ignore')
    logger.debug("Initial Length of HDUList: {}".format(len(hdulist)))

    # Make sure here is a primaryHDU
    if len(hdulist) == 1:
        phdu = fits.PrimaryHDU()
        phdu.header['ORIGIN'] = "OSSOS"
        hdulist.insert(0, phdu)

    logger.debug("Final Length of HDUList: {}".format(len(hdulist)))

    if len(cutouts) != len(hdulist) - 1:
        raise ValueError("Wrong number of cutout structures found in Content-Disposition response.")

    for hdu in hdulist[1:]:
        cutout = cutouts.pop(0)
        if 'ASTLEVEL' not in hdu.header:
            print(("WARNING: ******* NO ASTLEVEL KEYWORD ********** for {0} ********".format(observation.get_image_uri)))
            hdu.header['ASTLEVEL'] = 0
        hdu.header['EXTNO'] = cutout[0]
        naxis1 = hdu.header['NAXIS1']
        naxis2 = hdu.header['NAXIS2']
        default_datasec = "[{}:{},{}:{}]".format(1, naxis1, 1, naxis2)
        datasec = hdu.header.get('DATASEC', default_datasec)
        datasec = datasec_to_list(datasec)
        corners = datasec
        for idx in range(len(corners)):
            try:
                corners[idx] = int(cutout[idx+1])
            except Exception:
                pass

        hdu.header['DATASEC'] = reset_datasec("[{}:{},{}:{}]".format(corners[0],
                                                                     corners[1],
                                                                     corners[2],
                                                                     corners[3]),
                                              hdu.header.get('DATASEC', default_datasec),
                                              hdu.header['NAXIS1'],
                                              hdu.header['NAXIS2'])
        hdu.header['XOFFSET'] = int(corners[0]) - 1
        hdu.header['YOFFSET'] = int(corners[2]) - 1
        hdu.converter = CoordinateConverter(hdu.header['XOFFSET'], hdu.header['YOFFSET'])
        try:
                hdu.wcs = WCS(hdu.header)
        except Exception as ex:
            logger.error("Failed trying to initialize the WCS for {}".format(uri))
            raise ex
    logger.debug("Sending back {}".format(hdulist))
    return hdulist


def ra_dec_cutout(uri, sky_coord, radius, update_wcs=False):
    """

    :param uri: The vospace location of the image to make a cutout from
    :type uri: str
    :param sky_coord: The central coordinate of the cutout
    :type sky_coord: SkyCoord
    :param radius: The radius of the cutout
    :type radius: Quantity
    :return: An HDUList with the cutout
    :rtype: fits.HDUList
    """

    # These two lines enable debugging at httplib level (requests->urllib3->http.client)
    # You will see the REQUEST, including HEADERS and DATA, and RESPONSE with HEADERS but without DATA.
    # The only thing missing will be the response.body which is not logged.
    try:
        import http.client as http_client
    except ImportError:
        # Python 2
        import http.client as http_client

    # Get the 'uncut' images CRPIX1/CRPIX2 values

    cutout_filehandle = tempfile.NamedTemporaryFile()
    disposition_filename = client.copy(uri + "({},{},{})".format(sky_coord.ra.to('degree').value,
                                                                 sky_coord.dec.to('degree').value,
                                                                 radius.to('degree').value),
                                       cutout_filehandle.name,
                                       disposition=True)
    cutouts = decompose_content_decomposition(disposition_filename)

    cutout_filehandle.seek(0)
    try:
        hdulist = fits.open(cutout_filehandle, mode='update')
        hdulist.verify('silentfix+ignore')
    except Exception as ex:
        raise ex

    logger.debug("Got cutout boundaries of: {}".format(cutouts))
    logger.debug("Initial Length of HDUList: {}".format(len(hdulist)))

    # Make sure here is a primaryHDU
    if len(hdulist) == 1:
        phdu = fits.PrimaryHDU()
        phdu.header['ORIGIN'] = "OSSOS"
        hdulist.insert(0, phdu)

    logger.debug("Final Length of HDUList: {}".format(len(hdulist)))

    if len(cutouts) != len(hdulist) - 1:
        raise ValueError("Wrong number of cutout structures found in Content-Disposition response.")

    for hdu in hdulist[1:]:

        cutout = cutouts.pop(0)
        if update_wcs:
            # Pull the SG header from VOSpace and reset the CRPIX values based on cutout info from disposition matrix
            try:
                sg_key = "{}{}".format(hdu.header['expnum'], 'p')
                if sg_key not in sgheaders:
                    _get_sghead(hdu.header['expnum'])
                if sg_key in sgheaders:
                    for astheader in sgheaders[sg_key]:
                        if astheader is None:
                            continue
                        if astheader.get('EXTVER', -1) == hdu.header['EXTVER']:
                            break
                astheader['CRPIX1'] = astheader.get('CRPIX1', 1) - int(cutout[1]) + 1
                astheader['CRPIX2'] = astheader.get('CRPIX2', 1) - int(cutout[3]) + 1
                # pull some data structure keywords out of the astrometric headers
                for key in ['NAXIS', 'XTENSION', 'PCOUNT', 'GCOUNT',
                            'NAXIS1', 'NAXIS2', 'BITPIX', 'BZERO', 'BSCALE']:
                    if astheader.get(key, None) is not None:
                        del (astheader[key])
                hdu.header.update(astheader)
            except Exception as ex:
                logging.error("Got error while updating WCS: {}".format(ex))
                logging.error("Using existing WCS in image header")
        if 'ASTLEVEL' not in hdu.header:
            print(("******* NO ASTLEVEL ****************** for {0} ********".format(uri)))
            hdu.header['ASTLEVEL'] = 0
        hdu.header['EXTNO'] = cutout[0]
        hdu.header['DATASEC'] = reset_datasec("[{}:{},{}:{}]".format(cutout[1],
                                                                     cutout[2],
                                                                     cutout[3],
                                                                     cutout[4]),
                                              hdu.header.get('DATASEC', None),
                                              hdu.header['NAXIS1'],
                                              hdu.header['NAXIS2'])
        hdu.header['XOFFSET'] = int(cutout[1]) - 1
        hdu.header['YOFFSET'] = int(cutout[3]) - 1

        hdu.converter = CoordinateConverter(hdu.header['XOFFSET'], hdu.header['YOFFSET'])
        try:
                hdu.wcs = WCS(hdu.header)
        except Exception as ex:
            logger.error("Failed trying to initialize the WCS for {}".format(uri))
            raise ex
    logger.debug("Sending back {}".format(hdulist))
    return hdulist

def frame2expnum(frameid):
    """Given a standard OSSOS frameid return the expnum, version and ccdnum as a dictionary."""
    result = {}
    parts = re.search('(?P<expnum>\d{7})(?P<type>\S)(?P<ccd>\d\d)', frameid)
    assert parts is not None
    result['expnum'] = parts.group('expnum')
    result['ccd'] = parts.group('ccd')
    result['version'] = parts.group('type')
    return result

def get_frame(frameid, cutout=None):
    """Given a frameid, which consists of expnum version and ccd retrieve a cutout."""
    return get_image(cutout=cutout, **frame2expnum(frameid))


def get_image(expnum, ccd=None, version='p', ext=FITS_EXT,
              subdir=None, prefix=None, cutout=None, return_file=True, flip_image=True):
    """Get a FITS file for this expnum/ccd  from VOSpace.


    @param cutout: (str)
    @param return_file: return an filename (True) or HDUList (False)
    @param expnum: CFHT exposure number (int)
    @param ccd:     @param ccd:
    @param version: [p, s, o]  (char)
    @param ext:
    @param subdir:
    @param prefix:
    @param flip_image: Should the image be x/y flipped after being retrieved?
    @return: astropy.io.fits.PrimaryHDU
    """

    filename = os.path.basename(get_uri(expnum, ccd, version, ext=ext, subdir=subdir, prefix=prefix))
    if os.access(filename, os.F_OK) and return_file and cutout is None:
        return filename

    cutout_string = cutout
    try:
        if os.access(filename, os.F_OK) and cutout:
            cutout = datasec_to_list(cutout)
            hdulist = fits.open(filename, mode='update')
            if len(hdulist) > 1:
                raise ValueError("Local cutout access not designed to work on MEFs yet.")
            header = hdulist[0].header
            cutout[0] = cutout[0] < 0 and header['NAXIS1'] - cutout[0] + 1 or cutout[0]
            cutout[1] = cutout[1] < 0 and header['NAXIS1'] - cutout[1] + 1 or cutout[1]
            cutout[2] = cutout[2] < 0 and header['NAXIS2'] - cutout[2] + 1 or cutout[2]
            cutout[3] = cutout[3] < 0 and header['NAXIS2'] - cutout[3] + 1 or cutout[3]
            logger.debug("DATA array shape: {}".format(hdulist[0].data.shape))
            logger.debug("CUTOUT array: {} {} {} {}".format(cutout[0],
                                                            cutout[1],
                                                            cutout[2],
                                                            cutout[3]))
            flip = cutout[0] < cutout[1] and 1 or -1
            flop = cutout[2] < cutout[3] and 1 or -1
            header['CRPIX1'] = (header.get("CRPIX1", 1) - cutout[0]) * flip + 1
            header['CRPIX2'] = (header.get("CRPIX2", 1) - cutout[2]) * flop + 1
            header['CD1_1'] = header.get("CD1_1", 1) * flip
            header['CD2_1'] = header.get("CD2_1", 1) * flip
            header['CD2_2'] = header.get("CD2_2", 1) * flop
            header['CD1_2'] = header.get("CD1_2", 1) * flop

            data = hdulist[0].data[cutout[2]-1:cutout[3], cutout[0]-1:cutout[1]]
            hdulist[0].data = data
            header['DATASEC'] = reset_datasec(cutout_string,
                                              header['DATASEC'],
                                              header['NAXIS1'],
                                              header['NAXIS2'])
            if return_file:
                cutout_filename = os.path.splitext(filename)[0]+"_{}_{}_{}_{}.fits".format(cutout[0],
                                                                                           cutout[1],
                                                                                           cutout[2],
                                                                                           cutout[3])
                hdulist.writeto(cutout_filename)
                return cutout_filename
            else:
                return hdulist
    except Exception as e:
        logger.debug(str(e))
        logger.debug("Failed trying to access local copy: {} with cutout [{}:{}, {}:{}], using VOSpace".format(
            filename, cutout[2]-1, cutout[3], cutout[0]-1, cutout[1]))

    cutout = cutout_string

    if not subdir:
        subdir = str(expnum)

    logger.debug("Building list of possible uri locations")
    # # here is the list of places we will look, in order
    if version != 'p':
        locations = [(get_uri(expnum, ccd, version, ext=ext, subdir=subdir, prefix=prefix),
                      cutout)]
    else:
        locations = []
    logger.debug(str(locations))
    if ccd is not None:
        try:
            for this_ext in [ext]:
                ext_no = int(ccd) + 1
                # extension 1 -> 18 +  37,36 should be flipped.
                flip_these_extensions = list(range(1, 19))
                flip_these_extensions.append(37)
                flip_these_extensions.append(38)
                flip = (cutout is None and "fits" in ext and (
                    (ext_no in flip_these_extensions and flip_image) and "[-*,-*]" or "[*,*]")) or cutout
                locations.append((get_uri(expnum, version=version, ext=this_ext, subdir=subdir),
                                  "[{}]{}".format(ext_no, flip)))
        except Exception as e:
            logger.error(str(e))
            pass
    else:
        uri = get_uri(expnum, ccd, version, ext=ext, subdir=subdir, prefix=prefix)
        locations.append((uri, cutout))
        # uri = get_uri(expnum, ccd, version, ext=ext + ".fz", subdir=subdir, prefix=prefix)
        # locations.append((uri, cutout))

    err = errno.EFAULT
    while len(locations) > 0:
        (uri, cutout) = locations.pop(0)
        try:
            hdu_list = get_hdu(uri, cutout)
            if return_file:
                hdu_list.writeto(filename)
                del hdu_list
                return filename
            else:
                return hdu_list
        except Exception as e:
            err = getattr(e, 'errno', errno.EAGAIN)
            logger.debug("{}".format(type(e)))
            logger.debug("Failed to open {} cutout:{}".format(uri, cutout))
            logger.debug("vos sent back error: {} code: {}".format(str(e), getattr(e, 'errno', 0)))

    if err == errno.EAGAIN:
        time.sleep(5)
        return get_image(expnum, ccd=ccd, version=version, ext=ext,
                         subdir=subdir, prefix=prefix, cutout=cutout, return_file=return_file, flip_image=flip_image)
    raise IOError(err, "Failed to get image at uri: {} using {} {} {} {}.".format(uri, expnum, version, ccd, cutout))


def datasec_to_list(datasec):
    """
    convert an IRAF style PIXEL DATA section as to a list of integers.
    @param datasec: str
    @return: list
    """

    return [int(x) for x in re.findall(r"([-+]?[*\d]+?)[:,\]]+", datasec)]


def reset_datasec(cutout, datasec, naxis1, naxis2):
    """
    reset the datasec to account for a possible cutout.

    @param cutout:
    @param datasec:
    @param naxis1: size of the original image in the 'x' direction
    @param naxis2: size of the oringal image in the 'y' direction
    @return:
    """

    if cutout is None or cutout == "[*,*]":
        return datasec
    try:
        datasec = datasec_to_list(datasec)
    except:
        return datasec

    # check for '-*' in the cutout string and replace is naxis:1
    cutout = cutout.replace(" ", "")
    cutout = cutout.replace("[-*,", "{}:1,".format(naxis1))
    cutout = cutout.replace(",-*]", ",{}:1]".format(naxis2))
    cutout = cutout.replace("[*,", "[1:{},".format(naxis1))
    cutout = cutout.replace(",*]", ",1:{}]".format(naxis1))

    try:
        cutout = [int(x) for x in re.findall(r"([-+]?[*\d]+?)[:,\]]+", cutout)]
    except:
        logger.debug("Failed to processes the cutout pattern: {}".format(cutout))
        return datasec

    if len(cutout) == 5:
        # cutout likely starts with extension, remove
        cutout = cutout[1:]

    # -ve integer offsets indicate offset from the end of array.
    for idx in [0, 1]:
        if cutout[idx] < 0:
            cutout[idx] = naxis1 - cutout[idx] + 1
    for idx in [2, 3]:
        if cutout[idx] < 0:
            cutout[idx] = naxis2 - cutout[idx] + 1

    flip = cutout[0] > cutout[1]
    flop = cutout[2] > cutout[3]

    logger.debug("Working with cutout: {}".format(cutout))

    if flip:
        cutout = [naxis1 - cutout[0] + 1, naxis1 - cutout[1] + 1, cutout[2], cutout[3]]
        datasec = [naxis1 - datasec[1] + 1, naxis1 - datasec[0] + 1, datasec[2], datasec[3]]

    if flop:
        cutout = [cutout[0], cutout[1], naxis2 - cutout[2] + 1, naxis2 - cutout[3] + 1]
        datasec = [datasec[0], datasec[1], naxis2 - datasec[3] + 1, naxis2 - datasec[2] + 1]

    datasec = [max(datasec[0] - cutout[0] + 1, 1),
               min(datasec[1] - cutout[0] + 1, naxis1),
               max(datasec[2] - cutout[2] + 1, 1),
               min(datasec[3] - cutout[2] + 1, naxis2)]

    return "[{}:{},{}:{}]".format(datasec[0], datasec[1], datasec[2], datasec[3])


def get_hdu(uri, cutout=None):
    """Get a at the given uri from VOSpace, possibly doing a cutout.

    If the cutout is flips the image then we also must flip the datasec keywords.  Also, we must offset the
    datasec to reflect the cutout area being used.

    @param uri: The URI in VOSpace of the image to HDU to retrieve.
    @param cutout: A CADC data service CUTOUT paramter to be used when retrieving the observation.
    @return: fits.HDU
    """
    try:
        # the filename is based on the Simple FITS images file.
        filename = os.path.basename(uri)
        if os.access(filename, os.F_OK) and cutout is None:
            logger.debug("File already on disk: {}".format(filename))
            hdu_list = fits.open(filename, model='update') # , scale_back=True)
            hdu_list.verify('silentfix+ignore')

        else:
            logger.debug("Pulling: {}{} from VOSpace".format(uri, cutout))
            fpt = tempfile.NamedTemporaryFile(suffix='.fits', mode='w+b')
            cutout = cutout is not None and cutout or ""
            copy(uri+cutout, fpt.name)
            fpt.seek(0, 2)
            fpt.seek(0)
            logger.debug("Read from vospace completed. Building fits object.")
            hdu_list = fits.open(fpt, scale_back=False, mode='update')
            hdu_list.verify('silentfix+ignore')

            logger.debug("Got image from vospace")
            try:
                hdu_list[0].header['DATASEC'] = reset_datasec(cutout, hdu_list[0].header['DATASEC'],
                                                              hdu_list[0].header['NAXIS1'],
                                                              hdu_list[0].header['NAXIS2'])
            except Exception as e:
                logging.debug("error converting datasec: {}".format(str(e)))

        for hdu in hdu_list:
            logging.debug("Adding converter to {}".format(hdu))
            hdu.converter = CoordinateConverter(0, 0)
            try:
                hdu.wcs = WCS(hdu.header)
            except Exception as ex:
                logger.error("Failed trying to initialize the WCS: {}".format(ex))
    except Exception as ex:
        raise ex
    return hdu_list


def get_trans(expnum, ccd, prefix=None, version='p'):
    """

    @param expnum: The exposure number to get the WCS transformation for.
    @param ccd: The CCD of that exposure
    @param prefix: Any possible prefixs (such as 'fk')
    @param version: which version of the exposure?
    @return:
    """
    uri = get_uri(expnum, ccd, version, ext='trans.jmp', prefix=prefix)
    logging.info("get_trans: {}".format(uri))
    fobj = open_vos_or_local(uri)
    line = fobj.read()
    fobj.close()
    vs = line.split()
    trans = {'dx': float(vs[0]),
             'cd11': float(vs[1]),
             'cd12': float(vs[2]),
             'dy': float(vs[3]),
             'cd21': float(vs[4]),
             'cd22': float(vs[5])}
    return trans


def get_fwhm_tag(expnum, ccd, prefix=None, version='p'):
    """
    Get the FWHM from the VOSpace annotation.

    @param expnum:
    @param ccd:
    @param prefix:
    @param version:
    @return:
    """
    uri = get_uri(expnum, ccd, version, ext='fwhm', prefix=prefix)
    if uri not in fwhm:
        key = "fwhm_{:1s}{:02d}".format(version, int(ccd))
        fwhm[uri] = get_tag(expnum, key)
    return fwhm[uri]


def get_fwhm(expnum, ccd, prefix=None, version='p'):
    """Get the FWHM computed for the given expnum/ccd combo.

    @param expnum:
    @param ccd:
    @param prefix:
    @param version:
    @return:
    """

    uri = get_uri(expnum, ccd, version, ext='fwhm', prefix=prefix)
    filename = os.path.basename(uri)

    try:
        return fwhm[uri]
    except:
        pass

    try:
        fwhm[uri] = float(open(filename, 'r').read())
        return fwhm[uri]
    except:
        pass

    try:
        fwhm[uri] = float(open_vos_or_local(uri).read())
        return fwhm[uri]
    except Exception as ex:
        logger.error(str(ex))
        fwhm[uri] = 4.0
        return fwhm[uri]


def _get_zeropoint(expnum, ccd, prefix=None, version='p'):
    """
    Retrieve the zeropoint stored in the tags associated with this image.
    @param expnum: Exposure number
    @param ccd: ccd of the exposure
    @param prefix: possible prefix (such as 'fk')
    @param version: which version: p, s, or o ?
    @return: zeropoint
    """
    if prefix is not None:
        DeprecationWarning("Prefix is no longer used here as the 'fk' and 's' have the same zeropoint.")

    key = "zeropoint_{:1s}{:02d}".format(version, int(ccd))
    return get_tag(expnum, key)


def get_zeropoint(expnum, ccd, prefix=None, version='p'):
    """Get the zeropoint for this exposure using the zeropoint.used file created during source planting..

    This command expects that there is a file called #######p##.zeropoint.used which contains the zeropoint.

    @param expnum: exposure to get zeropoint of
    @param ccd: which ccd (extension - 1) to get zp
    @param prefix: possible string prefixed to expsoure number.
    @param version:  one of [spo] as in #######p##
    @return: zeropoint
    """
    uri = get_uri(expnum, ccd, version, ext='zeropoint.used', prefix=prefix)
    try:
        return zmag[uri]
    except:
        pass

    try:
        zmag[uri] = float(open_vos_or_local(uri).read())
        return zmag[uri]
    except:
        pass

    zmag[uri] = 0.0
    return zmag[uri]


def mkdir(dirname):
    """make directory tree in vospace.

    @param dirname: name of the directory to make
    """
    dir_list = []

    while not client.isdir(dirname):
        dir_list.append(dirname)
        dirname = os.path.dirname(dirname)
    while len(dir_list) > 0:
        logging.info("Creating directory: %s" % (dir_list[-1]))
        try:
            client.mkdir(dir_list.pop())
        except IOError as e:
            if e.errno == errno.EEXIST:
                pass
            else:
                raise e


def vofile(filename, **kwargs):
    """
    Open and return a handle on a VOSpace data connection
    @param filename:
    @param kwargs:
    @return:
    """
    basename = os.path.basename(filename)
    if os.access(basename, os.R_OK):
        return open(basename, 'r')
    kwargs['view'] = kwargs.get('view', 'data')
    return client.open(filename, **kwargs)


def open_vos_or_local(path, mode="rb"):
    """
    Opens a file which can either be in VOSpace or the local filesystem.

    @param path:
    @param mode:
    @return:
    """
    filename = os.path.basename(path)
    if os.access(filename, os.F_OK):
        return open(filename, mode)
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

        return vofile(path, mode=vofile_mode)
    else:
        return open(path, mode)


def copy(source, dest):
    """
    use the vospace service to get a file.

    @param source:
    @param dest:
    @return:
    """
    logger.info("copying {} -> {}".format(source, dest))

    return client.copy(source, dest)


def vlink(s_expnum, s_ccd, s_version, s_ext,
          l_expnum, l_ccd, l_version, l_ext, s_prefix=None, l_prefix=None):
    """make a link between two version of a file.

    @param s_expnum:
    @param s_ccd:
    @param s_version:
    @param s_ext:
    @param l_expnum:
    @param l_ccd:
    @param l_version:
    @param l_ext:
    @param s_prefix:
    @param l_prefix:
    @return:
    """
    source_uri = get_uri(s_expnum, ccd=s_ccd, version=s_version, ext=s_ext, prefix=s_prefix)
    link_uri = get_uri(l_expnum, ccd=l_ccd, version=l_version, ext=l_ext, prefix=l_prefix)

    return client.link(source_uri, link_uri)


def remove(uri):
    try:
        client.delete(uri)
    except Exception as e:
        logger.debug(str(e))


def delete(expnum, ccd, version, ext, prefix=None):
    """
    delete a file, no error on does not exist

    @param expnum:
    @param ccd:
    @param version:
    @param ext:
    @param prefix:
    @return:
    """
    uri = get_uri(expnum, ccd=ccd, version=version, ext=ext, prefix=prefix)
    remove(uri)


def my_glob(pattern):
    """
    get a listing matching pattern

    @param pattern:
    @return:
    """
    result = []
    if pattern[0:4] == 'vos:':
        dirname = os.path.dirname(pattern)
        flist = listdir(dirname)
        for fname in flist:
            fname = '/'.join([dirname, fname])
            if fnmatch.fnmatch(fname, pattern):
                result.append(fname)
    else:
        result = glob(pattern)
    return result


def listdir(directory, force=False):
    return client.listdir(directory, force=force)


def list_dbimages(dbimages=DBIMAGES):
    return listdir(dbimages)


def exists(uri, force=False):
    try:
        return client.get_node(uri, force=force) is not None
    except EnvironmentError as e:
        logger.error(str(e))  # not critical enough to raise
        # Sometimes the error code returned is the OS version, sometimes the HTTP version
        if e.errno in [404, os.errno.ENOENT]:
            return False


def move(old_uri, new_uri):
    client.move(old_uri, new_uri)


def delete_uri(uri):
    client.delete(uri)


def has_property(node_uri, property_name, ossos_base=True):
    """
    Checks if a node in VOSpace has the specified property.

    @param node_uri:
    @param property_name:
    @param ossos_base:
    @return:
    """
    if get_property(node_uri, property_name, ossos_base) is None:
        return False
    else:
        return True


def get_property(node_uri, property_name, ossos_base=True):
    """
    Retrieves the value associated with a property on a node in VOSpace.

    @param node_uri:
    @param property_name:
    @param ossos_base:
    @return:
    """
    # Must use force or we could have a cached copy of the node from before
    # properties of interest were set/updated.
    node = client.get_node(node_uri, force=True)
    property_uri = tag_uri(property_name) if ossos_base else property_name

    if property_uri not in node.props:
        return None

    return node.props[property_uri]


def set_property(node_uri, property_name, property_value, ossos_base=True):
    """
    Sets the value of a property on a node in VOSpace.  If the property
    already has a value then it is first cleared and then set.

    @param node_uri:
    @param property_name:
    @param property_value:
    @param ossos_base:
    @return:
    """
    node = client.get_node(node_uri)
    property_uri = tag_uri(property_name) if ossos_base else property_name

    # If there is an existing value, clear it first
    if property_uri in node.props:
        node.props[property_uri] = None
        client.add_props(node)

    node.props[property_uri] = property_value
    client.add_props(node)


def build_counter_tag(epoch_field, dry_run=False):
    """
    Builds the tag for the counter of a given epoch/field, without the OSSOS base.

    @param epoch_field:
    @param dry_run:
    @return:
    """
    logger.info("Epoch Field: {}, OBJECT_COUNT {}".format(str(epoch_field), str(OBJECT_COUNT)))
    tag = epoch_field[1] + "-" + OBJECT_COUNT

    if dry_run:
        tag += "-DRYRUN"

    return tag


def read_object_counter(node_uri, epoch_field, dry_run=False):
    """
    Reads the object counter for the given epoch/field on the specified node.

    @param node_uri:
    @param epoch_field:
    @param dry_run:
    @return: the current object count.
    """
    return get_property(node_uri, build_counter_tag(epoch_field, dry_run),
                        ossos_base=True)


def increment_object_counter(node_uri, epoch_field, dry_run=False):
    """
    Increment the object counter used to create unique object identifiers.

    @param node_uri:
    @param epoch_field:
    @param dry_run:
    @return: The object count AFTER incrementing.

    """
    current_count = read_object_counter(node_uri, epoch_field, dry_run=dry_run)

    if current_count is None:
        new_count = "01"
    else:
        new_count = coding.base36encode(coding.base36decode(current_count) + 1,
                                        pad_length=2)

    set_property(node_uri,
                 build_counter_tag(epoch_field, dry_run=dry_run),
                 new_count,
                 ossos_base=True)

    return new_count


def get_mopheader(expnum, ccd, version='p', prefix=None):
    """
    Retrieve the mopheader, either from cache or from vospace

    @param expnum:
    @param ccd:
    @param version:
    @param prefix:
    @return: Header
    """
    prefix = prefix is None and "" or prefix
    mopheader_uri = dbimages_uri(expnum=expnum,
                                 ccd=ccd,
                                 version=version,
                                 prefix=prefix,
                                 ext='.mopheader')
    if mopheader_uri in mopheaders:
        return mopheaders[mopheader_uri]

    filename = os.path.basename(mopheader_uri)

    if os.access(filename, os.F_OK):
        logger.debug("File already on disk: {}".format(filename))
        mopheader_fpt = BytesIO(open(filename, 'rb').read())
    else:
        mopheader_fpt = BytesIO(open_vos_or_local(mopheader_uri).read())

    with warnings.catch_warnings():
        warnings.simplefilter('ignore', AstropyUserWarning)
        mopheader = fits.open(mopheader_fpt)

        # add some values to the mopheader so it can be an astrom header too.
        header = mopheader[0].header
        try:
            header['FWHM'] = get_fwhm(expnum, ccd)
        except IOError:
            header['FWHM'] = 10
        header['SCALE'] = mopheader[0].header['PIXSCALE']
        header['NAX1'] = header['NAXIS1']
        header['NAX2'] = header['NAXIS2']
        header['MOPversion'] = header['MOP_VER']
        header['MJD_OBS_CENTER'] = str(Time(header['MJD-OBSC'],
                                            format='mjd',
                                            scale='utc', precision=5).replicate(format='mpc'))
        header['MAXCOUNT'] = MAXCOUNT
        mopheaders[mopheader_uri] = header
        mopheader.close()
    return mopheaders[mopheader_uri]


def _get_sghead(expnum):
    """
    Use the data web service to retrieve the stephen's astrometric header.

    :param expnum: CFHT exposure number you want the header for
    :rtype : list of astropy.io.fits.Header objects.
    """
    version = 'p'

    key = "{}{}".format(expnum, version)
    if key in sgheaders:
        return sgheaders[key]

    header_filename = "{}{}.head".format(expnum, version)

    if not os.access(header_filename, os.R_OK):
        url = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/CFHTSG/{}".format(header_filename)
        logging.getLogger("requests").setLevel(logging.ERROR)
        logging.debug("Attempting to retrieve {}".format(url))
        resp = requests.get(url)
        if resp.status_code != 200:
            raise IOError(errno.ENOENT, "Could not get {}".format(url))
        with open(header_filename,'w') as hobj:
            hobj.write(resp.content)

    with open(header_filename, 'w') as hobj:
        header_str_list = re.split('END      \n', hobj.read())

    # # make the first entry in the list a Null
    headers = [None]
    for header_str in header_str_list:
        headers.append(fits.Header.fromstring(header_str, sep='\n'))
        logging.debug(headers[-1].get('EXTVER', -1))
    sgheaders[key] = headers
    return sgheaders[key]


def get_header(uri):
    """
    Pull a FITS header from observation at the given URI

    @param uri:  The URI of the image in VOSpace.
    """
    if uri not in astheaders:
        astheaders[uri] = get_hdu(uri, cutout="[1:1,1:1]")[0].header
    return astheaders[uri]


def get_astheader(expnum, ccd, version='p', prefix=None):
    """
    Retrieve the header for a given dbimages file.

    @param expnum:  CFHT odometer number
    @param ccd: which ccd based extension (0..35)
    @param version: 'o','p', or 's'
    @param prefix:
    @return:
    """
    logger.debug("Getting ast header for {}".format(expnum))
    if version == 'p':
        try:
            # Get the SG header if possible.
            sg_key = "{}{}".format(expnum, version)
            if sg_key not in sgheaders:
                _get_sghead(expnum)
            if sg_key in sgheaders:
                for header in sgheaders[sg_key]:
                        if header.get('EXTVER', -1) == int(ccd):
                            return header
        except Exception as ex:
            print(ex)
            pass

    try:
       ast_uri = dbimages_uri(expnum, ccd, version=version, ext='.fits')
       if ast_uri not in astheaders:
           hdulist = get_image(expnum, ccd=ccd, version=version, prefix=prefix,
                               cutout="[1:1,1:1]", return_file=False, ext='.fits')
           assert isinstance(hdulist, fits.HDUList)
           astheaders[ast_uri] = hdulist[0].header
    except:
       ast_uri = dbimages_uri(expnum, ccd, version=version, ext='.fits.fz')
       if ast_uri not in astheaders:
           hdulist = get_image(expnum, ccd=ccd, version=version, prefix=prefix,
                               cutout="[1:1,1:1]", return_file=False, ext='.fits.fz')
           assert isinstance(hdulist, fits.HDUList)
           astheaders[ast_uri] = hdulist[0].header
    return astheaders[ast_uri]


def log_filename(prefix, task, version, ccd):
    return "{}{}_{}{}.txt".format(prefix, task, version, ccd)


def log_location(expnum, ccd):
    return os.path.dirname(get_uri(expnum, ccd=ccd))


class LoggingManager(object):

    def __init__(self, task, prefix, expnum, ccd, version, dry_run=False):
        self.logger = logging.getLogger('')
        self.log_format = logging.Formatter('%(asctime)s - %(module)s.%(funcName)s %(lineno)d: %(message)s')
        self.filename = log_filename(prefix, task, ccd=ccd, version=version)
        self.location = log_location(expnum, ccd)
        self.dry_run = dry_run

    def __enter__(self):
        if not self.dry_run:
            self.vo_handler = util.VOFileHandler("/".join([self.location, self.filename]))
            self.vo_handler.setFormatter(self.log_format)
            self.logger.addHandler(self.vo_handler)
        self.file_handler = logging.FileHandler(filename=self.filename)
        self.file_handler.setFormatter(self.log_format)
        self.logger.addHandler(self.file_handler)
        return self

    def __exit__(self, *args):
        if not self.dry_run:
            self.logger.removeHandler(self.vo_handler)
            self.vo_handler.close()
            del self.vo_handler
        self.logger.removeHandler(self.file_handler)
        self.file_handler.close()
        del self.file_handler
