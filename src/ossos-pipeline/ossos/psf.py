"""
The PSF for an OSSOS image.
"""
import logging
import os
import tempfile
from astropy.io import fits
from astropy.io.ascii import daophot
import numpy
from ossos import storage
import util


__author__ = 'jjk'

PSF_EXTENSION = ".psf.fits"
BRIGHT_STAR_LIST_EXTENSION = ".bright.psf"
BRIGHT_MAG_LIST_EXTENSION = ".bright.mag"
MOP_HEADER_EXTENSION = ".mopheader"
ZEROPOINT_EXTENSION = ".zeropoint.used"

PSF_ORDER = 3
SMALL_APERTURE_FRACTION = 1.1
LARGE_APERTURE_FRACTION = 5.0
NOMINAL_FWHM = 4.0

# The itime is set to 1 as the SGwyn ZP includes the exposure time.
DEFAULT_INTEGRATION_TIME = 1
DEFAULT_READ_NOISE = 4
DATA_MAX = 30000
DATA_MIN = -1000
MAXIMUM_LINEAR_COUNTS = 30000
BRIGHT_STAR_THRESHOLD = 4

# Width of sky annulus in units of fwhm
DEFAULT_SKY_DANNULUS = 1

ZEROPOINT_HEADER_KEYWORD = "PHOTZP"
READ_NOISE_KEYWORD = "RDNOISE"
UTC_OBS_HEADER_KEYWORD = "UTC-OBS"
INTEGRATION_TIME_HEADER_KEYWORD = "EXPTIME"
AIRMASS_HEADER_KEYWORD = "AIRMASS"
GAIN_HEADER_KEYWORD = "GAIN"


class Image(object):
    def __init__(self, expnum, ccd, version='p', prefix=None):
        self.expnum = expnum
        self.ccd = ccd
        self.version = version
        self.prefix = prefix is None and "" or prefix
        self._hdu = None
        self._fwhm = None
        self._zeropoint = None
        self._mopheader = None
        self._psf = None
        self._bright_star_list = None
        self._obj_jmp = None
        self._obj_matt = None
        self._iraf = None

    @property
    def iraf(self):
        """
        Load IRAF and set a bunch of default parameters that we use.
        :return:
        """
        if self._iraf is not None:
            return self._iraf

        from pyraf import iraf

        self._iraf = iraf
        iraf.set(uparm="./")
        iraf.digiphot(_doprint=0)
        iraf.apphot(_doprint=0)
        iraf.daophot(_doprint=0)

        iraf.daophot.verify = iraf.no
        iraf.daophot.verbose = iraf.no
        iraf.daophot.update = iraf.no

        iraf.psf.showplots = iraf.no
        iraf.psf.interactive = iraf.no

        iraf.datapars.exposure = ""
        iraf.datapars.datamin = 0
        iraf.datapars.itime = DEFAULT_INTEGRATION_TIME
        iraf.datapars.gain = GAIN_HEADER_KEYWORD
        iraf.datapars.airmass = AIRMASS_HEADER_KEYWORD
        iraf.datapars.obstime = UTC_OBS_HEADER_KEYWORD
        iraf.datapars.ccdread = READ_NOISE_KEYWORD
        iraf.datapars.readnoise = DEFAULT_READ_NOISE
        iraf.datapars.datamax = DATA_MAX
        iraf.datapars.datamin = DATA_MIN
        iraf.datapars.fwhmpsf = self.fwhm

        iraf.photpars.apertures = self.apmax
        iraf.photpars.apertures = self.apmin
        iraf.photpars.zmag = self.zeropoint

        iraf.fitskypars.salgori = "centroid"
        iraf.fitskypars.dannulus = DEFAULT_SKY_WIDTH * self.fwhm
        iraf.fitskypars.annulus = apmax + 2

        iraf.centerpars.calgori = "centroid"
        iraf.centerpars.maxshift = 3

        return self._iraf


    @property
    def bright_star_list(self):
        try:
            return storage.get_file(ext=BRIGHT_STAR_LIST_EXTENSION, **self.kwargs)
        except:
            return self.build_bright_star_list()

    @property
    def ext(self):
        return PSF_EXTENSION

    @property
    def filename(self):
        """
        Get the filename from the storage system, but don't actually retrieve a file from storage

        :return: filename
        """
        return os.path.basename(storage.get_uri(self.expnum,
                                                ccd=self.ccd,
                                                version=self.version,
                                                ext=self.ext,
                                                prefix=self.prefix))

    @property
    def file(self):
        """
        Get the image from the storage system.

        :return: filename
        """
        return storage.get_image(**self.kwargs)

    @property
    def psf(self):
        try:
            return storage.get_image(ext=self.ext, **self.kwargs)
        except IOError as err:
            logging.debug(str(err))
            return None

    @property
    def fwhm(self):
        if self._fwhm is None:
            try:
                self._fwhm = storage.get_fwhm(**self.kwargs)
            except IOError as err:
                logging.debug(str(err))
                self._fwhm = NOMINAL_FWHM
        return self._fwhm

    @property
    def kwargs(self):
        kwargs = {}
        for attribute in ["expnum",
                          "ccd",
                          "version",
                          "prefix"]:
            kwargs[attribute] = self.__getattribute__(attribute)
        return kwargs

    @property
    def zeropoint(self):
        try:
            return storage.get_zeropoint(**self.kwargs)
        except:
            return self.build_zeropoint_used()

    @property
    def hdu(self):
        """
        A FITS Header Data Unit that contains the image.
        :return:
        """
        if self._hdu is None:
            self._hdu = fits.open(self.file)
        return self._hdu

    def build_zeropoint_used(self):
        zeropoint_uri = storage.get_uri(ext=ZEROPOINT_EXTENSION,
                                        **self.kwargs)
        with open(os.path.basename(zeropoint_uri), 'w') as fhandle:
            fhandle.write(self.hdu.header.get(ZEROPOINT_KEYWORD, DEFAULT_ZEROPOINT))
        storage.copy(os.path.basename(zeropoint_uri), zeropoint_uri)
        return storage.get_zeropoint(**self.kwargs)

    @property
    def mopheader(self):
        try:
            return storage.get_mopheader(**self.kwargs)
        except:
            return build_mopheader()

    @property
    def apmin(self):
        apmin = int(10 * SMALL_APERTURE_FRACTION * self.fwhm + 0.5) / 10.0
        return apmin < 1.5 and 1.5 or apmin

    @property
    def apmax(self):
        """
        this is the aperture that should contain "all" the light from the star. The strange math at the end ensures
        that the final decimal in the apmax matches that in apmin.
        :return:
        """
        apmax = int(10 * LARGE_APERTURE_FRACTION * self.fwhm + 0.5) / 10.0
        return int(apmax) + self.apmin - int(self.apmin)

    @property
    def number_of_apertures(self):
        """How many aperture between apmin and apmax"""
        return int(self.apmax+0.1 - self.apmin) + 1

    def build_mopheader(self):
        """Build an OSSOS moving object pipeline header.

        mopheader is a short for header used to store some frequently accessed stuff.
        """
        util.exec_prog(['stepZjmp',
                        '-f',
                        self.filename])
        uri = storage.get_uri(ext=MOP_HEADER_EXTENSION, **self.kwargs)
        storage.copy(os.path.basename(uri), uri)
        return os.path.basename(uri)

    def build_bright_star_list(self):
        """
        Build a list of stars that are suitable for make a PSF from.
        :return: filename
        """
        util.exec_prog(['step0jmp',
                        '-f',
                        self.filename,
                        '-w',
                        self.fwhm,
                        '-t',
                        BRIGHT_STAR_THRESHOLD,
                        '-m',
                        MAXIMUM_LINEAR_COUNTS])
        uri = storage.get_uri(ext=BRIGHT_STAR_LIST_EXTENSION, **self.kwargs)
        storage.copy(os.path.basename(uri), uri)
        return os.path.basename(uri)

    def phot(self, star_list, mag_list, apertures=None):
        if apertures is None:
            apertures = self.apmin
        iraf = self.iraf
        iraf.photpars.apertures = apertures
        temp_mag_file = tempfile.NamedTemporaryFile(suffix="mag", delete=False)
        temp_mag_file.close()
        iraf.phot(self.filename,
                  star_list,
                  temp_mag_file)

        iraf.txdump(temp_mag_file, "*", "(SIER==0)&&(CIER==0)&&(PIER==0)&&(MSKY!=INDEF)&&(MSKY<1e5)&&(MSKY>0)",
                    Stdout=mag_list)
        return

    @property
    def bright_mag_list(self):
        try:
            return storage.get_file(ext=BRIGHT_MAG_LIST_EXTENSION, **self.kwargs)
        except IOError as err:
            logging.debug(err)
            return self.build_bright_mag()

    def build_bright_mag(self):
        """
        Compute the magnitudes of the bright stars in this image.
        :return: bright_mag_list
        """
        uri = storage.get_uri(ext=BRIGHT_MAG_LIST_EXTENSION, **self.kwargs)
        bright_mag_list = os.path.basename(uri)
        self.phot(self.bright_star_list, bright_mag_list)
        storage.copy(bright_mag_list, uri)
        return bright_mag_list

    @property
    def datamin(self):
        """
        The minimum good data value based on the being 5 sigma below sky.
        :return:
        """
        mag_table = daophot.Daophot().read(self.bright_mag_list)
        msky = mag_table['MSKY'][good].median()
        msky_std = mag_table['MSKY'][good].std()
        del mag_table
        return msky - 5.0*msky_std

    @property
    def pst_list(self):
        pst_file_name = os.path.basename(self.filename)+".pst"
        os.unlink(pst_file_name)
        self.iraf.pstselect(self.filename, self.bright_mag_list, pst_file_name, 25)
        return pst_file_name


    def build_psf(self):
        logging.info("Builing PSF: {}".format(self.filename))
        # reject stars with any error in the calculation
        # of their photometric magnitude

        previous_fwhm = None
        niters = 0
        self.iraf.datapars.datamin = self.datamin

        while previous_fwhm is None or abs(previous_fwhm - self.fwhm)/self.fwhm > 0.01 and niters < 5:
            niters += 1
            p_fwhm = self.fwhm

            # We should use a minimum aperture that is about 1.1 -1.2 the FWHM
            # This is set at the start of the psf loop so the PSF small aperture
            # matches that used in mkapfile after the PSF is built.
            # We do this twice since the first time we build
            # the PSF with the input FWHM
            # and the second time using the FWHM determined from the PSF stars

            # Adjust the large aperture size too, and the sky fitting parameters
            self.iraf.fitskypars.dannulus = DEFAULT_SKY_DANNULUS * self.apmin
            self.iraf.fitskypars.annulus = self.apmax + 2

            # set the PSF radius and fitting radius
            self.iraf.daopars.fitrad = self.apmin
            self.iraf.daopars.psfrad = self.apmax
            self.iraf.datapars.fwhm = self.fwhm
            self.iraf.datamin = self.datamin
            logging.info("Using fwhm of --> {}".format(self.fwhm))

            ## time to select upto 25 PSF stars, from the good stars we have left.
            logging.info("Selecting upto 25 PSF stars from {}".format(self.bright_mag_list))


            pst2_filename = os.path.splitext(self.filename)[0]+".pst.2"
            psg1_filename = os.path.splitext(self.filename)[0]+".psg.1"
            os.path.unlink(pst2_filename)
            os.path.unlink(psg1_filename)
            os.path.unliin(self.psf_filename)

            psf(t_image, t_image // ".phot", t_image // ".pst.1",
                t_psf,
                t_image // ".pst.2",
                t_image // ".psg.1", showplots -, interactive -)

            # Estimate the FWHM using the gaussian fit to the PSF
            hselect(t_psf, "PAR1", yes) | scan(xsig)
            hselect(t_psf, "PAR2", yes) | scan(ysig)
            hselect(t_psf, "NPSFSTAR", yes) | scan(npsfstar)
            t_fwhm = sqrt((xsig * xsig + ysig * ysig) / 2.0)
            print "Sigma -> " // t_fwhm
            if ( abs((xsig - ysig) / t_fwhm) > 0.2 ) {  #JJK Added a warning message here. March 2002
            touch (t_image // ".psf.WARNING")
            print("ERROR: The PSF is too elliptical for planting. image" // t_image, >> t_image // ".psf.WARNING" )
            }

            ## we must have 10 or more stars to get a good psf...
            if ( npsfstar < 9 ) {
            print("Only " // npsfstar // " where used to build the image, terminating")
            failedflag=1
            goto finalproc
            }

            # for MEGACAM the FWHM is about 84% of the
            # value you would get based on a guassian fit to the PSF
            t_fwhm = 0.84 * t_fwhm * sqrt(8 * log(2))
            print "Got FWHM of " // t_fwhm
            if ( t_fwhm > 8 ) {
            touch (t_image // ".psf.WARNING")
            print ("WARNING: Seeing value large? Measured " // t_fwhm,
            >> t_image // ".psf.WARNING" )
            }
            # keep going until change in t_fwhm between
            # loops is less than 0.05 (5%)
        }  # # Check that the output fwhm is reasonably close to the input one.  # and overwrite the input value with the computed one
        list = t_image // ".fwhm"
        dum = fscan(list, p_fwhm)
        if ( (p_fwhm > t_fwhm * 3.0) | | (t_fwhm > p_fwhm * 3.0) )
        {
        print("PSF change too  large")
        failedflag = 1
        goto
        finalproc

    }
    kdelete(t_image // ".fwhm");
    print (t_fwhm, >> t_image // ".fwhm")  # compute the apcor using the PSF stars.
    kdelete(t_image // ".nst.1");
    kdelete(t_image // ".nrj.1");
    nstar(t_image, t_image // ".psg.1",
    psfimage = t_image // ".psf.fits",
    nstarfile = t_image // ".nst.1",
    rejfile = t_image // ".nrj.1");

    kdelete(t_image // ".phot")
    pselect(t_image // ".nst.1", t_image // ".phot", "CHI < 2.5")  # build a file for use as input into phot
    kdelete(t_image // ".coo.2")
    txdump(t_image // ".nst.1", "XCENTER,YCENTER,MAG,SHARPNESS,ID", "CHI < 2.5", \
           headers +, > t_image // ".coo.2")  # Run PHOT with a range of apertures.
    kdelete(t_image // ".mag.2")
    photpars.apertures = apmin // ":" // apmax // ":1"
    naps = apmax - apmin + 1
    if (naps < 3)
    {


print("inner (" // apmin // ") and outer (" // apmax // ") aperatures must differ by more than 2 pixels\n")
failedflag = 1
goto
finalproc
}
phot(t_image, t_image // ".coo.2", t_image // ".mag.2")  # fit that range of apertures to a curve of growth.
kdelete(t_image // ".mkap")

failedflag = 1
iferr
{mkapfile(t_image // ".mag.2", naps, t_image // ".mkap", interactive -, verify -, nparams=t_order)}
goto
finalproc

failedflag = 0  # ## Read in the aperture correction file and convert to CFEPS Pipeline format
list = t_image // ".mkap"
line = apmin // " " // apmax // " 30 30 "  # the last line has the actual correction

ac = 0
while ( fscan(list, line) != EOF )
    ac += 1

if ( ac != 3 )  {
print ("Invalid mkapfile output? (check " // t_image // ".mkap)")
failedflag=1
goto finalproc
}


# Stick the apcor line in the way the pipeline wants it
apcor = -30
aperr = 30
wc = fscan(line, s1, apcor, aperr)
kdelete(t_image // ".apcor")
apcor = -1 * apcor
printf("%.1f %.1f %6.2f %6.2f\n", apmin, apmax, apcor, aperr,
>> t_image // ".apcor" )


# # spit out some warnings about apcor, if its a bit off.
if ( apmin < 2) {
touch ( t_image // ".apcor.WARNING" )
print ("WARNING: apmin small: " // apmin, >> t_image // ".apcor.WARNING")
}
if ( apmax > 35) {
touch ( t_image // ".apcor.WARNING" )
print ("WARNING: apmax huge: " // apmax, >> t_image // ".apcor.WARNING")
}
if ( apcor > 1.0 ) {
touch ( t_image // ".apcor.WARNING" )
print ("ERROR: apcor large ", >> t_image // ".apcor.WARNING")
}
if ( aperr > 0.2 ) {
touch ( t_image // ".apcor.WARNING" )
print ("ERROR: aperr large ", >> t_image // ".apcor.WARNING")
}

finalproc:

if ( t_keep_files )  {
print("Keeping intermediate files")
} else {
delete(t_image // ".bright.mag", verify-)
delete(t_image // ".mag*", verify-)
delete(t_image // ".pst*", verify-)
delete(t_image // ".psg*", verify-)
delete(t_image // ".coo*", verify-)
delete(t_image // ".nrj*", verify-)
delete(t_image // ".nst*", verify-)
}

if ( failedflag == 0 ) {
if ( imaccess(t_psf) ) {
print("\n" // t_psf // " OK\n")
touch (t_image // ".jmpmakepsf.OK")
} else {
print("\n NO PSF BUILT \n")
failedflag = 1
pwd | scan(t_base)
}
}
if (failedflag != 0) {
print("\n jmpmakepsf: FAILED in " // t_base // " for " // t_image // "\n", >> t_image // ".jmpmakepsf.FAILED")
print "failed to build psf"
}

end

