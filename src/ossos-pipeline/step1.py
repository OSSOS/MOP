#!/usr/bin/env python
"""step1 is to run the two source finding algorithms in the image.

step1jmp is a stand-alone fortran code from Jean-Marc Petit et al.
step1matt is a script from M. Holman that runs E. Bertain's sExtractor.

"""


import argparse
import logging
import os
from ossos import storage
from ossos import util
from astropy.io import fits

def step1(expnum,
              ccd,
              prefix='',
              version='p',
              fwhm=4, 
              sex_thresh=1.3, 
              wave_thresh=2.7, 
              maxcount=30000):
    """run the actual step1jmp/matt codes.

    expnum: the CFHT expousre to process
    ccd: which ccd in the mosaic to process
    fwhm: the image quality, FWHM, of the image.  In pixels.
    sex_thresh: the detection threhold to run sExtractor at
    wave_thresh: the detection threshold for wavelet
    maxcount: saturation level

    """

    filename = storage.get_image(expnum, ccd, version=version, prefix=prefix)
    mopheader = storage.get_image(expnum, ccd, version=version,
                                  ext='mopheader', prefix=prefix)
    fwhm = storage.get_fwhm(expnum, ccd, prefix=prefix, version=version)
    basename = os.path.splitext(filename)[0]
    
    outfile = util.exec_prog(['step1jmp', 
                              '-f', basename,
                              '-t', str(wave_thresh),
                              '-w', str(fwhm),
                              '-m', str(maxcount)])
    
    obj_uri = storage.get_uri(expnum,ccd,version=version,ext='obj.jmp',
                              prefix=prefix)
    obj_filename = basename+".obj.jmp"

    storage.copy(obj_filename,obj_uri)

    ## for step1matt we need the weight image
    flat_name = fits.open(filename)[0].header['FLAT']
    flat_name = flat_name[0:-5]
    flat_filename = storage.get_image(flat_name, ccd, version='', ext='fits',
                      subdir='calibrators', rescale=False)
    if not os.access('weight.fits',os.R_OK):
        os.symlink(flat_filename, 'weight.fits')
    outfile = util.exec_prog(['step1matt',
                              '-f', basename,
                              '-t', str(sex_thresh),
                              '-w', str(fwhm),
                              '-m', str(maxcount)])

    obj_uri = storage.get_uri(expnum,ccd,version=version,ext='obj.matt',
                              prefix=prefix)
    obj_filename = basename+".obj.matt"

    storage.copy(obj_filename,obj_uri)

    return True

if __name__=='__main__':
    ### Must be running as a script

    parser=argparse.ArgumentParser(
        description='Run step1jmp and step1matt on a given exposure.')

    parser.add_argument("--ccd","-c",
                        action="store",
                        default=None,
                        type=int,
                        dest="ccd")
    parser.add_argument("--fk", help="add the fk prefix on processing?",
                        default=False,
                        action='store_true')
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("--sex_thresh",
                        action="store",
                        type=float,
                        default=1.3,
                        help="sExtractor detection threhold")
    parser.add_argument("--wavelet_thresh",
                        type=float,
                        default=2.7,
                        help="Wavelet detection threhold")
    parser.add_argument("expnum",
                        type=int,
                        nargs='+',
                        help="expnum(s) to process")
    parser.add_argument("--version",
                        action='version',
                        version='%(prog)s 1.0')
    parser.add_argument('--type', default='p',
                        choices=['o','p','s'], help="which type of image")
    parser.add_argument("--verbose","-v",
                        action="store_true")
    parser.add_argument("--debug",'-d',
                        action='store_true')
    parser.add_argument("--force", action="store_true")

    args=parser.parse_args()

    level = logging.CRITICAL
    if args.debug:
        level = logging.DEBUG
    elif args.verbose:
        level = logging.INFO
    
    logging.basicConfig(level=level, format="%(message)s")
        
    storage._dbimages = args.dbimages

    if args.ccd is None:
        ccdlist = range(0,36)
    else:
        ccdlist = [args.ccd]

    prefix = ( args.fk and 'fk') or ''


    for expnum in args.expnum:
        for ccd in ccdlist:
            try:
                message = storage.SUCCESS
                if not storage.get_status(expnum, ccd, 'mkpsf'):
                    raise IOError(35, "mkpsf hasn't run?")
                if storage.get_status(expnum, ccd, prefix+'step1') and not args.force:
                    logging.critical(
                        "Already did %s %s, skipping" %(prefix+str(expnum),
                                                        str(ccd)))
                    continue
                logging.info("step1 on expnum: %s, ccd: %s" % (
                    prefix+str(expnum), str(ccd)))
                step1(expnum, ccd, prefix=prefix, version=args.type)
            except Exception as e:
                message = str(e)
                logging.error("Error running step1: %s " %(message))

            storage.set_status(expnum,
                               ccd,
                               prefix+'step1',
                               message)
        
            
