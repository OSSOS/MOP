#!/usr/bin/env python
"""step1 is to run the two source finding algorithms in the image.

step1jmp is a stand-alone fortran code from Jean-Marc Petit et al.
step1matt is a script from M. Holman that runs E. Bertain's sExtractor.

"""


import argparse
import logging
import os
import ossos

def run_step1(expnum,
              ccd, 
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

    filename = ossos.get_image(expnum, ccd, version='p')
    fwhm = ossos.get_fwhm(expnum, ccd)

    outfile = ossos.exec_prog(['step1jmp', 
                              '-f %s' % (filename),
                              '-t %f'  % (wave_thresh),
                              '-w %f ' % (fwhm),
                              '-m %f' % (maxcount)])
    
    obj_uri = ossos.dbimages_uri(expnum,ccd,version='p',ext='obj.jmp')
    obj_filename = os.path.splitext(filename)[0]+".obj.jmp"

    ossos.copy(obj_filename,obj_uri)

    outfile = ossos.exec_prog(['step1matt',
                               '-f %s' % (filename),
                               '-t %f' % (sex_thresh),
                               '-w %f' % (fwhm) ,
                               '-m %f' % (maxcount)])

    obj_uri = ossos.dbimages_uri(expnum,ccd,version='p',ext='obj.matt')
    obj_filename = os.path.splitext(filename)[0]+".obj.matt"

    ossos.copy(obj_filename,obj_uri)

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
    parser.add_argument("--verbose","-v",
                        action="store_true")

    args=parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    ossos._dbimages = args.dbimages

    if not args.ccd:
        ccdlist = range(0,36)
    else:
        ccdlist = [args.ccd]

    for expnum in args.expnum:
        for ccd in ccdlist:
            logging.info("step1 on expnum :%d, ccd: %d" % ( expnum, ccd))
            run_step1(expnum, ccd)
             
