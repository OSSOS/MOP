#!/usr/bin/env python


import os
from ossos import storage
from ossos import util
import argparse
import logging

def combine(expnum, ccd, prefix=None, type='p'):

    for ext in ['moving.matt','moving.jmp']:
        fname = storage.get_image(expnum, ccd=ccd, prefix=prefix, version=type, ext=ext)
    if prefix is not None and len(prefix) > 0:
        planted = storage.get_image('Object',
                                    subdir=str(expnum)+"/ccd%s" % ( str(ccd).zfill(2)),
                                    version='',
                                    ext='planted')
    else:
        prefix = ''

    base_image = os.path.basename( 
        storage.get_uri(expnum,
                        ccd=ccd,
                        prefix=prefix,
                        version=type,
                        ext=None))


    
    cmd_args = ['comb-list', prefix+str(expnum)+type+str(ccd).zfill(2)]
    util.exec_prog(cmd_args)

    for ext in['cands.comb', 'comb.found', 'comb.missed',
               'jmp.found', 'jmp.missed',
               'matt.found', 'matt.missed']:
        uri = storage.get_uri(expnum,
                              ccd=ccd,
                              prefix=prefix,
                              version=type,
                              ext=ext)
        filename = os.path.basename(uri)
        if not os.access(filename,os.R_OK):
            logging.critical("No %s file created." % (filename))
            continue
        storage.copy(filename, uri)

    if os.access('no_candidates',os.R_OK):
        uri = storage.get_uri(expnum,
                              ccd=ccd,
                              version=type,
                              ext='no_candidates')
        storage.copy('no_candidates', uri)
        return 'no_candidates'
    
    
    cmd_args = ['measure3', prefix+str(expnum)+type+str(ccd).zfill(2)]
    util.exec_prog(cmd_args)
    prefix+str(expnum)+type+str(ccd).zfill(2)+".measure3.cands.astrom"
    storage.copy(filename, 'vos:OSSOS/measure3/'+filename)
    return 'measure3'



if __name__=='__main__':
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
    parser.add_argument("expnum",
                        type=int,
                        help="expnum to process")
    parser.add_argument('--type', default='p', choices=['o','p','s'], help="which type of image")
    parser.add_argument("--verbose","-v",
                        action="store_true")
    parser.add_argument("--debug",'-d',
                        action='store_true')
    
    args=parser.parse_args()

    level = logging.CRITICAL
    if args.debug:
        level = logging.DEBUG
    elif args.verbose:
        level = logging.INFO
    
    logging.basicConfig(level=level, format="%(message)s")
        
    storage._dbimages = args.dbimages

    prefix = ( args.fk and 'fk' ) or ''

    ccdlist = ( args.ccd is None and range(0,36) ) or [args.ccd]

    for ccd in ccdlist:
        if not storage.get_status(args.expnum, ccd, 'step3'):
            raise IOError(35, "need to run step3 first")
        message = combine(args.expnum, ccd, prefix=prefix, type=args.type)
        storage.set_status(args.expnum, ccd, 'combine', message)
