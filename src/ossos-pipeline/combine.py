#!/usr/bin/env python


import os
from ossos import storage
from ossos import util
from ossos import mop_file
import argparse
import logging

MEASURE3='vos:OSSOS/measure3/'

def combine(expnum, ccd, prefix=None, type='p', field=None, measure3=MEASURE3 ):

    if field is None:
        field=str(expnum)

    field += "_%s" % ( str(ccd))

    for ext in ['moving.matt','moving.jmp']:
        fname = storage.get_image(expnum,
                                  ccd=ccd,
                                  prefix=prefix,
                                  version=type, ext=ext)
    if prefix is not None and len(prefix) > 0:
        planted = storage.get_image('Object',
                                    subdir=str(expnum)+"/ccd%s" % (
            str(ccd).zfill(2)),
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
               'matt.found', 'matt.missed' ]:
        uri = storage.get_uri(expnum,
                              ccd=ccd,
                              prefix=prefix,
                              version=type,
                              ext=ext)
        filename = os.path.basename(uri)
        if not os.access(filename,os.R_OK):
            logging.critical("No %s file" % (filename))
            continue
        vospace_name = "%s.%s" % ( field, ext ) 
        storage.copy(filename, os.path.join(uri, vospace_name))

    base_name = prefix+str(expnum)+type+str(ccd).zfill(2)
    cands_file = base_name+'.cands.comb'
    
    if not os.access(cands_file,os.R_OK):
        nocands_file = ( prefix+
                         str(expnum)+
                         type+
                         str(ccd).zfill(2)+
                         '.no_candidates' )
        open(nocands_file, 'w').close()
        vospace_name = "%s.no_candidates" % ( field ) 
        storage.copy(nocands_file,os.path.join(measure3, vospace_name))

        return storage.SUCCESS


    cands_file = mop_file.Parser().parse(cands_file)
    for file_id in cands_file.header.file_ids:
        rec_no=cands_file.header.file_ids.index(file_id)
        storage.get_image(expnum=cands_file.header.keywords['EXPNUM'][rec_no],
                          ccd=ccd,
                          version=type,
                          prefix=prefix,
                          ext='fits')

    cmd_args = ['measure3', prefix+str(expnum)+type+str(ccd).zfill(2)]
    util.exec_prog(cmd_args)
    
    filename=base_name+".measure3.cands.astrom"
    vospace_filename = "%s.measure3.cands.astrom" % ( field)
    storage.copy(filename, os.path.join(measure3,vospace_filename))
    return storage.SUCCESS



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
    parser.add_argument("--field",
                        action="store",
                        default=None,
                        help="Name of the field being combined")
    parser.add_argument("expnum",
                        type=int,
                        help="expnum to process")
    parser.add_argument("measure3",
                        action="store",
                        help="VOSpace location for measure3 files",
                        default=MEASURE3)
    parser.add_argument('--type', default='p', choices=['o','p','s'], help="which type of image")
    parser.add_argument("--verbose","-v",
                        action="store_true")
    parser.add_argument("--debug",'-d',
                        action='store_true')
    parser.add_argument("--force", '-f', action='store_true')
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
        message = storage.SUCCESS
        try:
            if not storage.get_status(args.expnum, ccd, prefix+'step3'):
                logging.error(storage.get_status(
                        args.expnum,
                        ccd,
                        'step3',
                        return_message=True))
                raise IOError(35, "need to run step3 first")
            if storage.get_status(args.expnum, ccd, prefix+'combine') and not args.force:
                continue
            message = combine(args.expnum, 
                              ccd, 
                              prefix=prefix, 
                              type=args.type, 
                              field=args.field, 
                              measure3=args.measure3
                              )
        except Exception as e:
            message = str(e)

        storage.set_status(args.expnum, ccd, prefix+'combine', message)
