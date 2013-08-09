#!/usr/bin/env python

import vos
import logging
import argparse
import sys
from ossos import storage
import os
import time
from cStringIO import StringIO

success_parts = {
    'update_header': {'keywords': ['PHOTREF'], 'ccds': [36]},
    'step1': {'version': 'p', 'exts': ['obj.jmp', 'obj.matt'], 'files': [] },
    'step2': {'version': 'p', 'exts': ['unid.jmp', 'unid.matt', 'trans'], 'files': []},
    'step3': {'version': 'p', 'exts': ['moving.matt', 'moving.jmp'], 'files': []},
    'mkpsf': {'version': 'p', 'exts': ['zeropoint.used', 'psf.fits', 'apcor', 'phot'], 'files': []},
    'plant': {'version': 's', 'files': ['Object.planted', 'shifts'],
              'exts': ['fits','apcor','fwhm', 'mopheader','zeropoint.used']},
    'combine': {}
}


def checker(expnum, ccd, command):
    """
    Check that step1 succeeded by looking for the outputs of step1.

    Check that .obj.jmp .obj.matt exist

    :param expnum: the integer exposure number to check.
    :param ccd: ccd to check.
    """
    version = success_parts[command].get('version', 'p')
    extensions = success_parts[command].get('exts', [])
    files = success_parts[command].get('files',[])
    keywords = success_parts[command].get('keywords', [])

    success = True
    for ext in extensions:
        uri = storage.get_uri(expnum,ccd,version='p',ext=ext)
        success = success and storage.exists(uri)

    for filename in files:
        uri = storage.get_uri(expnum=filename, ccd=None, version=None, ext=None,
                              subdir='{0:s}/ccd{1:02d}'.format(str(expnum), ccd))
        success = success and storage.exists(uri)

    if len(keywords) == 0 :
        return success
    filename = storage.DBIMAGES+'/{0}/{0}{1}.fits'.format(expnum, version)

    hdulist = storage.fits.open(StringIO(storage.vofile(filename, os.O_RDONLY).read(2880*100)))

    for keyword in keywords:
        success = hdulist[1].header.get(keyword, False) and True

    return success



vospace = vos.Client()

TRIPLETS_NODE = 'vos:OSSOS/triplets'

MOP_COMMAND_ORDER = {
    'LEAD': ['update_header', 'mkpsf', 'step1', 'step2', 'step3', 'combine'],
    'OTHER': ['update_header', 'mkpsf', 'step1']
}



def get_triplets(triplets_dir=TRIPLETS_NODE):
    triplets = []
    for filename in vospace.listdir(triplets_dir):
        line = ''
        for char in storage.vofile(os.path.join(triplets_dir, filename), os.O_RDONLY).read():
            line += char
            if char == '\n':
                triplets.append(line.strip().split())
                line = ''
    return triplets



def main(expnums=[]):

    triplets = get_triplets()
    leads = []
    for triple in triplets:
        if len(triple) > 0:
            leads.append(int(triple[0]))

    for expnum in expnums:
        exp_type = ( expnum in leads and 'LEAD' ) or 'OTHER'
        tags = storage.get_tags(expnum, force=True)
        for command in MOP_COMMAND_ORDER[exp_type]:
            for ccd in success_parts[command].get('ccds',range(0,36)):
                tag = storage.tag_uri(storage.get_process_tag(command, ccd))
                print expnum, ccd, command, tags.get(tag, None)
                if tags.get(tag, None) != storage.SUCCESS:
                    try:
                        if checker(expnum, ccd, command):
                            storage.set_status(expnum, ccd, command, status=storage.SUCCESS)
                    except Exception as e:
                        sys.stderr.write(str(e))
                        pass
                sys.stdout.flush()

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("expnums", nargs='*', help="a list of exposure numbers to check")
    args = parser.parse_args()

    expnums = args.expnums
    if len(expnums) == 0:
        expnums = storage.list_dbimages()
    main(expnums)


