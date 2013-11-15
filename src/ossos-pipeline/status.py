#!/usr/bin/env python

import vos
import logging
import argparse
import sys
from ossos import storage
import os
import time

vospace = vos.Client()

TRIPLETS_NODE = 'vos:OSSOS/triplets'


def get_triplets(triplets_dir=TRIPLETS_NODE):
    
    triplets = []
    for filename in vospace.listdir(triplets_dir):
        line = ''
        for char in vospace.open(os.path.join(triplets_dir, filename), view='data').read():
            line += char
            if char=='\n':
                triplets.append(line.strip().split())
                line = ''
    return triplets

        
def main(commands=['update_header', 'mkpsf', 'step1', 'step2', 'step3', 'combine'], 
         ccds=range(0,36),
         launch=False, triplets_dir=TRIPLETS_NODE,
         delete=False):
   
    cmd_order={'LEAD': ['update_header', 'mkpsf', 'step1', 'step2', 'step3', 'combine'],
               'OTHER': ['update_header', 'mkpsf', 'step1' ] }
 
    existance = {'mkpsf': ['psf.fits', ],
                 'step1': ['obj.jmp', 'obj.matt'],
                 'step2': ['unid.jmp', 'unid.matt'],
                 'step3': ['cands.jmp', 'cands.matt']}

    for line in get_triplets(triplets_dir):
        for command in commands:        
            exp_type = 'LEAD'
            for expnum in line[0:3]:
                tags = storage.get_tags(expnum)
                if command not in cmd_order[exp_type]:
                    continue
                for ccd in ccds:
                    tag = storage.tag_uri(storage.get_process_tag(command, ccd))
                    print line[3], command, expnum, ccd, tags.get(tag,None)
                    if tags.get(tag,None) != storage.SUCCESS:
                        if launch:
                            submit(command, expnum, ccd)
                        if delete:
                            success=True
                            for ext in existance[command]:
                                uri = storage.dbimages_uri(expnum, ccd, version='p', ext=ext)
                                if not storage.exists(uri):
                                    success=False
                            print expnum, ccd, success
                            #for next_command in cmd_order[exp_type][commands.index(command)+1:]:
                            #    storage.set_status(expnum, ccd, next_command, command+" FAILED")
                        if success:
                            storage.set_status(expnum, ccd, command, version='p', status='success')
                exp_type='OTHER'


def submit(command, expnum, ccd):
    job_id = "_".join([str(int(time.time()*1000)),str(expnum),str(ccd),command])
    os.system("./submit_job.sh %s %s.py %s --ccd %s --verbose" % ( job_id, command, expnum, ccd))


if __name__=='__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("command", nargs='*', help="which command to check the status of", default=['mkpsf','step1'] )
    parser.add_argument("--launch", default=False, action='store_true')
    parser.add_argument("--ccds", nargs='*', help="which CCD should command be tagged on", default=range(0,36))
    args = parser.parse_args()

    main(args.command, args.ccds, args.launch, delete=True)


