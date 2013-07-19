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

        
def main(command, launch=False, triplets_dir=TRIPLETS_NODE):
    
    for line in get_triplets(triplets_dir):
        for expnum in line[0:3]:
            for ccd in range(36):
                if not storage.get_status(expnum, ccd, command):
                    print command, expnum, ccd, storage.get_status(expnum, ccd, command, return_message=True)
                    if launch:
                        submit(command, expnum, ccd)


def submit(command, expnum, ccd):
    job_id = "_".join([str(int(time.time()*1000)),str(expnum),str(ccd),command])
    os.system("./submit_job.sh %s %s.py %s --ccd %s --verbose" % ( job_id, command, expnum, ccd))


if __name__=='__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("command", help="which command to check the status of")
    parser.add_argument("--launch", default=False, action='store_true')
    args = parser.parse_args()

    main(args.command, args.launch)


