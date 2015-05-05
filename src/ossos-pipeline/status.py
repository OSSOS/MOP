#!python

import vos
import logging
import argparse
import sys
from ossos import storage
import os
import time


def main(expnum):
   
    commands= ['mkpsf', 
               'step1', 
               'step2', 
               'step3', 
               'combine']

    tags = storage.get_tags(expnum)
    for ccd in range(36):
        for command in commands:
            tag = storage.tag_uri(storage.get_process_tag(command, ccd, 'p'))
            if tags.get(tag, None) != storage.SUCCESS:
                sys.stderr.write("FAILED: {} {} {}\n".format(expnum, ccd, command))
                continue
            tag = storage.tag_uri(storage.get_process_tag('fk'+command, ccd, 's'))
            if tags.get(tag, None) != storage.SUCCESS:
                sys.stderr.write("FAILED: {} {} {}\n".format(expnum, ccd, command))
                continue
            
if __name__=='__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("expnum", help="which exposure to check")
    args = parser.parse_args()

    main(args.expnum)


