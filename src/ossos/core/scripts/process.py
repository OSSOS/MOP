

from ossos.pipeline import mk_mopheader, mkpsf, step1, slow
from ossos import util, storage
import logging
import sys
import os
import shutil

util.config_logging(logging.INFO)

version='p'
force=False
dry_run=False
prefix=''

lines = open(sys.argv[1]).readlines()
basedir=os.getcwd()


for line in lines:
    expnum = int(line.strip())
    for ccd in storage.get_ccdlist(expnum):
        try:
            os.chdir(basedir)
            if not os.access(str(expnum),os.F_OK):
                os.mkdir(str(expnum))
            os.chdir(str(expnum))
            if not os.access(str(ccd), os.F_OK):
                os.mkdir(str(ccd))
            os.chdir(str(ccd))
            try:
                print(os.getcwd())
                mk_mopheader.run(expnum, ccd=ccd, version=version, dry_run=dry_run, prefix='', force=force, ignore_dependency=False)
                mkpsf.run(expnum, ccd=ccd, version=version, dry_run=dry_run, prefix=prefix, force=force)
                step1.run(expnum, ccd=ccd, version=version, dry_run=dry_run, prefix=prefix, force=force)
                slow.run(expnum, ccd, version=version, dry_run=dry_run, prefix=prefix, force=force)
            except Exception as ex:
                print(ex)
        except Exception as ex:
            print(ex)
        finally:
            os.chdir(basedir)
            shutil.rmtree("{}/{}".format(expnum, ccd), ignore_errors=True)
