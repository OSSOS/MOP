import glob
import os

from setuptools import setup, find_packages
import sys

if sys.version_info[0] > 2:
    print 'The MOP package is only compatible with Python version 2.7+'
    sys.exit(-1)


#import astropy
#from astropy.setup_helpers import (register_commands, adjust_compiler,
#                                   filter_packages, update_package_files,
#                                   get_debug_option)
#from astropy.version_helpers import get_git_devstr, generate_version_py
#version = "0.1dev"+get_git_devstr(False)
version = "0.2"

setup(name='ossos',
      version=version,
      url='https://github.com/ijiraq/MOP',
      description="Outer Solar System Origins Survey (OSSOS) Pipeline",
      packages=['ossos', 'ossos/gui', 'ossos/fitsviewer','ossos/downloads', 'ossos/downloads/cutouts', 'ossos/gui/models', 'ossos/gui/views' ],
      install_requires=[ 'pyraf', 'distribute', 'astropy', 'vos', 'pyephem'],
      scripts=['mkpsf.py',
               'step1.py',
               'step2.py',
               'step3.py',
               'update_header.py',
               'scramble.py',
               'plant.py',
               'preproc.py',
               'planted_mag_check.py',
               'validate.py',
               'ObsStatus.py',
               'combine.py', ],
)
