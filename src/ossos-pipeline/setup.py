import os

from distutils.core import setup
import sys

if sys.version_info[0] > 2:
    print 'The MOP package is only compatible with Python version 2.7+'
    sys.exit(-1)

# # Build the list of scripts to be installed.
script_dir = 'pipeline'
scripts = []
for script in os.listdir(script_dir):
    if script[-1] in ["~", "#"]:
        continue
    scripts.append(os.path.join(script_dir, script))
scripts.append('validate.py')

version = "0.3.2"

setup(name='ossos',
      version=version,
      url='http://github.com/OSSOS/MOP',
      description="Outer Solar System Origins Survey (OSSOS) Pipeline",
      package_data={'ossos': ['gui/*.json']},
      requires=['pyraf', 'astropy (==0.2.5)', 'vos', 'pyephem', 'requests', 'pyOpenSSL', 'numpy', 'wxPython', 'pyds9',
                'matplotlib', 'Polygon2'],
      scripts=scripts,
      packages=['ossos',
                'ossos/fitsviewer',
                'ossos/downloads', 'ossos/downloads/cutouts',
                'ossos/gui', 'ossos/gui/models', 'ossos/gui/views',
                'planning', 'planning/plotting',
      ],
)
