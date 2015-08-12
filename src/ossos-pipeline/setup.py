import os
from distutils.core import setup
import sys

if sys.version_info[0] > 2:
    print 'The MOP package is only compatible with Python version 2.7+, not yet with 3.x'
    sys.exit(-1)

# # Build the list of scripts to be installed.
script_dir = 'pipeline'
scripts = []
for script in os.listdir(script_dir):
    if script[-1] in ["~", "#"]:
        continue
    scripts.append(os.path.join(script_dir, script))
scripts.append('validate.py')

setup(name='ossos',
      version="0.3.3",
      url='http://github.com/OSSOS/MOP',
      author='''JJ Kavelaars (jjk@uvic.ca),
              Brett Gladman (gladman@astro.ubc.ca),
              Jean-Marc Petit (jmpetit@obs-besancon.fr),
              Matt Holman (holman@cfa.harvard.edu),
              Hans Scholl,
              Michele Bannister (micheleb@uvic.ca),
              David Rusk''',
      maintainer='M Bannister and JJ Kavelaars',
      maintainer_email='jjk@uvic.ca',
      description="Outer Solar System Origins Survey (OSSOS) Pipeline",
      long_description='See http://www.ossos-survey.org/ for science details.',
      classifiers=['Intended Audience :: Science/Research',
                   'Topic :: Scientific/Engineering :: Astronomy',
                   'Development Status :: 4 - Beta',
                   'Programming Language :: Python :: 2 :: Only',
                   'Operating System :: MacOS :: MacOS X',
                   'Environment :: X11 Applications',
                   'License :: OSI Approved :: GNU General Public License (GPL)',
      ],
      package_data={'ossos': ['gui/*.json']},
      install_requires=['pyraf >= 2.1.1',
                        'astropy >= 0.2.5',
                        'vos >= 1.11.3',
                        'pyephem',
                        'requests',
                        'pyOpenSSL',
                        'numpy >= 1.6.1',
                        'wxPython >= 2.8.12.1',
                        'pyds9',
                        'matplotlib',
                        'Polygon2',
                        'd2to1 >= 0.2.10', 'scipy',
      ],
      scripts=scripts,
      packages=['ossos',
                'ossos/fitsviewer',
                'ossos/downloads', 'ossos/downloads/cutouts',
                'ossos/gui', 'ossos/gui/models', 'ossos/gui/views',
                'planning', 'planning/plotting',
      ],
)
