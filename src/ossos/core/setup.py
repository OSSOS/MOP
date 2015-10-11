import os
from setuptools import setup, find_packages
import sys
from optparse import OptionParser

parser=OptionParser()
parser.add_option('--skip_dependencies',
                  action='store_true', dest='we_trust_you',
                  default=False,
                  help='Install assuming you have already installed the necessary dependencies, '
                       'and they are all available in your paths. '
                       'DEFAULT=%default.')
# e.g. pip install ossos -t /Users/username/mybin/ --global-option="--skip_dependencies"
(opt,args) = parser.parse_args()

if not opt.skip_dependencies:
    dependencies = ['pyraf >= 2.1.1',
                    'astropy >= 0.2.5',
                    'vos >= 2.0',
                    'pyephem',
                    'requests >= 2.7',
                    'pyOpenSSL',
                    'numpy >= 1.6.1',
                    'wxPython >= 2.8.12.1',
                    'pyds9',
                    'matplotlib',
                    'Polygon2',
                    'd2to1 >= 0.2.10', 'scipy', 'uncertainties',
                        ]
else:
    dependencies = []


if sys.version_info[0] > 2:
    print 'The MOP package is only compatible with Python version 2.7+, not yet with 3.x'
    sys.exit(-1)

# # Build the list of scripts to be installed.
script_dir = 'tools'
scripts = []
for script in os.listdir(script_dir):
    if script[-1] in ["~", "#"]:
        continue
    scripts.append(os.path.join(script_dir, script))

setup(name='ossos',
      version="0.4.1",
      url='http://github.com/OSSOS/MOP',
      author='''JJ Kavelaars (jjk@uvic.ca),
              Michele Bannister (micheleb@uvic.ca),
              David Rusk''',
      maintainer='M Bannister and JJ Kavelaars',
      maintainer_email='jjk@uvic.ca',
      description="Outer Solar System Origins Survey (OSSOS)",
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
      install_requires=dependencies,
      scripts=scripts,
      packages=find_packages(exclude="tests*")
      )
