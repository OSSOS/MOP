import os
import sys
from ossos.__version__ import version

from setuptools import setup, find_packages

dependencies = ['pyraf >= 2.1.1',
                    'astropy >= 0.2.5',
                    'vos >= 2.0',
                    'ephem',
                    'requests >= 2.7',
                    'pyOpenSSL',
                    'numpy >= 1.6.1',
                    'wxPython-common > 2.8.1',
                    'matplotlib',
                    'Polygon2',
                    'd2to1 >= 0.2.10', 'scipy', 'uncertainties', 'pyds9 >= 1.8'
                        ]


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
      version=version,
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
      dependency_links=['git+https://github.com/ericmandel/pyds9.git#egg=pyds9-1.8'],
      install_requires=dependencies,
      scripts=scripts,
      packages=find_packages(exclude=['tests',])
      )
