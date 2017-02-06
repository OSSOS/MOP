import os
import sys
from ossos.__version__ import version

from setuptools import setup, find_packages

dependencies = ['pyraf >= 2.1.1',
                'requests >= 2.7',
                'astropy >= 0.2.5',
                'vos >= 2.0',
                'ephem',
                'numpy >= 1.6.1',
                'wxPython >= 3.0.0.0',
                'matplotlib',
                'Polygon2',
                'd2to1 >= 0.2.10',
                'scipy',
                'uncertainties',
                'pyds9 >= 1.8',
                'mp_ephem']


if sys.version_info[0] > 2:
    print 'The MOP package is only compatible with Python version 2.7+, not yet with 3.x'
    sys.exit(-1)

# # Build the list of tools and scripts to be installed.
#script_dirs = ['tools', 'scripts']
#scripts = []
#for script_dir in script_dirs:
#    for script in os.listdir(script_dir):
#        if script[-1] in ["~", "#"]:
#           continue
#        scripts.append(os.path.join(script_dir, script))

console_scripts = [ 'mkpsf = ossos.pipeline.mkpsf:main', 'step1 = ossos.pipeline.step1:main', 'mk_mopheader = ossos.pipeline.mk_mopheader:main' ]
gui_scripts = [ 'validate = ossos.tools.validate:main' ]

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
      entry_points = { 'console_scripts': console_scripts,
                       'gui_scripts': gui_scripts },
      packages=find_packages(exclude=['tests',])
      )
