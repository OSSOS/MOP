import re
from setuptools import setup, find_packages

VERSION_FILENAME = "ossos/version.py"
line = open(VERSION_FILENAME, "rt").read()
VERSION_RE = r"^__version__ = ['\"]([^'\"]*)['\"]"
mo = re.search(VERSION_RE, line, re.M)
if mo:
    version_string = mo.group(1)
else:
    raise RuntimeError("Unable to find version string in %s." % (VERSION_FILENAME,))

dependencies = ['requests >= 2.7',
                'astropy >= 4.0',
                'vos >= 3.3.2',
                'numpy >= 1.6.1',
                'matplotlib',
                'd2to1 >= 0.2.10',
                'scipy',
                'uncertainties',
                'pyds9 >= 1.8',
                'wxPython > 4.0',
                'pypubsub > 4.0',
                'mp_ephem']

console_scripts = ['mkpsf = ossos.pipeline.mkpsf:main', 'step3 = ossos.pipeline.step3:main',
                   'step2 = ossos.pipeline.step2:main', 'step1 = ossos.pipeline.step1:main',
                   'combine = ossos.pipeline.combine:main',
                   'mk_mopheader = ossos.pipeline.mk_mopheader:main',
                   'optimize_pointings = ossos.planning.optimize_pointings:main',
                   'build_astrometry_report = ossos.pipeline.build_astrometry_report:main',
                   'update_astrometry = ossos.pipeline.update_astrometry:main',
                   'measure3 = ossos.pipeline.measure3:main',
                   'align = ossos.pipeline.align:main',
                   'plant = ossos.pipeline.plant:main',
                   'astrom_mag_check = ossos.pipeline.astrom_mag_check:main',
                   'scramble = ossos.pipeline.scramble:main']

gui_scripts = ['validate = ossos.tools.validate:main']

setup(name='ossos',
      version=version_string,
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
      # scripts=["scripts/gui",],
      entry_points={'console_scripts': console_scripts,
                    'gui_scripts': gui_scripts},
      packages=find_packages(exclude=['tests', ])
      )
