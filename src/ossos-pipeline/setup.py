from setuptools import setup, find_packages
import sys

if sys.version_info[0] > 2:
    print 'The caom2 package is only compatible with Python version 2.n'
    sys.exit(-1)


setup(name='ossos',
      version=0.1,
      url='https://github.com/ijiraq/MOP',
      description="Outer Solar System Origins Survey (OSSOS) Pipeline",
      packages=['ossos'],
      install_requires=['distribute'],
      scripts=['mkpsf.py',
               'step1.py',
               'step2.py',
               'step3.py',
               'update_header.py',
               'scramble.py',
               'plant.py',
               'preproc.py',
               'combine.py'],
      classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'Intended Audience :: End Users/Desktop',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: GNU General Public License v3',
        'Operating System :: POSIX',
        'Programming Language :: Python',
        ],    
)
