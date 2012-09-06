#!/usr/bin/env python

from distutils.core import setup

setup(name="JJpython",
      version="1.0",
      description="JJ's Python modules at the CADC",
      author="JJ Kavelaars",
      author_email="jjk@hia.nrc.ca",
      url="http://salish.dao.nrc.ca/~kavelaar/",
      py_modules=['MOPfiles','MOPdbaccess', ],
      scripts=['center.py']
      )
