from setuptools import setup

requires = [
    'pyramid',
    'zope.cachedescriptors',
]
namespace_packages=['web']
setup(name='web.field_obs',
      entry_points="""\
      [paste.app_factory]
      main = web.overview:main
      """,
)

