from setuptools import setup, find_packages

requires = [
    'pyramid',
    'zope.cachedescriptors',
]
namespace_packages = ['web']
setup(name='web.field_obs',
      namespace_packages=namespace_packages,
      packages=find_packages(),
      install_requires=requires,
      entry_points="""\
      [paste.app_factory]
      main = web.overview:main
      """,
)

