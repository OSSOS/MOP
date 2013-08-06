from setuptools import setup

requires = [
    'pyramid',
]
namespace_packages=['web']
setup(name='web.field_obs',
      entry_points="""\
      [paste.app_factory]
      main = web.overview:main
      """,
)

