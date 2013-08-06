from setuptools import setup

requires = [
    'pyramid',
]
namespace_packages=['web' ]
setup(name='web.overview',
      entry_points="""\
      [paste.app_factory]
      main = web.overview:main
      """,
)

