from setuptools import setup, find_packages

requires = [
    'pyramid',
    'pyramid_chameleon',  # new dependency: Pyramid 1.5 separates out templating support
    'waitress',
    'zope.cachedescriptors',
    'astropy',
    'sqlalchemy',
    'pyOpenSSL',
    'psycopg2',
]

setup(name='web',
      version='0.2',
      author='Michele Bannister',
      author_email='micheleb@uvic.ca',
      description='Metadata website for the Outer Solar System Origins Survey',
      url='https://github.com/OSSOS/MOP/tree/master/src/ossos-pipeline/web',
      classifiers=[
          "Programming Language :: Python",
          "Framework :: Pyramid",
      ],
      namespace_packages=['web'],
      packages=find_packages(),
      install_requires=requires,
      entry_points="""
      [paste.app_factory]
      main = web.overview:main
      """,
)

