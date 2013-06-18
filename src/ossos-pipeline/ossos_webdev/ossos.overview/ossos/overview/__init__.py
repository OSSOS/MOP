# package

from pyramid.config import Configurator


def main(global_config, **settings):
    config = Configurator(settings=settings)
    config.add_route(name='overview', path='/')
    config.scan(package='ossos')
    return config.make_wsgi_app()