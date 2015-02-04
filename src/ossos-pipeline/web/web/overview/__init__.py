# package
from pyramid.config import Configurator
from pyramid.security import Allow, Authenticated
from pyramid.authentication import AuthTktAuthenticationPolicy
from pyramid.authorization import ACLAuthorizationPolicy

import web.auth
import web.block
import web.bootstrap
import web.field_obs


class Root(object):
    __acl__ = [
        (Allow, Authenticated, 'ossos')
    ]

    def __init__(self, request):
        self.request = request


def main(global_config, **settings):
    config = Configurator(settings=settings,
                          root_factory=Root  # then it knows what __acl__ it can have
    )

    authn_policy = AuthTktAuthenticationPolicy('ossos is wonderful', hashalg='sha512')
    config.set_authentication_policy(authn_policy)
    config.set_authorization_policy(ACLAuthorizationPolicy())

    # then continue as normal
    config.add_route(name='overview', path='/')

    config.include(web.auth)
    config.include(web.block)
    config.include(web.bootstrap)
    config.include(web.field_obs)

    config.scan()

    print(config.registry.keys())

    # the first place it will go is web.auth.model.py's @view_config 'login'
    # which has a def login_view(request) which we want to set first.

    return config.make_wsgi_app()
