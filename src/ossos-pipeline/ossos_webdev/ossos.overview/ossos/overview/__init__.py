# package

from pyramid.config import Configurator
from pyramid.security import authenticated_userid, Allow, Authenticated
from pyramid.authorization import ACLAuthorizationPolicy


class Root(object):
    __acl__ = [
        (Allow, Authenticated, 'ossos')
        ]

    def __init__(self, request):
        self.request = request


def main(global_config, **settings):

    #  authz_policy = ACLAuthorizationPolicy()


    config = Configurator(settings=settings,
#		authentication_policy=authn_policy,
#        authorization_policy=authz_policy,
        root_factory=Root
    	)

#    config.set_authorization_policy(authenticated_userid(login_view(request)))

    config.add_route(name='overview', path='/')
    config.scan(package='ossos')
    return config.make_wsgi_app()