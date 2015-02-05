def includeme(config):
    config.add_route('login', '/login')
    config.add_route('logout', '/logout')
    config.scan(__name__)
