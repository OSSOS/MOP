def includeme(config):
    config.add_route('block', 'blocks/{blockID}')
    config.scan(__name__)
