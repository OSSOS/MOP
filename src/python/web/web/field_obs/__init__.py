def includeme(config):
    config.add_route('field_obs', 'blocks/fields/{fieldID}')
    config.scan(__name__)
