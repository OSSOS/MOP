class ProgressiveImageLoader(object):
    def __init__(self, resolver, image_retriever):
        self.resolver = resolver
        self.image_retriever = image_retriever

    def start_loading(self, astrom_data, callback):
        for source in astrom_data.sources:
            for reading in source:
                image_uri = self.resolver.resolve_uri(reading.obs)
                image, converter = self.image_retriever.retrieve_image(
                    image_uri, reading
                )

                assert image is not None, \
                    "No image retrieved for source reading %s" % reading

                reading.image = image
                reading.converter = converter

                callback()
