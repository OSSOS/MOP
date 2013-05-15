__author__ = "David Rusk <drusk@uvic.ca>"

import threading


class AsynchronousImageLoader(object):
    """
    Loads images asynchronously from the rest of the application.
    """

    def __init__(self, resolver, image_retriever):
        self.resolver = resolver
        self.image_retriever = image_retriever

    def start_loading(self, astrom_data,
                      image_loaded_callback=None,
                      all_loaded_callback=None):

        self.image_loaded_callback = image_loaded_callback
        self.all_loaded_callback = all_loaded_callback

        lookupinfo = []
        for source_num, source in enumerate(astrom_data.sources):
            for obs_num, reading in enumerate(source):
                image_uri = self.resolver.resolve_uri(reading.obs)
                lookupinfo.append((image_uri, reading, source_num, obs_num))

        self.do_loading(lookupinfo)

    def do_loading(self, lookupinfo):
        SerialImageRetrievalThread(self, self.image_retriever,
                                   lookupinfo).start()

    def on_image_loaded(self, image, converter, reading, source_num, obs_num):
        reading.image = image
        reading.converter = converter

        if self.image_loaded_callback is not None:
            self.image_loaded_callback(source_num, obs_num)

    def on_all_loaded(self):
        print "All images loaded"
        if self.all_loaded_callback is not None:
            print "All images loaded callback"
            self.all_loaded_callback()


class SerialImageRetrievalThread(threading.Thread):
    """
    Retrieve each image serially, but in this separate thread so it can
    happen in the background.
    """

    def __init__(self, loader, image_retriever, lookupinfo):
        super(SerialImageRetrievalThread, self).__init__()

        self.loader = loader
        self.image_retriever = image_retriever
        self.lookupinfo = lookupinfo

    def run(self):
        for image_uri, reading, source_num, obs_num in self.lookupinfo:
            image, converter = self.image_retriever.retrieve_image(image_uri, reading)
            self.loader.on_image_loaded(image, converter, reading, source_num, obs_num)

        self.loader.on_all_loaded()
