__author__ = "David Rusk <drusk@uvic.ca>"

import threading


class AsynchronousImageDownloadManager(object):
    """
    Coordinates the downloading of images asynchronously from the rest of
    the application.
    """

    def __init__(self, resolver, image_retriever):
        self.resolver = resolver
        self.image_retriever = image_retriever

    def start_download(self, astrom_data,
                       image_loaded_callback=None,
                       all_loaded_callback=None):

        self.image_loaded_callback = image_loaded_callback
        self.all_loaded_callback = all_loaded_callback

        lookupinfo = []
        for source_num, source in enumerate(astrom_data.sources):
            for obs_num, reading in enumerate(source):
                image_uri = self.resolver.resolve_uri(reading.obs)
                lookupinfo.append((image_uri, reading, source_num, obs_num))

        self.do_download(lookupinfo)

    def do_download(self, lookupinfo):
        SerialImageDownloadThread(self, self.image_retriever,
                                  lookupinfo).start()

    def on_image_downloaded(self, image, converter, reading, source_num, obs_num):
        reading.image = image
        reading.converter = converter

        if self.image_loaded_callback is not None:
            self.image_loaded_callback(source_num, obs_num)

    def on_all_downloaded(self):
        if self.all_loaded_callback is not None:
            self.all_loaded_callback()


class SerialImageDownloadThread(threading.Thread):
    """
    Retrieve each image serially, but in this separate thread so it can
    happen in the background.
    """

    def __init__(self, loader, image_retriever, lookupinfo):
        super(SerialImageDownloadThread, self).__init__()

        self.download_manager = loader
        self.image_retriever = image_retriever
        self.lookupinfo = lookupinfo

    def run(self):
        for image_uri, reading, source_num, obs_num in self.lookupinfo:
            image, converter = self.image_retriever.retrieve_image(image_uri, reading)
            self.download_manager.on_image_downloaded(image, converter, reading, source_num, obs_num)

        self.download_manager.on_all_downloaded()
