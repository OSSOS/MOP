__author__ = "David Rusk <drusk@uvic.ca>"

import os
import threading

from ossos import storage
from ossos.gui import logger


class SynchronizationManager(object):
    def __init__(self, remote_context, sync_enabled=False):
        self.remote_context = remote_context
        self.sync_enabled = sync_enabled

        self.syncable_files = []

    def enable_sync(self):
        self.sync_enabled = True
        self.sync_all()

    def disable_sync(self):
        self.sync_enabled = False

    def add_syncable_file(self, path):
        self.syncable_files.append(path)

        if self.sync_enabled:
            self.sync_all()

    def sync_all(self):
        while self.syncable_files:
            syncable_file = self.syncable_files.pop(0)
            thread = threading.Thread(target=self.do_synchronize,
                                      args=(syncable_file, ))
            thread.start()

    def do_synchronize(self, local_path):
        remote_uri = self.get_remote_uri(local_path)
        logger.info("Syncing %s to %s." % (local_path, remote_uri))
        storage.copy(local_path, remote_uri)

    def get_remote_uri(self, local_path):
        basename = os.path.basename(local_path)
        return self.remote_context.get_full_path(basename)

