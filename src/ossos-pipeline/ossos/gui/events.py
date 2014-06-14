__author__ = "David Rusk <drusk@uvic.ca>"

# NOTE: we are using version 1 of the pubsub API
# (http://pubsub.sourceforge.net/apidocs/docs4v1.html#label-api-v1-docs)
# This is in order to maintain compatiblity with software available on older
# systems such as ScientificLinux 5.5 on CANFAR VMs.

try:
    # On up-to-date systems this import will trigger using version 1 of
    # the pubsub API.
    from wx.lib.pubsub import setupv1
except ImportError:
    # Old systems will not be able to execute that import, but it is ok
    # because all they have is the version 1 API.
    pass

try:
    from wx.lib.pubsub import Publisher as pub
except ImportError:
    from wx.lib.pubsub import setuparg1
    from wx.lib.pubsub import pub

# Event ids
ROOT = ("rootid", )
CHANGE_IMAGE = ROOT + ("change_image", )
IMG_LOADED = ROOT + ("imgload", )
FINISHED_WORKUNIT = ROOT + ("finished_workunit", )
NO_AVAILABLE_WORK = ROOT + ("no_available_work", )


def send(event_id, data=None):
    pub.sendMessage(event_id, data=data)


def subscribe(event_id, callback):
    pub.subscribe(callback, event_id)


def unsub_all():
    pub.unsubAll()
