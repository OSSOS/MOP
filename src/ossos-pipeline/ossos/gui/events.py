__author__ = "David Rusk <drusk@uvic.ca>"

# TODO: compatibility with both new and old versions
#from wx.lib.pubsub import setupv1
from wx.lib.pubsub import Publisher as pub

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
