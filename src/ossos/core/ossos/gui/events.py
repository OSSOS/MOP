__author__ = "David Rusk <drusk@uvic.ca>"

# NOTE: we are using version 1 of the pubsub API
# (http://pubsub.sourceforge.net/apidocs/docs4v1.html#label-api-v1-docs)
# This is in order to maintain compatiblity with software available on older
# systems such as ScientificLinux 5.5 on CANFAR VMs.

try:
    from pubsub import pub
#    from wx.lib.pubsub import Publisher as pub
except ImportError as ierr:
    print(f"{ierr}")
    print(f"This package now requires PyPubSub to be installed, Hint: pip install pypubsub")
    exit()

# Event ids
ROOT = ("rootid", )
CHANGE_IMAGE = ROOT + ("change_image", )
IMG_LOADED = ROOT + ("imgload", )
FINISHED_WORKUNIT = ROOT + ("finished_workunit", )
NO_AVAILABLE_WORK = ROOT + ("no_available_work", )


def send(event_id, data=None):
    pub.sendMessage(event_id, event=data)


def subscribe(event_id, callback):
    pub.subscribe(callback, event_id)


def unsub_all():
    pub.unsubAll()
