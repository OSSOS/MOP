__author__ = "David Rusk <drusk@uvic.ca>"

# TODO: compatibility with both new and old versions
from wx.lib.pubsub import setupv1
from wx.lib.pubsub import Publisher as pub

# Event ids
ROOT = ("rootid", )

NAV = ROOT + ("nav", )
NAV_SRC = NAV + ("src", )
NAV_OBS = NAV + ("obs", )

NEXT_SRC = NAV_SRC + ("next", )
PREV_SRC = NAV_SRC + ("prev", )
NEXT_OBS = NAV_OBS + ("next", )
PREV_OBS = NAV_OBS + ("prev", )

IMG_LOADED = ROOT + ("imgload", )

NEW_WORK_UNIT = ROOT + ("new_workunit", )
FINISHED_WORKUNIT = ROOT + ("finished_workunit", )
NO_AVAILABLE_WORK = ROOT + ("no_available_work", )


def send(event_id, data=None):
    pub.sendMessage(event_id, data=data)


def subscribe(event_id, callback):
    pub.subscribe(callback, event_id)


def unsub_all():
    pub.unsubAll()
