__author__ = "David Rusk <drusk@uvic.ca>"

# Pub/Sub ids
MSG_ROOT = ("astrodataroot", )

MSG_NAV = MSG_ROOT + ("nav", )
MSG_NAV_SRC = MSG_NAV + ("src", )
MSG_NAV_OBS = MSG_NAV + ("obs", )

MSG_NEXT_SRC = MSG_NAV_SRC + ("next", )
MSG_PREV_SRC = MSG_NAV_SRC + ("prev", )
MSG_NEXT_OBS = MSG_NAV_OBS + ("next", )
MSG_PREV_OBS = MSG_NAV_OBS + ("prev", )

MSG_IMG_LOADED = MSG_ROOT + ("imgload", )

MSG_NEW_WORK_UNIT = MSG_ROOT + ("newworkunit", )
MSG_FILE_PROC = MSG_ROOT + ("fileproc", )
MSG_ALL_ITEMS_PROC = MSG_ROOT + ("allproc", )
