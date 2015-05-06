"""
Wraps the standard library logging module and allows us to set the log
level differently for our code and third-party code.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import logging
import os
import tempfile

# set logging level and shunt to a file
OSSOS_DEBUG_LEVEL = logging.CRITICAL

logfile = os.path.join(tempfile.gettempdir(), "ossos_validation.log")
logging.basicConfig(filename=logfile,
                    format="%(levelname)s: %(module)s.%(funcName)s %(lineno)d: %(message)s (%(asctime)s)",
                    datefmt="%m/%d/%Y %I:%M:%S %p",
                    level=logging.WARNING)

_logger = logging.getLogger(__name__)
_logger.setLevel(OSSOS_DEBUG_LEVEL)
debug = _logger.debug
info = _logger.info
warning = _logger.warning
error = _logger.error
critical = _logger.critical
