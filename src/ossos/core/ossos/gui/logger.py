"""
Wraps the standard library logging module and allows us to set the log
level differently for our code and third-party code.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import logging
import os
import tempfile

# set logging level and shunt to a file
logging.getLogger('vos').setLevel(logging.ERROR)

logfile = os.path.join(tempfile.gettempdir(), "ossos_validation.log")
logging.basicConfig(filename=logfile,
                    format="%(levelname)s: %(asctime)s %(thread)d %(module)s.%(funcName)s %(lineno)d: %(message)s",
                    datefmt="%m/%d/%Y %I:%M:%S %p",
                    level=logging.ERROR)

_logger = logging.getLogger(__name__)
debug = _logger.debug
info = _logger.info
warning = _logger.warning
error = _logger.error
critical = _logger.critical


def set_debug():
    logging.getLogger(__name__).setLevel(logging.DEBUG)
