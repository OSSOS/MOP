"""
Wraps the standard library logging module and allows us to set the log
level differently for our code and third-party code.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import logging
import os
import tempfile

# Use debug for our own code, but not globally because vos is very verbose.
OSSOS_DEBUG_LEVEL = logging.DEBUG
THIRD_PARTY_DEBUG_LEVEL = logging.INFO

logfile = os.path.join(tempfile.gettempdir(), "ossos_validation.log")
logging.basicConfig(filename=logfile,
                    format="%(levelname)s: %(message)s (%(asctime)s)",
                    datefmt="%m/%d/%Y %I:%M:%S %p",
                    level=THIRD_PARTY_DEBUG_LEVEL)

_logger = logging.getLogger(__name__)
_logger.setLevel(OSSOS_DEBUG_LEVEL)


def debug(message):
    _logger.debug(message)


def info(message):
    _logger.info(message)


def warning(message):
    _logger.warning(message)


def critical(message):
    _logger.critical(message)
