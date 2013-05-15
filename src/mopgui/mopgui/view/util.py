"""
Assorted utilities related to the application view.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import os


def get_asset_full_path(asset_name):
    """Get the full path of an asset based on its filename"""
    return os.path.join(os.path.dirname(__file__), "assets", asset_name)
