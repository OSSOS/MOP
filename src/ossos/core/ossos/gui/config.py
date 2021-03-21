import json
import os
__author__ = "David Rusk <drusk@uvic.ca>"

DEFAULT_CONFIG_FILE = "config.json"

_configs = {}


def read(keypath, configfile=None):
    """
    Reads a value from the configuration file.

    Args:
      keypath: str
        Specifies the key for which the value is desired.  It can be a
        hierarchical path.  Example: "section1.subsection.key1"
      configfile: str
        Path to the config file to read.  Defaults to None, in which case
        the application's default config file is used.

    Returns:
      value from configuration file
    """
    if configfile in _configs:
        appconfig = _configs[configfile]
    else:
        appconfig = AppConfig(configfile=configfile)
        _configs[configfile] = appconfig

    return appconfig.read(keypath)


class AppConfig(object):
    """
    Provides programmatic access to contents of the application
    configuration file.
    """

    def __init__(self, configfile=None):
        if configfile is None or not os.access(configfile, os.R_OK):
            configfile = self._get_default_config_file_path(configfile=configfile)

        with open(configfile, "rb") as filehandle:
            self._data = json.loads(filehandle.read())

    def _get_default_config_file_path(self, configfile=None):
        if configfile is None:
            configfile = DEFAULT_CONFIG_FILE
        return os.path.join(os.path.dirname(__file__), configfile)

    def read(self, keypath):
        curr_data = self._data
        for key in keypath.split("."):
            curr_data = curr_data[key]
        # Possibly overide the value with an OS variable.
        # print(f'{keypath} - '+str(os.getenv(f'{keypath}', None)))
        return os.getenv(f'MOP_{keypath.replace(".","_")}', curr_data)

    def unravel(self, d):
        list_of_keys = []
        for key in d:
            if isinstance(d[key], dict):
                for sub_key in self.unravel(d[key]):
                    list_of_keys.append(f'{key}.{sub_key}')
            else:
                list_of_keys.append(f'{key}')
        return list_of_keys

    def __str__(self):
        env_vars = self.unravel(self._data)
        _result = ""
        for key in env_vars:
            _result += f'MOP_{key.replace(".", "_")}: {self.read(key)}\n'
        return _result
