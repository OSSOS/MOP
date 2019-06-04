"""This module abstracts all vospace activities.  Including a switch to using username/password pairs."""
from getpass import getpass

from requests.auth import HTTPBasicAuth
from vos.vos import Client, Connection
import sys
import types
import netrc
import logging
logging.getLogger('vos').setLevel(logging.ERROR)

VOSPACE_SERVER = "www.canfar.phys.uvic.ca"


class Wrapper(Client):

    def __getattribute__(self, item):
        func = object.__getattribute__(self, item)
        if isinstance(func, types.MethodType):
            def my_func(*args, **kwargs):
                sys.stderr.write("{} -> {} {}\n".format(item, args[1:], kwargs))
                result = func(*args[1:], **kwargs)
                sys.stderr.write("{} <- {}\n".format(item, result))
                return result
            meth = types.MethodType(my_func, self, Client)
        elif isinstance(func, Connection):
            sys.stderr.write("*-"*40+"\n")
            meth = func
        else:
            meth = func

        return meth

client = Client()

