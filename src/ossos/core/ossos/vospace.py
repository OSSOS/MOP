"""This module abstracts all vospace activities"""


from vos import vos


import logging
logging.getLogger('vos').setLevel(logging.DEBUG)
client = vos.Client()

