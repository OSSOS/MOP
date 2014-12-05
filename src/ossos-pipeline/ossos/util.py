"""OSSOS helper methods"""

import subprocess
import logging
from logging import handlers
import os
import vos



def exec_prog(args):
    '''Run a subprocess, check for .OK and raise error if does not exist.

    args:  list of arguments, for value is the command to execute.

    '''

    program_name = args[0]
    logging.info(" ".join(args))
    output = subprocess.check_output(args, stderr=subprocess.STDOUT)
    if not os.access(program_name+".OK", os.F_OK):
        logging.error("No {}.OK file?".format(program_name))
        raise subprocess.CalledProcessError(-1, ' '.join(args), output)
    os.unlink(program_name+".OK")
    if os.access(program_name+".FAILED", os.F_OK) :
        os.unlink(program_name+".FAILED")
    return output


class VOFileHandler(handlers.BufferingHandler):
    """
    A handler class that writes formatted logging records to VOSpace files.
    """
    def __init__(self, filename, vos_client=None):
        self.filename = filename
        self._client = vos_client
        self.stream = None
        super(VOFileHandler, self).__init__(1024*1024)

    @property
    def client(self):
        """
        Send back the client we were sent, or construct a default one.
        """
        if self._client is not None:
            return self._client
        self._client = vos.Client()
        return self._client

    def close(self):
        """
        Closes the stream.
        """
        self.flush()
        try:
            if self.stream is not None:
                self.stream.close()
                self.stream = None
        except:
            pass

    def flush(self):
        if self.stream is None:
            self.stream = self._open()
        for record in self.buffer:
            self.stream.write("{}\n".format(self.format(record)))
        self.buffer = []

    def _open(self):
        """
        Open the current base file with the (original) mode
        Return the resulting stream.
        """
        try:
            if self.stream.closed:
                return self.stream
        except AttributeError:
            pass

        buff = None
        try:
            buff = self.client.open(self.filename, view='data').read()
        except:
            pass

        stream = self.client.open(self.filename, os.O_WRONLY)
        if buff is not None:
            stream.write(buff)

        return stream
