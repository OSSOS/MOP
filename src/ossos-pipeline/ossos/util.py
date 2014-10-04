"""OSSOS helper methods"""

import subprocess
import os
import logging

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

