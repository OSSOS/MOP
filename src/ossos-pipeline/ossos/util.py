"""OSSOS helper methods"""

import subprocess
import os


def exec_prog(args):
    '''Run a subprocess, check for .OK and raise error if does not exist.

    args:  list of arguments, for value is the command to execute.

    '''

    program_name = args[0]
    output = subprocess.check_call(args)
    if not os.access(program_name+".OK", os.F_OK):
        raise subprocess.CalledProcessError(-1, ' '.join(args), output)

    return output

