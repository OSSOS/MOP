# The OSSOS project uses a 'square' detection efficiency
# eff(m) = (eff_max-c*(m-21)**2)/(1+exp((m-M_0)/sig))
import numpy


def square(m, eff_max,c,m0,sigma,m1=21):
    """
    eff_max:  Maximum of the efficiency function (peak efficiency)
    c: shape of the drop-off in the efficiency at bright end
    m0: transition to tappering at faint magnitudes
    sigma: width of the transition in efficeincy
    m1: magnitude at which peak efficeincy occurs

    square(m) = (eff_max-c*(m-21)**2)/(1+exp((m-M_0)/sig))
    """

    return (eff_max-c*(m-21)**2)/(1+numpy.exp((m-m0)/sigma))

def parse_square_param(line):
    """
    Parse the line from the .eff file that contains the efficiency function
    parameters for a 'square' function
    line : the line containt the parameters, must start with 'square_param'
    """
    if not line.startswith("square_param="):
        raise ValueError("Not a valid square_param line")
    values = line.split()
    params = {'sigma': float(values.pop()),
              'm0': float(values.pop()),
              'c': float(values.pop()),
              'eff_max': float(values.pop())
              }
    return params

def parse_mag_error(line):
    if not line.startswith("mag_error="):
        raise ValueError("Not a valid mag_error param line")
    values = line.split()[1:]
    params = {}
    count = len(values)
    for value in float(values.pop()):
        params["p{}".format(count)] = value
        count -= 1
    return params

def rates(line):
    if not line.lstrip().startswith("rates="):
        raise ValueError("Not a rates line")
    values = line.split()
    params = {'upper': float(values.pop()),
              'lower': float(values.pop())}
    return params

def dummy(line):
    return line

def parse_eff(filename):
    """
    Parse through Jean-Marcs OSSSO .eff files.
    
    The efficiency files comes in 'chunks' meant to be used at different 'rates' of motion.

    """
    blocks = []
    block = {}

    with open(filename) as efile:
        for line in efile.readlines():
            if line.lstrip().startswith("#"):
                continue
            keyword = line.lstrip().split("=")[0]
            funcs = {'square_param': parse_square_param, 
                     'rates': rates}
            block[keyword] = funcs.get(keyword, dummy)(line)
            if keyword == 'mag_lim':
                blocks.append(block)
                block = {}

    return blocks
