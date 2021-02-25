

__author__ = 'jjk'

from numpy import (array, where)
import os
# from ossos.parsers import read_smooth_fit

path = '/Users/bannisterm/Dropbox/Papers in progress/OSSOS/First_quarter/data/'


def square_fit(m, params):
    return where(m < 21., params[0],
                    (params[0] - params[1] * (m - 21.0) ** 2) / (1. + np.exp((m - params[2]) / params[3])))


def square_fit_discovery_mag(obj, discov_mag, discov_rate):
    pwd = path + 'efficiency_motion_rates'
    blocks = {'o3e': "13AE", 'o3o': "13AO"}
    block = blocks[obj.lstrip('u')[0:3]]
    smooth_parameter_files = [name for name in os.listdir('{}/{}/'.format(pwd, block)) if name.startswith('smooth')]
    smooth_parameter_files.sort(key=lambda x: float(x.split('-')[1]))
    rates = array([float(x.split('-')[2]) for x in smooth_parameter_files])
    rate, = where(discov_rate < rates)
    fn = smooth_parameter_files[rate[0]]
    # pd, ps, mag_limit = read_smooth_fit('{}/{}/{}'.format(pwd, block, fn))
    eta = square_fit(discov_mag, ps)
    # print obj, discov_mag, eta, discov_rate, block, fn

    return float(eta)


