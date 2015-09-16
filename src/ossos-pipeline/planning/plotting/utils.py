__author__ = 'jjk'

import numpy as np
import os

path = '/Users/bannisterm/Dropbox/Papers in progress/OSSOS/First_quarter/data/'


def square_fit(m, params):
    return np.where(m < 21., params[0],
                    (params[0] - params[1] * (m - 21.0) ** 2) / (1. + np.exp((m - params[2]) / params[3])))


def square_fit_discovery_mag(obj, discov_mag, discov_rate):
    pwd = path + 'efficiency_motion_rates'
    blocks = {'o3e': "13AE", 'o3o': "13AO"}
    block = blocks[obj.lstrip('u')[0:3]]
    smooth_parameter_files = filter(lambda name: name.startswith('smooth'), os.listdir('{}/{}/'.format(pwd, block)))
    smooth_parameter_files.sort(key=lambda x: float(x.split('-')[1]))
    rates = np.array([float(x.split('-')[2]) for x in smooth_parameter_files])
    rate, = np.where(discov_rate < rates)
    fn = smooth_parameter_files[rate[0]]
    pd, ps, mag_limit = read_smooth_fit('{}/{}/{}'.format(pwd, block, fn))
    eta = square_fit(discov_mag, ps)
    # print obj, discov_mag, eta, discov_rate, block, fn

    return float(eta)


def read_smooth_fit(fichier):
    pd, ps, mag_limit = None, None, None
    with open(fichier, 'r') as infile:
        lines = infile.readlines()
        for line in lines:
            if line[0] != "#":
                if line.startswith("double_param="):
                    pd = map(float, line[13:-1].split())
                if line.startswith("square_param="):
                    ps = map(float, line[13:-1].split())
                if line.startswith("mag_lim="):
                    mag_limit = float(line.split()[1])

    return pd, ps, mag_limit
