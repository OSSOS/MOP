__author__ = 'Michele Bannister   git:@mtbannister'

import os

dir = '/Users/michele/Dropbox/OSSOS/measure3/2013A-O/initial_tracked_discoveries/'

for fn in [ff for ff in os.listdir(dir) if ff.endswith('.complete')]:
    outfile = dir + 'rest_of_2013/' + fn.split('.')[0] + '.' + fn.split('.')[4] + '.mpc'
    with open(dir + fn, 'r') as infile:
        lines = infile.readlines()
        for line in lines:
            # line is 0:80, month is 21:22
            if line[21:22] != '5':
                with open(outfile, 'a') as ofile:
                    ofile.write(line)

