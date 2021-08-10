import re
import sys

import ephem


# Number_Mil={'B': 110000, 'C': 120000, 'D': 130000, 'E': 140000, 'F': 150000}
# Number_Cent={'J': 1900, 'K': 2000}

yy = {'I': 1800, 'J': 1900, 'K': 2000}
n_code = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
kilo = 'ABCDEFGHIJKLMNOPQRSTUV'


# noinspection PyBroadException
def date_unpack(packed_date):
    (yyyy, mm, dd) = (2000, 0o1, 0o1)
    try:
        yyyy = yy[packed_date[0]] + int(packed_date[1:3])
        mm = n_code.rindex(packed_date[3])
        dd = float(n_code.rindex(packed_date[4]))
    except Exception as ex:
        sys.stderr.write(str(ex))
        sys.stderr.write("ERROR converting date part {}:".format(packed_date))
    return yyyy, mm, dd


def desig_unpack(packed_designation):
    if re.match(r'\d+', packed_designation):
        return str(int(packed_designation))
    j = kilo.rfind(packed_designation[0])
    if j != -1:
        if re.match(r'^\d+$', packed_designation[1:7]):
            return str(100000 + j * 10000 + int(packed_designation[1:7]))
    try:
        yyyy = str(yy[packed_designation[0]] + int(packed_designation[1:3]))
    except KeyError as ex:
        sys.stderr.write(str(ex))
        return packed_designation
    m_code = packed_designation[3] + packed_designation[6]
    cycle = n_code.rindex(packed_designation[4]) * 10 + int(
        packed_designation[5])
    if cycle > 0:
        cycle = str(cycle)
    else:
        cycle = ''
    return yyyy + ' ' + m_code + cycle


def get_kbos(mpc_file, cond='a > 30'):
    f = open(mpc_file)
    lines = f.readlines()
    f.close()
    line_count = 0
    kbos = []
    for line in lines:
        line_count = line_count + 1
        if line[0] == '#' or \
                len(line) < 103 or \
                line[0:3] == '---' or \
                line[0:3] == 'Des':
            continue
        kbo = ephem.EllipticalBody()

        try:
            if len(line[8:13].strip()):
                kbo._H = float(line[8:13])
                kbo._G = float(line[14:19])
            else:
                kbo._H = 20
                kbo._G = 0
        except Exception as ex:
            sys.stderr.write("Failed to compute H mag.")
            sys.stderr.write(str(ex))
            sys.stderr.write(f"{line}\n")
            kbo._H = 20
            kbo._G = 0
        arc = line[127:136]
        try:
            if 'days' in arc:
                arc = int(arc.split()[0]) / 365.25
            else:
                arc = -eval(arc)
        except Exception as ex:
            sys.stderr.write("Failed to get arc length.")
            sys.stderr.write(str(ex))
            sys.stderr.write(f"{line}\n")
            sys.stderr.write("Setting to 0.")
            arc = 0
        kbo._epoch_M = date_unpack(line[20:25].strip())
        kbo._M = float(line[26:35].strip())
        kbo._om = float(line[37:46].strip())
        kbo._Om = float(line[48:57].strip())
        kbo._inc = float(line[59:68].strip())
        kbo._e = float(line[70:79].strip())
        kbo._epoch = '2011/08/01'
        kbo._a = float(line[92:103].strip())
        kbo.compute()

        a = kbo.a
        q = kbo.a * (1 - kbo.e)
        H = kbo.H

        if a is None or q is None or H is None or arc is None:
            continue

        if eval(cond):
            kbo.name = desig_unpack(line[0:7].strip())
            kbos.append(kbo)

    return kbos
