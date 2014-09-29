__author__ = 'Michele Bannister   git:@mtbannister'

import math
import os

import ephem

from ossos import mpc
from ossos import orbfit
from ossos import storage
import parameters


def ossos_release_parser():
    retval = []
    infs = ['/Users/michele/Dropbox/OSSOS/o13eBlockV2.dat', '/Users/michele/Dropbox/OSSOS/OblockStage0_ossosv2.txt',
            '/Users/michele/Dropbox/OSSOS/Lstage0.txt', parameters.MOST_RECENT_OSSOS_RELEASE]
    for inf in infs[3:]:
        with open(inf, 'r') as infile:
            if inf == infs[0]:
                # columns:
                # classification  object  mag  stdev   dist  ..E nobs time av_xres av_yres max_x max_y (not sure what
                # use those)
                # a ..E  e  ..E  i ..E    node     ..E    argperi     ..E           M        ..E   ra_dis  dec_dis
                for line in infile.readlines()[
                            11:len(infile.readlines()) - 5]:  # FIXME: better parsing for other discovery files
                    ln = line.split()
                    obj = parameters.tno()
                    obj.classification = ln[0]
                    obj.name = ln[1]
                    obj.mag = float(ln[2])
                    obj.mag_stdev = float(ln[3])
                    obj.dist = float(ln[4])
                    obj.dist_e = float(ln[5])
                    obj.nobs = int(ln[6])
                    obj.arclen = float(ln[7])
                    obj.a = float(ln[12])
                    obj.a_e = float(ln[13])
                    obj.e = float(ln[14])
                    obj.e_e = float(ln[15])
                    obj.i = float(ln[16])
                    obj.i_e = float(ln[17])
                    obj.peri = obj.a * (1. - obj.e)
                    retval.append(obj)
            if inf == infs[1]:
                for line in infile.readlines()[14:]:
                    ln = line.split()
                    obj = parameters.tno()
                    obj.name = ln[0]
                    obj.mag = float(ln[1])
                    obj.mag_stdev = float(ln[2])
                    obj.dist = float(ln[3])
                    obj.dist_e = float(ln[4])
                    obj.nobs = int(ln[5])
                    obj.arclen = float(ln[6])
                    obj.i = float(ln[7])
                    obj.i_e = float(ln[8])
                    retval.append(obj)
            if inf == infs[2]:
                for line in infile.readlines():
                    if len(line) > 1 and not line.startswith('#'):
                        ln = line.split()
                        obj = parameters.tno()
                        obj.classification = ln[0]
                        obj.name = ln[1]
                        if not obj.name.endswith('nt'):
                            obj.mag = float(ln[2])
                            obj.mag_stdev = float(ln[3])
                            obj.dist = float(ln[4])
                            obj.dist_e = float(ln[5])
                            obj.nobs = int(ln[6])
                            obj.arclen = float(ln[7])
                            obj.a = float(ln[12])
                            obj.a_e = float(ln[13])
                            obj.e = float(ln[14])
                            obj.e_e = float(ln[15])
                            obj.i = float(ln[16])
                            obj.i_e = float(ln[17])
                            obj.peri = obj.a * (1. - obj.e)
                            obj.node = float(ln[18])
                            obj.node_e = float(ln[19])
                            obj.argp = float(ln[20])
                            obj.argp_e = float(ln[21])
                            obj.M = float(ln[22])
                            obj.M_e = float(ln[23])
                            obj.ra_discov = float(ln[24])
                            obj.dec_discov = float(ln[25])
                            retval.append(obj)
            if inf == infs[3]:
                for line in infile.readlines():
                    ln = line.split()
                    obj = parameters.tno()
                    obj.name = ln[0]
                    # if not obj.name.endswith('nt'):
                    obj.mag = float(ln[1])
                    obj.mag_stdev = float(ln[2])
                    obj.dist = float(ln[3])
                    obj.dist_e = float(ln[4])
                    obj.nobs = int(ln[5])
                    obj.arclen = float(ln[6])
                    obj.a = float(ln[11])
                    obj.a_e = float(ln[12])
                    obj.e = float(ln[13])
                    obj.e_e = float(ln[14])
                    obj.i = float(ln[15])
                    obj.i_e = float(ln[16])
                    obj.node = float(ln[17])
                    obj.node_e = float(ln[18])
                    obj.argp = float(ln[19])
                    obj.argp_e = float(ln[20])
                    obj.M = float(ln[21])
                    obj.M_e = float(ln[22])
                    obj.ra_discov = float(ln[23])
                    obj.dec_discov = float(ln[24])
                    obj.peri = obj.a * (1. - obj.e)
                    retval.append(obj)

    return retval


def ossos_discoveries(no_nt_and_u=True):
    """
    Returns a list of orbfit.Orbfit objects with the observations in the Orbfit.observations field.
    """
    retval = []

    discovery_files = [n for n in os.listdir(parameters.REAL_KBO_AST_DIR) if n.endswith('.mpc') or n.endswith('.ast')]
    for filename in discovery_files:
        # keep out the not-tracked for now. Some uncharacterised are quite good so keep those.
        if no_nt_and_u:
            if not filename.__contains__('nt') and not filename.__contains__('u'):
                observations = mpc.MPCReader(parameters.REAL_KBO_AST_DIR + filename).mpc_observations
                orbit = orbfit.Orbfit(observations)
                retval.append(orbit)

    return retval


def ossos_release_with_metadata():
    """
    Wrap the objects from the Version Releases together with the objects instantiated from fitting their mpc lines
    """
    discoveries = ossos_release_parser()
    observations = ossos_discoveries()

    for obj in discoveries:
        observation = [n for n in observations if n.observations[-1].provisional_name == obj.name][0]
        for obs in observation.observations:
            if obs.discovery.is_discovery:
                if obj.mag is not None:
                    H = obj.mag + 2.5 * math.log10(1. / ((obj.dist ** 2) * ((obj.dist - 1.) ** 2)))
                else:
                    H = None
                obj.H = H
                obj.T = observation.T
                obj.discovery_date = obs.date
                obj.observations = observation

    return discoveries  # list of tno() objects


def synthetic_model_kbos(at_date=parameters.NEWMOONS[parameters.DISCOVERY_NEW_MOON], maglimit=25.2, kbotype=False,
                         arrays=False):
    # # build a list of Synthetic KBOs
    print "LOADING SYNTHETIC MODEL KBOS FROM: {}".format(parameters.L7MODEL)
    kbos = []
    if arrays:  # much easier to use for plt.scatter()
        ra = []
        dist = []
        hlat = []
    lines = storage.open_vos_or_local(parameters.L7MODEL).read().split('\n')
    counter = 0
    for line in lines:
        if len(line) == 0 or line[0] == '#':  # skip initial column descriptors and the final blank line
            continue
        kbo = ephem.EllipticalBody()
        values = line.split()
        kbo.name = values[8]
        if kbotype and (kbo.name == kbotype):  # and (values[9] == '3'):
            kbo._a = float(values[0])
            kbo._e = float(values[1])
            kbo._inc = float(values[2])
            kbo._Om = float(values[3])
            kbo._om = float(values[4])
            kbo._M = float(values[5])
            kbo._H = float(values[6])
            epoch = ephem.date(2453157.50000 - ephem.julian_date(0))
            kbo._epoch_M = epoch
            kbo._epoch = epoch
            date = ephem.date(at_date)
            kbo.compute(date)
            counter += 1

            # ## only keep objects that are brighter than limit
            if (kbo.mag < maglimit):
                kbos.append(kbo)
                if arrays:
                    ra.append(kbo.ra)
                    dist.append(kbo.sun_distance)
                    hlat.append(kbo.hlat)

    print '%d synthetic model kbos brighter than %d retained from %d in L7 model'.format(len(kbos), maglimit,
                                                                                         counter)
    if not arrays:
        return kbos
    else:
        return ra, dist, hlat


def output_discoveries_for_animation():
    discoveries = ossos_release_with_metadata()
    with open('/Users/michele/Desktop/OSSOSdiscoveries.txt', 'w') as outfile:
        outfile.write('name a e i node peri MA epoch H date_of_discovery\n')
        for obj in discoveries:
            # a e i node peri MA epoch H date_of_discovery
            outfile.write(
                '{:>10s} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:>10s}\n'.format(
                    obj.name, obj.a, obj.e, obj.i, obj.node, obj.argp, obj.M, obj.T, obj.H, obj.discovery_date))
