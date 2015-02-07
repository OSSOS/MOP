__author__ = 'Michele Bannister   git:@mtbannister'

import os
import cPickle

import ephem

from ossos import mpc
from ossos import orbfit
from ossos import storage
import parameters
from parameters import tno


def ossos_release_parser():
    retval = []
    with open(parameters.RELEASE_CLASSIFICATION, 'r') as classfile:
        classlines = classfile.readlines()[1:]  # first line is column definitions
    with open(parameters.RELEASE_SUMMARY, 'r') as summary:
        for i, line in enumerate(summary.readlines()[1:]):
            obj = tno.from_summary_line(line, version=parameters.RELEASE_VERSION)
            obj = tno.from_class_line(classlines[i], version=parameters.RELEASE_VERSION, existing_object=obj)
            retval.append(obj)

    return retval


def ossos_discoveries(no_nt_and_u=True):
    """
    Returns a list of orbfit.Orbfit objects with the observations in the Orbfit.observations field.
    """
    retval = []

    # discovery_files = [n for n in os.listdir(parameters.REAL_KBO_AST_DIR)]# if n.endswith('.mpc') or n.endswith(
    # '.ast')]
    for filename in storage.listdir(parameters.REAL_KBO_AST_DIR):
        # keep out the not-tracked and uncharacterised.
        if no_nt_and_u:
            if not (filename.__contains__('nt') or filename.__contains__('u')):
                print(filename)
                observations = mpc.MPCReader(parameters.REAL_KBO_AST_DIR + filename)
                orbit = orbfit.Orbfit(observations.mpc_observations)
                retval.append((observations, orbit))

    return retval


def ossos_release_with_metadata():
    """
    Wrap the objects from the Version Releases together with the objects instantiated from fitting their mpc lines
    """
    # discoveries = ossos_release_parser()
    discoveries = []
    observations = ossos_discoveries()
    for obj in observations:
        discov = [n for n in obj[0].mpc_observations if n.discovery.is_discovery][0]
        tno = parameters.tno()
        tno.dist = obj[1].distance
        tno.ra_discov = discov.coordinate.ra.degrees
        tno.mag = discov.mag
        tno.name = discov.provisional_name
        discoveries.append(tno)

    # for obj in discoveries:
    # observation = [n for n in observations if n.observations[-1].provisional_name == obj.name][0]
    # for obs in observation.observations:
    #         if obs.discovery.is_discovery:
    #             if obj.mag is not None:
    #                 H = obj.mag + 2.5 * math.log10(1. / ((obj.dist ** 2) * ((obj.dist - 1.) ** 2)))
    #             else:
    #                 H = None
    #             obj.H = H
    #             obj.T = observation.T
    #             obj.discovery_date = obs.date
    #             obj.observations = observation

    return discoveries  # list of tno() objects


def synthetic_model_kbos(at_date=parameters.NEWMOONS[parameters.DISCOVERY_NEW_MOON], maglimit=24.5, kbotype=False,
                         arrays=False):
    # # build a list of Synthetic KBOs
    kbos = []
    print "LOADING SYNTHETIC MODEL KBOS FROM: {}".format(parameters.L7MODEL)
    pastpath = parameters.L7_HOME + str(at_date).split()[0].replace('/', '-') + '_' + str(maglimit) + (
    kbotype or '') + '.dat'
    print(pastpath)
    if os.path.exists(pastpath):
        with open(pastpath) as infile:
            ra, dist, hlat, Hmag = cPickle.load(infile)
        print('{} synthetic model kbos brighter than {} at {} in L7 model'.format(len(ra), maglimit, at_date))
    else:
        if arrays:  # much easier to use for plt.scatter()
            ra = []
            dist = []
            hlat = []
            Hmag = []

        lines = storage.open_vos_or_local(parameters.L7MODEL).read().split('\n')
        counter = 0
        for line in lines:
            if len(line) == 0 or line[0] == '#':  # skip initial column descriptors and the final blank line
                continue
            kbo = ephem.EllipticalBody()
            values = line.split()
            kbo.name = values[8]
            if kbotype and (kbo.name == kbotype) and (values[9] == '3' and values[10] == '2'):  # keeping 3:2 resonators
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
                        ra.append(float(kbo.ra))
                        dist.append(float(kbo.sun_distance))
                        hlat.append(float(kbo.hlat))
                        Hmag.append(float(kbo._H))

        print '{} synthetic model kbos brighter than {} at {} retained from {} in L7 model'.format(len(kbos), maglimit,
                                                                                                   at_date, counter)
    if not arrays:
        return kbos
    else:
        if not os.path.exists(pastpath):
            with open(pastpath, 'w') as outfile:
                cPickle.dump((ra, dist, hlat, Hmag), outfile)
        return ra, dist, hlat, Hmag


def output_discoveries_for_animation():
    discoveries = ossos_release_with_metadata()
    with open('/Users/michele/Desktop/OSSOSdiscoveries.txt', 'w') as outfile:
        outfile.write('name a e i node peri MA epoch H date_of_discovery\n')
        for obj in discoveries:
            # a e i node peri MA epoch H date_of_discovery
            outfile.write(
                '{:>10s} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:8.2f} {:>10s}\n'.format(
                    obj.name, obj.a, obj.e, obj.i, obj.node, obj.argp, obj.M, obj.T, obj.H, obj.discovery_date))
