# coding=utf-8
__author__ = 'Michele Bannister   git:@mtbannister'

import os
import cPickle
from collections import OrderedDict

import ephem
from astropy.table import Table
from uncertainties import ufloat
import numpy

from ossos import mpc
from ossos import orbfit
from ossos import storage
import parameters
from ossos.gui import context


# from parameters import tno

def ossos_release_parser(table=False):
    '''
    extra fun as this is space-separated so using CSV parsers is not an option
    '''
    names = ['cl', 'p', 'j', 'k', 'sh', 'object', 'mag', 'mag_uncert', 'F', 'H_sur', 'dist', 'dist_E', 'nobs',
             'time', 'av_xres', 'av_yres', 'max_x', 'max_y', 'a', 'a_E', 'e', 'e_E', 'i', 'i_E', 'node', 'node_E',
             'argperi', 'argperi_E', 'time_peri', 'time_peri_E', 'ra_dis', 'dec_dis', 'jd_dis', 'rate']

    if table:
        # have to specify the names because the header line contains duplicate IDs, which wrecks auto-column creation
        retval = Table.read(parameters.RELEASE_DETECTIONS, format='ascii', guess=False,
                            delimiter=' ', data_start=0, comment='#', names=names, header_start=None)
    else:
        retval = []
        with open(parameters.RELEASE_DETECTIONS, 'r') as detectionsfile:
            for line in detectionsfile.readlines()[1:]:  # first line is column definitions
                obj = tno.from_str(line, version=parameters.RELEASE_VERSION)
                retval.append(obj)

    return retval


class tno(object):
    def __init__(self, observations):
        self.orbit = orbfit.Orbfit(observations.mpc_observations)
        self.discovery = [n for n in observations.mpc_observations if n.discovery.is_discovery][0]
        self.name = observations.provisional_name
        return


def ossos_discoveries(directory=parameters.REAL_KBO_AST_DIR, suffix='ast', no_nt_and_u=True, single_object=None):
    """
    Returns a list of objects holding orbfit.Orbfit objects with the observations in the Orbfit.observations field.
    """
    retval = []
    working_context = context.get_context(directory)
    files = working_context.get_listing(suffix)
    if single_object is not None:
        files = filter(lambda name: name.startswith(single_object), files)
    for filename in files:
        try:
            observations = mpc.MPCReader(directory + filename)
            obj = tno(observations)
            retval.append(obj)
        except Exception as e:
            print str(e)
            print filename
            pass
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
    # if obs.discovery.is_discovery:
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


def round_sig_error(num, uncert):
    '''
    Return a string of the number and its uncertainty to the right sig figs via uncertainty's print methods.
    The uncertainty determines the sig fig rounding of the number.
    https://pythonhosted.org/uncertainties/user_guide.html
    '''
    u = ufloat(num, uncert)
    outstr = '{:.1uLS}'.format(u)

    return outstr


def linesep(name, distinguish=None):
    names = {'N': "Objects in resonance with Neptune",
             'U': "Objects in resonance with Uranus",  # don't have one yet but Kat does check
             'i': 'Inner classical belt',
             'm': 'Main classical belt',
             'o': 'Outer classical belt',
             'd': 'Detached classical belt',
             'x': 'Scattering disk',
             'c': 'Centaurs',
    }
    if distinguish:
        if distinguish == 'sca':
            name = 'x'
        elif distinguish == 'det':
            name = 'd'
        elif distinguish == 'cen':
            name = 'c'

    midline = r"\cutinhead{" + \
              "{}".format(names[name]) + \
              "} \n "

    return midline


def create_table(tnos, outfile):
    # sort order gives Classical, Detached, Resonant, Scattered
    tnos.sort(['cl', 'p', 'j', 'k', 'object'])  # this at least works just fine

    header = r"\begin{deluxetable}{cccccccccc}" + '\n' + \
             r"\tablehead{\colhead{Object} & \colhead{MPC designation} & \colhead{a (AU)} & \colhead{e} & \colhead{i " \
             r"($^{\circ}$)} & " \
             "\colhead{r$_{H}$ (AU)} & \colhead{H} & \colhead{Comment} }" + "\n" \
             + "\startdata \n"
    footer = r"\enddata " + "\n" + \
             r"\tablecomments{M:N: object is in the M:N resonance; I: the orbit classification is currently insecure; " \
             r"" \
             r"" \
             r"H: the human operator intervened to declare the orbit security status. " \
             r"The full orbital elements are available in electronic form from the Minor Planet Center.} " "\n" + \
             "\end{deluxetable} \n"

    # Scrape the index file for MPC designations - this would work better if the alternate designations were consistent
    # idx = {}
    # with open(parameters.IDX) as infile:
    # lines = infile.readlines()


    # want to output:
    with open(outfile, 'w') as ofile:
        ofile.write(header)
        for i, r in enumerate(tnos):
            if r['p'] != tnos[i - 1]['p']:  # just changed between object classification types
                if r['p'] == 'x':  # 'x' doesn't give enough info to set scattered or detached
                    ofile.write(linesep(r['p'], distinguish=r['cl']))
                else:
                    ofile.write(linesep(r['p']))
            # obj a ± da e ± de i ± di r ± dr H ± dH j k sh(if insecure)
            out = "{} & & {} & {} & {} & {} & {} & ".format(r['object'],
                                                            round_sig_error(r['a'], r['a_E']),
                                                            round_sig_error(r['e'], r['e_E']),
                                                            round_sig_error(r['i'], r['i_E']),
                                                            round_sig_error(r['dist'], r['dist_E']),
                                                            r['H_sur'])
            if r['j'] != -1:
                out += "{}:{} & ".format(r['j'], r['k'])  # resonant object: give the resonance
            else:
                out += " "  # it's a classical or scattered object
            if r['sh'] != 'S':
                out += "{} {} \n".format(r['sh'], r'\\')
            else:
                out += " {} \n".format(r'\\')
            ofile.write(out)
        ofile.write(footer)


def release_to_latex(outfile):
    tnos = ossos_release_parser(table=True)
    uncharacterised = tnos[numpy.array([name.startswith("u") for name in tnos['object']])]
    characterised = tnos[numpy.array([name.startswith("o") for name in tnos['object']])]
    create_table(characterised, outfile)
    create_table(uncharacterised, 'u_' + outfile)


def parse_subaru_radec(line):
    d = line.split()
    pointing_name = line.split('=')[0]
    ra = d[1].split('=')[1]
    dec = d[2].split('=')[1]
    if len(ra.split('.')[0]) == 5:  # LACK OF SEPARATORS ARGH
        ra = '0' + ra
    if len(dec.split('.')[0]) == 5:
        dec = '0' + dec
    ra = "{}:{}:{}".format(ra[0:2], ra[2:4], ra[4:])
    dec = "{}:{}:{}".format(dec[0:2], dec[2:4], dec[4:])
    return pointing_name, ra, dec


def parse_subaru_mags():
    tnos = ossos_discoveries(parameters.REAL_KBO_AST_DIR, suffix='ast')
    index = OrderedDict()
    with open('/Users/michele/Desktop/Col3N.txt', 'r') as infile:
        lines = infile.readlines()
        for line in lines:
            pointing, ra, dec = parse_subaru_radec(line)
            objs = [t for t in pointing.split('_') if t not in ['only', 'alternate', 'bonus']]
            mags = []
            regular = []
            for obj in objs:
                mag = [t.discovery.mag for t in tnos if t.name.endswith(obj)][0]
                mags.append(mag)
                if len([t for t in parameters.COLOSSOS if t.endswith(obj)]) == 0:
                    regular.append(obj)
            index[pointing] = (ra, dec, mags, regular)
            print pointing, index[pointing]

    with open('/Users/michele/Desktop/Col3N_201509121000.txt', 'w') as outfile:
        outfile.write('{: <25} {: <15} {: <15} {: <30} {: <10}\n'.format('Pointing',
                                                                         'RA (hh:mm:ss)',
                                                                         'Dec (dd:mm:ss)',
                                                                         'magnitudes at discovery (m_r)',
                                                                         'regular OSSOS'))
        for key, val in index.items():
            outfile.write('{: <25} {: <15} {: <15} {: <30} {: <10}\n'.format(key,
                                                                             val[0],
                                                                             val[1],
                                                                             ', '.join([str(f) for f in val[2]]),
                                                                             ', '.join([f for f in val[3]])
            ))


if __name__ == '__main__':
    ossos_release_parser(table=True)
    release_to_latex('v{}'.format(parameters.RELEASE_VERSION) + '_table.tex')
    # parse_subaru_mags()
