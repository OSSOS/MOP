# coding=utf-8

import logging
import os
import re
import sys
import pickle
from collections import OrderedDict
import math
import ephem
from astropy.table import Table
from uncertainties import ufloat
import numpy
import pandas
from ossos import (mpc, orbfit, parameters, storage)
from ossos.planning.plotting.utils import square_fit_discovery_mag
from ossos.planning.plotting.deluxe_table_formatter import deluxe_table_formatter, extreme_tno_table_formatter
__author__ = 'Michele Bannister   git:@mtbannister'


# from parameters import tno

def ossos_release_parser(table=False, data_release=parameters.RELEASE_VERSION):
    """
    extra fun as this is space-separated so using CSV parsers is not an option
    """
    names = ['cl', 'p', 'j', 'k', 'sh', 'object', 'mag', 'e_mag', 'Filt', 'Hsur', 'dist', 'e_dist', 'Nobs',
             'time', 'av_xres', 'av_yres', 'max_x', 'max_y', 'a', 'e_a', 'e', 'e_e', 'i', 'e_i', 'Omega', 'e_Omega',
             'omega', 'e_omega', 'tperi', 'e_tperi', 'RAdeg', 'DEdeg', 'JD', 'rate']#, 'eff', 'm_lim']

    if table:
        retval = Table.read(parameters.RELEASE_DETECTIONS[data_release], format='ascii', guess=False,
                           delimiter=' ', data_start=0, comment='#', names=names, header_start=None)
    else:
        retval = []
        with open(data_release, 'r') as detectionsfile:
            for line in detectionsfile.readlines()[1:]:  # first line is column definitions
                obj = TNO.from_string(line, version=parameters.RELEASE_DETECTIONS[data_release])
                retval.append(obj)

    return retval


def ossos_objects_parser(fn):
        names = ['object', 'mag', 'mag_E', 'dist', 'dist_E', 'nobs',
             'time', 'av_xres', 'av_yres', 'max_x', 'max_y', 'a', 'a_E', 'e', 'e_E', 'i', 'i_E', 'node', 'node_E',
             'argperi', 'argperi_E', 'time_peri', 'time_peri_E', 'ra_dis', 'dec_dis',]
        retval = Table.read(fn, format='ascii', guess=False,
                           delimiter=' ', data_start=0, comment='#', names=names, header_start=None)
        return retval


class TNO(object):

    def __init__(self, observations, ast_filename=None, abg_filename=None):

        if observations is None:
          try:
            self.orbit = orbfit.Orbfit(None, ast_filename, abg_filename)
            self.name = os.path.basename(ast_filename).split('.')[0]
          except Exception as ex:
            logging.error("Failed to compute orbit using inputs {}".format(ast_filename))
            raise ex
        else:
            self.orbit = orbfit.Orbfit(observations.mpc_observations, ast_filename, abg_filename)
            try:
                # Previously discovered objects can have 'discovery-marked' lines that predate the OSSOS survey.
                discoveries = [n for n in observations.mpc_observations if (n.discovery.is_discovery and n.date > parameters.SURVEY_START)]
                # Some objects were linked to independent OSSOS-observed triples; these are also multiply 'discovery-marked'
                if len(discoveries) > 1:
                    discoveries.sort(key=lambda x: x.date)
                # once sorted and restricted to within the survey timeframe, it's always the earliest such marked line.
                self.discovery = discoveries[0]
            except Exception as ex:
                self.discovery = False
                self.name = observations[0].provisional_name.rstrip('.ast').split('.')[0]
        self.mag = None
        for observation in self.orbit.observations:
            if observation.mag is not None:
                self.mag = (self.mag is None or self.mag < observation.mag) and observation.mag or self.mag


def ossos_discoveries(directory=parameters.REAL_KBO_AST_DIR,
                      suffix='ast',
                      no_nt_and_u=False,
                      single_object=None,
                      all_objects=True,
                      data_release=None,
                      ):
    """
    Returns a list of objects holding orbfit.Orbfit objects with the observations in the Orbfit.observations field.
    Default is to return only the objects corresponding to the current Data Release.
    """
    retval = []
    # working_context = context.get_context(directory)
    # files = working_context.get_listing(suffix)
    files = [f for f in os.listdir(directory) if (f.endswith('mpc') or f.endswith('ast') or f.endswith('DONE'))]

    if single_object is not None:
        files = [name for name in files if name.startswith(single_object)]
    elif all_objects and data_release is not None:
        # only return the objects corresponding to a particular Data Release
        data_release = ossos_release_parser(table=True, data_release=data_release)
        objects = data_release['object']
        files = [name for name in files if name.partition(suffix)[0].rstrip('.') in objects]

    for filename in files:
        # keep out the not-tracked and uncharacteried.
        if no_nt_and_u and (filename.__contains__('nt') or filename.startswith('u')):
            continue
        # observations = mpc.MPCReader(directory + filename)
        mpc_filename = directory + filename
        abg_filename = os.path.abspath(directory + '/../abg/') + "/" + os.path.splitext(filename)[0] + ".abg"
        obj = TNO(None, ast_filename=mpc_filename, abg_filename=abg_filename)
        retval.append(obj)

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


def _kbos_from_survey_sym_model_input_file(model_file):
    """
    Load a Survey Simulator model file as an array of ephem EllipticalBody objects.
    @param model_file:
    @return:
    """
    lines = storage.open_vos_or_local(model_file).read().split('\n')
    kbos = []
    for line in lines:
        if len(line) == 0 or line[0] == '#':  # skip initial column descriptors and the final blank line
            continue
        kbo = ephem.EllipticalBody()
        values = line.split()
        kbo.name = values[8]
        kbo.j = values[9]
        kbo.k = values[10]
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
        kbos.append(kbo)
    return kbos


def kbos_from_survey_sym_model_input_file(infile):
    # names = ['a', 'e', 'i', 'node', 'peri', 'M', 'H', 'distance', 'orbtype', 'j', 'k']

    # 3 components (hot [h], cold kernel [c] and cold stirred [f])
    names = ['a', 'e', 'i', 'node', 'peri', 'M', 'H', 'distance', 'component', 'orbtype']

    kbos = pandas.read_table(infile, names=names, delim_whitespace=True, comment='#')

    return kbos


def synthetic_model_kbos(at_date=parameters.NEWMOONS[parameters.DISCOVERY_NEW_MOON], maglimit=24.5, kbotype=False,
                         arrays=False):
    # # build a list of Synthetic KBOs
    kbos = []
    print("LOADING SYNTHETIC MODEL KBOS FROM: {}".format(parameters.L7MODEL))
    pastpath = parameters.L7_HOME + str(at_date).split()[0].replace('/', '-') + '_' + str(maglimit) + (
        kbotype or '') + '.dat'
    print(pastpath)

    # Load the previously cached orbits, quick.
    if os.path.exists(pastpath) and arrays:
        with open(pastpath) as infile:
            ra, dist, hlat, Hmag = pickle.load(infile)
        print(('{} synthetic model kbos brighter than {} at {} in L7 model'.format(len(ra), maglimit, at_date)))
        return ra, dist, hlat, Hmag

    model_kbos = _kbos_from_survey_sym_model_input_file(parameters.L7MODEL)

    counter = 0
    ra = []
    dist = []
    hlat = []
    Hmag = []

    for kbo in model_kbos:
        if kbotype and (kbo.name == kbotype) and (kbo.j == '3' and kbo.k == '2'):  # keeping 3:2 resonators
            date = ephem.date(at_date)
            kbo.compute(date)
            counter += 1
            # ## only keep objects that are brighter than limit
            if kbo.mag < maglimit:
                kbos.append(kbo)
                ra.append(float(kbo.ra))
                dist.append(float(kbo.sun_distance))
                hlat.append(float(kbo.hlat))
                Hmag.append(float(kbo._H))

    print('{} synthetic model kbos brighter than {} at {} retained from {} in L7 model'.format(len(kbos), maglimit,
                                                                                               at_date, counter))
    if not arrays:
        return kbos
    else:
        with open(pastpath, 'w') as outfile:
            pickle.dump((ra, dist, hlat, Hmag), outfile)
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


def round_sig_error(num, uncert, pm=False):
    """
    Return a string of the number and its uncertainty to the right sig figs via uncertainty's print methods.
    The uncertainty determines the sig fig rounding of the number.
    https://pythonhosted.org/uncertainties/user_guide.html
    """
    u = ufloat(num, uncert)
    if pm:
        return '{:.1uL}'.format(u)
    else:
        return '{:.1uLS}'.format(u)


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
              "} \n"

    return midline


def extract_mpc_designations(initial_id=['o3', 'o4', 'o5']):
    # Scrape the index file for MPC designations, if they exist
    idx = {}
    with open(parameters.IDX) as infile:
        lines = infile.readlines()
        for line in lines:
            key = line.split(' ')[0].split('\t')[0]
            if key[0:2] in initial_id:
                mpcstr = ""
                ls = line[14:].split(' ')
                if len(ls) == 3:
                    ls = ls[1:]
                    mpcstr = "{} {}".format(ls[0], ls[1][0:2]) + r"$_{" + "{}".format(ls[1][2:].rstrip('\r\n')) + r"}$"
                idx[key] = mpcstr

    return idx


def extract_highest_designation(initial_id):
    # Scrape the index file for MPC designations, if they exist
    idx = {}
    with open(parameters.IDX) as infile:
        lines = infile.readlines()
        for line in lines:
            designations = line.split(' ')
            if initial_id in designations:
                # Highest internal OSSOS one will be leftmost. FIXME: some logic for getting MPC designations, to right
                return designations[0].split('\t')[0]

def stddev_phot(observations):
    """
    Compute the flux variation over the set of observations provided.
    @param observations:
    @return: float
    """
    return None


def create_table(tnos, outfile, initial_id='o3'):
    # sort order gives Classical, Detached, Resonant, Scattered
    # Sort by discovery mag within each classification.
    tnos.sort(['cl', 'p', 'j', 'k', 'mag'])

    tnos = [r for r in tnos if r['object'][0:3] in ['o3l', 'o4h', 'o5p', 'o5m']]  # hack for the next Core paper only

    # FIXME: add a catch if object doesn't yet have a MPC designation
    idx = extract_mpc_designations()

    with deluxe_table_formatter(outfile) as ofile:
        for i, r in enumerate(tnos):
            if r['object'][0:3] not in ['o3l', 'o4h', 'o5p', 'o5m']: # hack for the next Core paper only
                continue
            # a labelled line separator heading for each new object classification type
            if r['p'] != tnos[i - 1]['p']:
                if r['p'] == 'x':  # 'x' doesn't give enough info to set scattered or detached: go check
                    ofile.write(linesep(r['p'], distinguish=r['cl']))
                else:
                    ofile.write(linesep(r['p']))

            # Mag uncertainty is over the whole light curve. Uncharacterised objects might not have enough valid points.
            sigma_mag = stddev_phot(r['object'])
            if sigma_mag is None:
                sigma_mag = r'--'
            else:
                sigma_mag = '{:2.2f}'.format(sigma_mag)

            # Individual object discovery efficiency from the function based on its mag + motion rate at discovery
            # eff_at_discovery = square_fit_discovery_mag(r['object'], r['mag'], r['rate'])
            eff_at_discovery = r'--'

            # mag +\- dmag, std dev of all clean photometry, efficiency function at that discovery mag
            # m +\- dm sigma_m eff_discov RA Dec a +\- da e +\- de i +\- di r +\- dr H +\- dH j k MPC obj status
            out = "{} & {} & {} & {:3.3f} & {} & {} & {} & {} & {} & {} & {} & {} & ".format(
                round_sig_error(r['mag'], r['e_mag']),
                sigma_mag,
                eff_at_discovery,
                math.degrees(ephem.degrees(str(r['RAdeg']))),
                r['DEdeg'],
                round_sig_error(r['a'], r['e_a']),
                round_sig_error(r['e'], r['e_e']),
                round_sig_error(r['i'], r['e_i']),
                round_sig_error(r['dist'], r['e_dist']),
                r['Hsur'],
                idx[r['object']],  # MPC designation is already formatted.
                r['object']
            )
            # output the orbit classification with nice formatting, or leave a gap if unclassified (e.g. a classical)
            if r['j'] != -1:
                out += "{}:{} ".format(r['j'], r['k'])  # resonant object: give the resonance
            else:
                out += "    "  # it's a classical or scattered object
            if r['sh'] != 'S':
                out += "{} {} \n".format(r['sh'], r'\\')
            else:
                out += "  {} \n".format(r'\\')
            ofile.write(out)


def release_to_latex(outfile):
    tnos = ossos_release_parser(table=True)
    characterised = tnos[numpy.array([name.startswith("o") for name in tnos['object']])]
    create_table(characterised, outfile, initial_id='o3')

    # uncharacterised = tnos[numpy.array([name.startswith("u") for name in tnos['object']])]
    # create_table(uncharacterised, 'u_' + outfile)

    return


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
            print(pointing, index[pointing])

    with open('/Users/michele/Desktop/Col3N_201509121000.txt', 'w') as outfile:
        outfile.write('{: <25} {: <15} {: <15} {: <30} {: <10}\n'.format('Pointing',
                                                                         'RA (hh:mm:ss)',
                                                                         'Dec (dd:mm:ss)',
                                                                         'magnitudes at discovery (m_r)',
                                                                         'regular OSSOS'))
        for key, val in list(index.items()):
            outfile.write('{: <25} {: <15} {: <15} {: <30} {: <10}\n'.format(key,
                                                                             val[0],
                                                                             val[1],
                                                                             ', '.join([str(f) for f in val[2]]),
                                                                             ', '.join([f for f in val[3]])
                                                                             ))


def block_table_pprint():
    """

    :return:
    """
    with open('block_table.tex', 'w') as outfile:
        for name, coords in list(parameters.BLOCKS.items()):
            print(name, coords)
            ra = ephem.hours(coords['RA'])
            dec = ephem.degrees(coords['DEC'])
            eq = ephem.Equatorial(ra, dec)
            ec = ephem.Ecliptic(eq)
            outfile.write("{} & {:2.1f} & {:2.1f} & {:2.1f} & {:2.1f} {} \n".format(
                name[2:],
                math.degrees(ephem.degrees(ra)),
                math.degrees(dec),
                math.degrees(ec.lat),
                math.degrees(ec.lon),
                r"\\"))
    return


def read_smooth_fit(fichier):
    """

    :param fichier:
    :return:
    """
    pd, ps, mag_limit = None, None, None
    with open(fichier, 'r') as infile:
        lines = infile.readlines()
        for line in lines:
            if line[0] != "#":
                if line.startswith("double_param="):
                    pd = list(map(float, line[13:-1].split()))
                if line.startswith("square_param="):
                    ps = list(map(float, line[13:-1].split()))
                if line.startswith("mag_lim="):
                    mag_limit = float(line.split()[1])

    return pd, ps, mag_limit


# Number_Mil={'B': 110000, 'C': 120000, 'D': 130000, 'E': 140000, 'F': 150000}
#Number_Cent={'J': 1900, 'K': 2000}
def date_unpack(pdate):
    (yyyy, mm, dd) = (2000, 0o1, 0o1)
    try:
        YY = {'I': 1800, 'J': 1900, 'K': 2000}
        Ncode = '0123456789ABCDEFGHIJKLMNOPQRSTUV'
        yyyy = YY[pdate[0]] + int(pdate[1:3])
        mm = Ncode.rindex(pdate[3])
        dd = float(Ncode.rindex(pdate[4]))
    except:
        sys.stderr.write("ERROR converting date part {}:".format(pdate))
    return (yyyy, mm, dd)


def mpcorb_desig_unpack(desig):
    if re.match('\d+', desig):
        return str(int(desig))
    Kilo = 'ABCDEFGHIJKLMNOPQRSTUV'
    j = Kilo.rfind(desig[0])
    if j != -1:
        if re.match('^\d+$', desig[1:7]):
            return str(100000 + j * 10000 + int(desig[1:7]))
    YY = {'I': 1800, 'J': 1900, 'K': 2000}
    try:
        yyyy = str(YY[desig[0]] + int(desig[1:3]))
    except KeyError as e:
        return desig
    Ncode = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    Mcode = desig[3] + desig[6]
    cycle = Ncode.rindex(desig[4]) * 10 + int(desig[5])
    if (cycle > 0):
        cycle = str(cycle)
    else:
        cycle = ''
    return yyyy + ' ' + Mcode + cycle


def mpcorb_getKBOs(mpc_file, cond='a > 30'):
    f = open(mpc_file)
    lines = f.readlines()
    f.close()

    # while "------" not in lines[0]:
    #     lines.pop(0)
    lines.pop(0)
    nobj = 0
    lineCount = 0
    kbos = []
    for line in lines:
        lineCount = lineCount + 1
        if line[0] == '#' or len(line) < 103 or line[0:3] == '---':
            continue
        kbo = ephem.EllipticalBody()

        if len(line[8:13].strip()):
            kbo._H = float(line[8:13])
            kbo._G = float(line[14:19])
        else:
            kbo._H = 20
            kbo._G = 0
        arc = line[127:136]
        try:
            if 'days' in arc:
                arc = int(arc.split()[0]) / 365.25
            else:
                arc = -eval(arc)
        except:
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

        a = kbo._a
        q = kbo._a * (1 - kbo._e)
        H = kbo._H

        if eval(cond):
            kbo.name = mpcorb_desig_unpack(line[0:7].strip())
            kbos.append(kbo)

    return kbos

def create_extremetno_table(outfile):
    tnos = Table.read('/Users/bannisterm/Dropbox/Papers_in_progress/OSSOS/high_q_uo3l91/results.csv',
                      format='csv', data_start=1)
    with extreme_tno_table_formatter(outfile) as ofile:
        for r in tnos[::-1]:
            # Name q a e i Ω ω π Arc Discovery
            om_bar = r['om']+r['w']
            if om_bar > 360:
                om_bar = om_bar % 360.
            out = "{} & {} & {} & {} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {} & ".format(
                r['full_name'],
                round_sig_error(r['q'], r['sigma_q'], pm=True),
                round_sig_error(r['a'], r['sigma_a'], pm=True),
                round_sig_error(r['e'], r['sigma_e'], pm=True),
                r['i'],
                r['om'],
                r['w'],
                # round_sig_error(r['e'], r['sigma_e'], pm=False),
                # round_sig_error(r['i'], r['sigma_i'], pm=False),
                # round_sig_error(r['om'], r['sigma_om'], pm=False),
                # round_sig_error(r['w'], r['sigma_w'], pm=False),
                om_bar,
                r['data_arc'],
            )
            out += "  {} \n".format(r'\\')
            ofile.write(out)



if __name__ == '__main__':
    # create_extremetno_table('extreme_tnos.tex')

    # ossos_release_parser(table=True)

    release_to_latex('v{}'.format(parameters.RELEASE_VERSION) + '_table.tex')

    # parse_subaru_mags()
    # block_table_pprint()
