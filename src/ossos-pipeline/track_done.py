__author__ = 'michele'

from ossos import mpc
from ossos.orbfit import Orbfit
from ossos import storage

import numpy

# the various track processing dirs
DISCOVERIES = 'vos:OSSOS/measure3/2013A-E/track/discoveries/'
FP = 'vos:OSSOS/measure3/2013A-E/track/false_positives/'
NAILING = 'vos:OSSOS/measure3/2013A-E/track/nailing/'
LOST = 'vos:OSSOS/measure3/2013A-E/track/lost/'
NC = 'vos:OSSOS/measure3/2013A-E/track/need_comparitor/'
SUBMITTED = 'vos:OSSOS/measure3/2013A-E/track/submitted/'
CHECKUP = 'vos:OSSOS/measure3/2013A-E/track/checkup/'

def get_names(path, blockID):
    """
    :param path: the VOSpace path to folder
    :param blockID: the block ID, formatted as 'O13AE' etc
    :return: a set containing those provisional designations present in the folder
    """
    retval = set()
    for fn in storage.listdir(path):
        name = [f for f in fn.split('.') if f.startswith(blockID)][0]
        retval.add(name)

    return retval

def copy_unconsidered(names):
    local_path = '/Users/michele/measure3/2013A-E/genuine_reals_run/tracking/discoveries/'
    discovery_files = storage.listdir(DISCOVERIES)
    for fn in discovery_files:
        for name in names:
            if fn.__contains__(name):
                print fn
                storage.copy(DISCOVERIES+fn, local_path+fn)

    return

def parse(name, subs, path=SUBMITTED):
    filename = ''
    for sub in subs:
        if sub.__contains__(name):
            filename = path+sub
    filehandle = storage.open_vos_or_local(filename, "rb")
    filestr = filehandle.read()
    filehandle.close()
    input_mpc_lines = filestr.split('\n')

    mpc_observations = []
    for line in input_mpc_lines:
        mpc_observation = mpc.Observation.from_string(line)
        if mpc_observation is not None:
            mpc_observations.append(mpc_observation)
    mpc_observations.sort(key=lambda obs: obs.date.jd)
    length_of_observation_arc = mpc_observations[-1].date.jd - mpc_observations[0].date.jd
    orbit = Orbfit(mpc_observations)

    return length_of_observation_arc, orbit



if __name__=='__main__':
    blockID = 'O13AE'
    # the nine I first sent to Brett as a small initial batch
    initial_submitted = ['O13AE2M', 'O13AE3O', 'O13AE3R', 'O13AE3Y', 'O13AE3Z', 'O13AE41', 'O13AE45', 'O13AE4D', 'O13AE4J']
    discoveries = get_names(DISCOVERIES, blockID)
    false_positives = get_names(FP, blockID)
    nailings = get_names(NAILING, blockID)
    lost = get_names(LOST, blockID)
    need_comparator = get_names(NC, blockID)
    submitted = get_names(SUBMITTED, blockID)
    arc_extended = get_names(CHECKUP, blockID)

    # check no names in common exist in either!
    for dd in discoveries:
        if dd in false_positives:
            print 'Oh no! discovery also exists in false positives', dd
    print 'Discoveries:', len(discoveries)

    for fp in false_positives:
        if fp in discoveries:
            print 'Oh no! false positive still in discoveries.', fp
    print 'False positives:', len(false_positives)

    # a discovery must have these kinds of counterparts:
    #       must have a nailing or it could be lost (lost should have a nailing file, but might not)
    #       or it must need a comparator. Or it could have been submitted in that first batch.
    unnailed = []
    unconsidered = []
    for dd in discoveries:
        if (dd not in nailings) and (dd not in lost) and (dd not in need_comparator) and (dd not in submitted):
            print 'Not yet considered discovery!', dd
            unconsidered.append(dd)
    print 'Not yet considered:', len(unconsidered)
#    copy_unconsidered(lost)

    print 'Nailed discoveries:', len(nailings)+len(initial_submitted)
    print 'Lost discoveries:', len(lost)
    print lost
    print 'Needs comparison image before nailing possible:', len(need_comparator)
    print need_comparator

    # lost all have nailings files (some of zero length) so don't need to count them twice
    print 'Total:', sum([len(unconsidered), len(nailings), len(need_comparator), len(initial_submitted)])

    # If a discovery has a nailing, it must also have a submitted - unless it was part of that first batch
    print 'Submitted:', len(submitted)
    n_and_sub = []
    for dd in discoveries:
        if (dd in nailings) and (dd in submitted):
            n_and_sub.append(dd)
        else:
            if (dd in nailings) and (dd not in submitted):
                print 'nailing, but not submitted?', dd
            if ((dd in submitted) and (dd not in nailings)) and (dd not in initial_submitted):
                print 'submitted but not nailed...', dd

    # how many nailed files have checkups?
    # (don't have to include initial_submitted in these, they don't have nailings or checkups)
    for nn in nailings:
        if (nn not in arc_extended):
            print 'nailed, but arc not extended?', nn

    # What's the arc length on the nailed ones?
    subs = storage.listdir(SUBMITTED)
    arclens = []
    for ss in submitted:
        arclen, orbit = parse(ss, subs)
        arclens.append(arclen)

    arclens.sort()
    print arclens
    print len(arclens)

    bins = [0,1,4,20,40,60,90,150]
    hist,bin_edges = numpy.histogram(arclens,bins=bins)
    print hist