import numpy
import os
import sys

from ossos import mpc
from ossos.orbfit import Orbfit
from ossos import storage

MAXCOUNT = 30000

SSOS_URL = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/ssos/ssos.pl"
RESPONSE_FORMAT = 'tsv'
# was set to \r\n ?
NEW_LINE = '\r\n'

astDir = 'vos:OSSOS/measure3/2013A-E/track/submitted'

def getList(name = None):

    for filename in storage.listdir(astDir):
        if name is None or name in filename:
            build(os.path.join(astDir,filename))


def summarize(orbit):
    for observation in orbit.observations:
            print observation.to_string()

    orbit = Orbfit(orbit.observations)

    print ""
    print orbit

    orbit.predict(orbit.observations[0].date)
    coord1 = orbit.coordinate
    orbit.predict(orbit.observations[-1].date)
    coord2 = orbit.coordinate
    motion_rate = coord1.separation(coord2).arcsecs/(orbit.arc_length*24)  # how best to get arcsec moved between first/last?
    print "{:>10s} {:8.2f}".format('rate ("/hr)', motion_rate)

    orbit.predict('2014-04-04')  # hardwiring next year's prediction date for the moment
    print "{:>10s} {:8.2f} {:8.2f}\n".format("Expected accuracy on 4 April 2014 (arcsec)", orbit.dra, orbit.ddec)

    return



def build(filename):
    filehandle = storage.open_vos_or_local(filename, "rb")
    filestr = filehandle.read()
    filehandle.close()

    input_mpc_lines = filestr.split('\n')

    mpc_observations = []
    mag = []
    for line in input_mpc_lines:
        mpc_observation = mpc.Observation.from_string(line)
        if mpc_observation is not None:
            if mpc_observation.mag is not None and mpc_observation.mag > 0: 
                mag.append(mpc_observation.mag)
                
            mpc_observations.append(mpc_observation)

    mpc_observations.sort(key=lambda obs: obs.date.jd)
    orbit = Orbfit(mpc_observations)

    mag = numpy.array(mag)
    print orbit.residuals

    print "{:10s} {:7.2f} {:7.2f} {:7.3f} {:7.3f} {:7.3f} {:7.3f} {:7.3f} {:7.3f} {:7.3f}".format(orbit.name, orbit.a, orbit.da, orbit.e, orbit.de, orbit.inc, orbit.dinc, orbit.distance, mag.mean(), mag.std())




if __name__ == '__main__':
    name = None
    if len(sys.argv) > 1:
	name = sys.argv[1]
    print "{:10s} {:7s} {:7s} {:7s} {:7s} {:7s} {:7s} {:7s} {:7s} {:7s}".format("NAME","a(AU)","da(AU)","e","de","Inc(d)","dInc","D(AU)","mag_r","merr")
    getList(name)
