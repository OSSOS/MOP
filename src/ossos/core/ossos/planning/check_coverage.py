#!python
"""
Query the CADC for all exposures that brett is the PI of and taken in 2017
observations of the TNOs listed on the command line
"""

from astropy import units
from astropy.coordinates import SkyCoord
from astropy.time import TimeDelta, Time
import numpy as np
from ossos.cameras import Camera
from ossos import mpc
import mp_ephem
from ossos.ephem_target import EphemTarget
import sys, os
import argparse
import logging
import math
from copy import deepcopy
import ephem
from astropy.table import Table
import requests
from io import StringIO

cfht = ephem.Observer()
cfht.lat = 0.344
cfht.lon = -2.707
cfht.elevation = 4100
cfht.date = '2018/03/28 20:00:00'
sun = ephem.Sun()
fb = ephem.FixedBody()


def query():
    url = """http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/tap/sync?LANG=ADQL&REQUEST=doQuery&QUERY=SELECT%20Observation.observationURI%20AS%20%22Preview%22%2C%20Observation.collection%20AS%20%22Collection%22%2C%20Observation.sequenceNumber%20AS%20%22Sequence%20Number%22%2C%20Plane.productID%20AS%20%22Product%20ID%22%2C%20COORD1(CENTROID(Plane.position_bounds))%20AS%20%22RA%20(J2000.0)%22%2C%20COORD2(CENTROID(Plane.position_bounds))%20AS%20%22Dec.%20(J2000.0)%22%2C%20Observation.target_name%20AS%20%22Target%20Name%22%2C%20Plane.time_bounds_lower%20AS%20%22Start%20Date%22%2C%20Plane.time_exposure%20AS%20%22Int.%20Time%22%2C%20Observation.instrument_name%20AS%20%22Instrument%22%2C%20Plane.energy_bandpassName%20AS%20%22Filter%22%2C%20Plane.calibrationLevel%20AS%20%22Cal.%20Lev.%22%2C%20Observation.type%20AS%20%22Obs.%20Type%22%2C%20Observation.proposal_id%20AS%20%22Proposal%20ID%22%2C%20Observation.proposal_pi%20AS%20%22P.I.%20Name%22%2C%20Plane.dataRelease%20AS%20%22Data%20Release%22%2C%20Observation.observationID%20AS%20%22Obs.%20ID%22%2C%20Plane.energy_bounds_lower%20AS%20%22Min.%20Wavelength%22%2C%20Plane.energy_bounds_upper%20AS%20%22Max.%20Wavelength%22%2C%20AREA(Plane.position_bounds)%20AS%20%22Field%20of%20View%22%2C%20Plane.position_bounds%20AS%20%22Polygon%22%2C%20Plane.position_sampleSize%20AS%20%22Pixel%20Scale%22%2C%20Plane.energy_resolvingPower%20AS%20%22Resolving%20Power%22%2C%20Plane.time_bounds_upper%20AS%20%22End%20Date%22%2C%20Plane.dataProductType%20AS%20%22Data%20Type%22%2C%20Observation.target_moving%20AS%20%22Moving%20Target%22%2C%20Plane.provenance_name%20AS%20%22Provenance%20Name%22%2C%20Plane.provenance_keywords%20AS%20%22Provenance%20Keywords%22%2C%20Observation.intent%20AS%20%22Intent%22%2C%20Observation.target_type%20AS%20%22Target%20Type%22%2C%20Observation.target_standard%20AS%20%22Target%20Standard%22%2C%20Plane.metaRelease%20AS%20%22Meta%20Release%22%2C%20Observation.algorithm_name%20AS%20%22Algorithm%20Name%22%2C%20Observation.proposal_title%20AS%20%22Proposal%20Title%22%2C%20Observation.proposal_keywords%20AS%20%22Proposal%20Keywords%22%2C%20Plane.position_resolution%20AS%20%22IQ%22%2C%20Observation.instrument_keywords%20AS%20%22Instrument%20Keywords%22%2C%20Plane.energy_transition_species%20AS%20%22Molecule%22%2C%20Plane.energy_transition_transition%20AS%20%22Transition%22%2C%20Observation.proposal_project%20AS%20%22Proposal%20Project%22%2C%20Plane.energy_emBand%20AS%20%22Band%22%2C%20Plane.provenance_reference%20AS%20%22Prov.%20Reference%22%2C%20Plane.provenance_version%20AS%20%22Prov.%20Version%22%2C%20Plane.provenance_project%20AS%20%22Prov.%20Project%22%2C%20Plane.provenance_producer%20AS%20%22Prov.%20Producer%22%2C%20Plane.provenance_runID%20AS%20%22Prov.%20Run%20ID%22%2C%20Plane.provenance_lastExecuted%20AS%20%22Prov.%20Last%20Executed%22%2C%20Plane.provenance_inputs%20AS%20%22Prov.%20Inputs%22%2C%20Plane.energy_restwav%20AS%20%22Rest-frame%20Energy%22%2C%20Observation.requirements_flag%20AS%20%22Quality%22%2C%20Plane.planeID%20AS%20%22planeID%22%2C%20isDownloadable(Plane.planeURI)%20AS%20%22DOWNLOADABLE%22%2C%20Plane.planeURI%20AS%20%22CAOM%20Plane%20URI%22%20FROM%20caom2.Plane%20AS%20Plane%20JOIN%20caom2.Observation%20AS%20Observation%20ON%20Plane.obsID%20%3D%20Observation.obsID%20WHERE%20%20(%20Plane.calibrationLevel%20%3D%20'1'%20AND%20Observation.instrument_name%20%3D%20'MegaPrime'%20AND%20Observation.collection%20%3D%20'CFHT'%20AND%20INTERSECTS(%20INTERVAL(%2057785.0%2C%201.7976931348623157E308%20)%2C%20Plane.time_bounds%20)%20%3D%201%20AND%20lower(Observation.proposal_pi)%20LIKE%20'%25gladman%25'%20AND%20%20(%20Plane.quality_flag%20IS%20NULL%20OR%20Plane.quality_flag%20!%3D%20'junk'%20)%20)&FORMAT=votable"""
    return Table.read(StringIO(requests.get(url).content))


def coverage(orbits, locations):
    """

    @param orbits: a dictionary of orbits to optimize the point of.
    @param locations: list of CFHT pointings.
    @return:
    """

    # For each required target find the pointing that will include the largest number of other required targets
    # and then tweak that specific pointing to include the maximum number of secondary targets.
    for location in locations:
        p = Camera(SkyCoord(location["RA__J2000.0_"], location["Dec.__J2000.0_"], unit=('degree', 'degree')))
        for kbo_name in orbits:
            orbits[kbo_name].predict(Time(location["Start_Date"], format='mjd'))
            if p.polygon.isInside(orbits[kbo_name].coordinate.ra.degree,
                                  orbits[kbo_name].coordinate.dec.degree):
                print((location['Target_Name'], kbo_name))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('pointing_objects',
                        type=str,
                        help="Name of file that contains list of objects to try and observe.")
    parser.add_argument('--runid',
                        help="CFHT ID for this QUEUE Run",
                        default='16BQ17')

    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    logging.info("Optimized to include as many objects in {} as possible".format(args.pointing_objects))

    filenames = open(args.pointing_objects).readlines()

    orbits = {}
    tokens = []

    # load the orbits of all objects of interest.
    for filename in filenames:
        filename = filename.strip()
        token = os.path.splitext(os.path.basename(filename))[0]
        abg_filename = os.path.splitext(filename)[0]+".abg"
        orbits[token] = mp_ephem.BKOrbit(None, ast_filename=filename, abg_file=abg_filename)
        tokens.append(token)

    coverage(orbits, query())


if __name__ == '__main__':
    sys.exit(main())



