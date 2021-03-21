import json
import argparse
import numpy
import sys
import copy
from astropy.coordinates import SkyCoord
from astropy import units
import operator


class Program(object):
    def __init__(self, runid="16BP06", pi_login="gladman"):
        self.config = {"runid": runid,
                       "pi_login": pi_login,
                       "program_configuration": {"targets": [],
                                                 "observing_blocks": [],
                                                 "observing_groups": []
                                                 }}

    def add_target(self, target):
        self.config["program_configuration"]["targets"].append(target)

    def add_observing_block(self, observing_block):
        self.config["program_configuration"]["observing_blocks"].append(observing_block)

    def add_observing_group(self, observing_group):
        self.config["program_configuration"]["observing_groups"].append(observing_group)


class Target(object):
    def __init__(self, filename=None):
        self.config = json.load(open(filename))

    @property
    def token(self):
        return self.config["identifier"]["client_token"]

    @property
    def mag(self):
        return self.config["moving_target"]["ephemeris_points"][0]["mag"]

    @property
    def coordinate(self):
        return SkyCoord(self.config["moving_target"]["ephemeris_points"][0]["coordinate"]["ra"],
                        self.config["moving_target"]["ephemeris_points"][0]["coordinate"]["dec"],
                        unit='degree')


class ObservingBlock(object):
    def __init__(self, client_token, target_token):
        self.config = {"identifier": {"client_token": client_token},
                       "target_identifier": {"client_token": target_token},
                       "constraint_identifiers": [{"server_token": "C1"}],
                       "instrument_config_identifiers": [{"server_token": "I1"}]}

    @property
    def token(self):
        return self.config["identifier"]["client_token"]


class ObservingGroup(object):
    def __init__(self, client_token):
        self.config = {"identifier": {"client_token": client_token},
                       "observing_block_identifiers": []}

    def add_ob(self, client_token):
        self.config["observing_block_identifiers"].append({"client_token": client_token})


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('pi_login')
    parser.add_argument('qrunid')
    parser.add_argument('runid')
    parser.add_argument('targets', nargs='+')
    parser.add_argument('--og-idx-start', default=0) 
    args = parser.parse_args()

    # Break the targets into OBs based on their max mag of source in pointing.
    cuts = numpy.array([23.0, 23.5, 24.0, 24.5, 25.0, 25.5, 26.0, 30.0])
    IC_exptimes = [50,  100,  200,  300,  400,  500,  600, 700]

    program = Program(runid=args.runid, pi_login=args.pi_login)
    ob_tokens = []
    mags = {}
    ob_coordinate = {}
    for filename in args.targets:
        target = Target(filename)
        program.add_target(target.config)
        ob_token = "OB-{}-{}".format(target.token, target.mag)
        ob = ObservingBlock(ob_token, target.token)
        idx = (target.mag > cuts).sum() + 4
        idx = 2
        ob.config["instrument_config_identifiers"] = [{"server_token": "I{}".format(idx)}]
        program.add_observing_block(ob.config)
        ob_tokens.append(ob_token)
        mags[ob_token] = target.mag
        ob_coordinate[ob_token] = target.coordinate

    sf = lambda x, y: cmp(x.ra, y.ra)
    order_tokens = sorted(ob_coordinate, cmp=sf, key=ob_coordinate.get)
    total_itime = 0
    ogs = {}
    scheduled = {}
    og_idx = int(args.og_idx_start)
    if False:
    # while len(scheduled) < len(ob_tokens):
        og_idx += 1
        og_token = "OG_{}_{}_{}_{}".format(args.runid, args.qrunid, og_idx, 0)
        sys.stdout.write("{}: ".format(og_token))
        og = ObservingGroup(og_token)
        og_coord = None
        og_itime = 0
        for ob_token in order_tokens:
            if ob_token not in scheduled:
                if og_coord is None:
                    og_coord = ob_coordinate[ob_token]
                if ob_coordinate[ob_token].separation(og_coord) > 30 * units.degree:
                    continue
                og.add_ob(ob_token)
                scheduled[ob_token] = True
                sys.stdout.write("{} ".format(ob_token))
                sys.stdout.flush()
                idx = (mags[ob_token] > cuts).sum()
                idx = 1
                print(ob_token, mags[ob_token], idx + 4)
                og_itime += IC_exptimes[idx] + 40
                if og_itime > 3000.0:
                    break
                break
        total_itime += og_itime
        sys.stdout.write(" {}s \n".format(og_itime))
        program.add_observing_group(og.config)
        nrepeats = 0
        for repeat in range(nrepeats):
            total_itime += og_itime
            og_token = "OG_{}_{}_{}_{}".format(args.runid, args.qrunid, og_idx, repeat + 1)
            og = copy.deepcopy(og)
            og.config["identifier"]["client_token"] = og_token
            program.add_observing_group(og.config)
    # print "Total I-Time: {} hrs".format(total_itime/3600.)
    json.dump(program.config, open('program.json', 'w'), indent=4, sort_keys=True)
