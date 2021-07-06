import json
import argparse
import sys
from astropy.coordinates import SkyCoord


class Program(object):
    def __init__(self, runid="17AF22", pi_login="marsset"):
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
    args = parser.parse_args()

    # Break the targets into OBs based on their max mag of source in pointing.
    OB = {"I1": "r", "I2": "u"}
    OG = ["I1"] + ["I2"]*14 + ["I1"]
    
    program = Program()
    ob_tokens = {}
    runid = "17AF22"
    targets = {}
    og_idx = 0
    for filename in args.targets:
        target = Target(filename)
        targets[target.token] = target
        program.add_target(target.config)
        for idx in ["I1", "I2"]:
           ob_token = "OB-{}-{}-{}".format(runid, target.token, OB[idx])
           ob = ObservingBlock(ob_token, target.token)
           ob.config["instrument_config_identifiers"] = [{"server_token": "{}".format(idx)}]
           program.add_observing_block(ob.config)
        og_idx += 1
        og_token = "OG_{}_{}_{}_{}".format(args.runid, args.qrunid, og_idx, 0)
        sys.stdout.write("{}: ".format(og_token))
        og = ObservingGroup(og_token)
        for IC in OG:
            ob_token = "OB-{}-{}-{}".format(runid, target.token, OB[IC])
            og.add_ob(ob_token)
        program.add_observing_group(og.config)

    json.dump(program.config, open('program.json', 'w'), indent=4, sort_keys=True)
