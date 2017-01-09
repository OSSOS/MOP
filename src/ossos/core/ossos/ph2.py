"""CFHT PH2 objects"""
import json


class PH2(object):

    def to_json_string(self):
        return json.dumps(self.to_dict())

    def from_json_string(self, value):
        parts = json.loads(value)
        for part in parts:


    def to_dict(self):
        _dict = self.__dict__
        for part in _dict:
            if isinstance(_dict[part], PH2):
                _dict[part] = _dict[part].to_dict()
            if isinstance(_dict[part], list):
                _dict[part] = [item.to_dict() for item in _dict[part]]
        return _dict


class Program(PH2):

    def __init__(self, runid, pi_login):
        self.runid = runid
        self.pi_login = pi_login
        self.program_configuration = None


class ProgramConfiguration(PH2):

    def __init__(self):
        self.targets = []
        self.observing_blocks = []
        self.observing_groups = []


class Identifier(PH2):

    def __init__(self, server_token=None, client_token=None):
        if server_token is not None:
            self.server_token = server_token
        if client_token is not None:
            self.client_token = client_token


class Coordinate(PH2):

    def __init__(self, ra, dec):
        assert isinstance(ra, float)
        assert isinstance(dec, float)
        self.ra = ra
        self.dec = dec


class EphmerisPoint(PH2):

    def __init__(self, epoch, coordinate):
        assert isinstance(epoch, float)
        self.epoch = epoch
        assert isinstance(coordinate, Coordinate)
        self.coordinate = coordinate


class Target(PH2):

    def __init__(self, identifier=None, name=None, moving_target=None, fixed_target=None):
        self.identifier = identifier
        self.name = name
        if moving_target is not None:
            assert isinstance(moving_target, MovingTarget)
            self.moving_target = moving_target
        else:
            self.fixed_target = fixed_target


class MovingTarget(PH2):

    def __init__(self, ephemeris_points=None):

        if ephemeris_points is None:
            self.ephmeris_points = []
        else:
            assert isinstance(ephemeris_points, list)
            self.ephmeris_points = ephemeris_points


class ObservingBlock(PH2):

    def __init__(self, identifier, target, constraint, instrument_config):
        """
        An Observing Block holds a chunk of information that describes an single observations of a target.

        @type identifier: Identifier
        @type target: Target
        @type constraint: Constraint
        @type instrument_config: InstrumentConfig
        @param identifier: identifier of this observing block, unique within the runid
        @param target: the target of this observing block
        @param constraint: the observing constraint to apply
        @param instrument_config: the instrument configuration to use
        """
        self.identifier = identifier
        self.target_identifier = target.identifier
        self.constraint_identifier = constraint.identifier
        self.instrument_config_identifier = instrument_config.identifier


class ObservingGroup(PH2):

    def __init__(self, identifier):
        """
        A Group of Observing Blocks that should be observed together in a sequence.

        @type identifier: Identifier
        @param identifier: A unique string within this program that labels this Observing Group
        """
        self.identifier = identifier
        self.observing_block_identifiers = []


class Constraint(PH2):

    def __init__(self, identifier):
        """
        The constaraint on the observations (IQ, Photometric, Airmass, etc). Currently only set via web form.

        @type identifier; Identifier
        @param identifier: A unique, within this program, identifier of a constraint set.
        """

        self.identifier = identifier


class InstrumentConfig(PH2):
    """
    The instrument configuration on the observations (Exposure Time, Filter, Dither Pattern)

    @type identifier; Identifier
    @param identifier: A unique, within this program, identifier of a instrument configuration set.
    """

    def __init__(self, identifier):
        self.identifier = identifier


