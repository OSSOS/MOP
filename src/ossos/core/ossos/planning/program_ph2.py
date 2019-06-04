from ossos.ph2 import *

if __name__ == "__main__":

    program = Program(runid="16BE91", pi_login="mwilson")
    program_configuration = ProgramConfiguration()
    program.program_configuration = program_configuration

    constraint = Constraint(Identifier(server_token="C1"))

    instrument_config = InstrumentConfig(Identifier(server_token="I1"))

    moving_target = MovingTarget()
    coordinate = Coordinate(10., 10.)
    ephmeris_point = EphmerisPoint(540000.0, coordinate)
    moving_target.ephmeris_points.append(ephmeris_point)
    target = Target(identifier=Identifier(client_token="target_identifier"),
                    name="target_name",
                    moving_target=moving_target)

    ob = observing_block = ObservingBlock(Identifier(client_token="ob_identifier"),
                                          target=target,
                                          constraint=constraint,
                                          instrument_config=instrument_config)

    og = ObservingGroup(Identifier(client_token="og_identifier"))
    og.observing_block_identifiers.append(ob.identifier)

    program_configuration.observing_blocks.append(ob)
    program_configuration.observing_groups.append(og)
    program_configuration.targets.append(target)
    print((program.to_json()))
