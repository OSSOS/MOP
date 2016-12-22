__author__ = 'bannisterm'

import argparse

from ossos import storage, mpc, ssos
from ossos.gui import context, tasks
from ossos import parameters, parsers


if __name__ == '__main__':
    description = 'Given a block ID (e.g. o5d), report which objects in that block have unmeasured lines of astrometry,' \
                  'as found by SSOIS from the present arc of each object in the block.'
    epilog = '''
    For all objects in the database, check if they have images taken by OSSOS on which they are predicted to fall
    but on which their astrometry/photometry have not yet been measured to generate a recorded MPC line.
    Output a list of objects to work on, and a list of images that have failed processing for any reason.
    '''
    parser = argparse.ArgumentParser(description=description, epilog=epilog)
    parser.add_argument('block',
                        help="The three-digit OSSOS designation for the set of TNOs of interest.",
                        )
    args = parser.parse_args()

    outfile = 'need_to_measure_{}.txt'.format(args.block)

    working_context = context.get_context(parameters.REAL_KBO_AST_DIR)
    files = working_context.get_listing('ast')
    files = [f for f in files if f.startswith(args.block)]

    with open(outfile, 'a') as ofile:
        ofile.write('Examining {} object files.\n'.format(len(files)))

    for fn in files:
        with open(outfile, 'a') as ofile:
            ofile.write('{}\n'.format(fn))
        parser = ssos.TracksParser(skip_previous=True)
        tracks_data = parser.parse(parameters.REAL_KBO_AST_DIR + fn)

        if len(tracks_data.observations) > 1:  # it is set to always return the discovery image
            with open(outfile, 'a') as ofile:
                ofile.write('{} unmeasured observations!\n'.format(len(tracks_data.observations) - 1))
            obj = parsers.TNO(mpc.MPCReader(parameters.REAL_KBO_AST_DIR + fn))

            for obs in tracks_data.observations:
                discovery_frame = obj.discovery.comment.frame.split('p')[0].strip(' ')
                if obs.expnum != discovery_frame:  # are these new?
                    with open(outfile, 'a') as ofile:
                        ofile.write('{}\n'.format(obs.get_mpc_date()))
                    # Any processing-related reason why this image hasn't been measured yet? If so we need to process.
                    # assert storage.get_status('update_header', '', obs.expnum, "p", 36)

                    if not storage.get_status('mkpsf', '', obs.expnum, "p", obs.ccdnum) or \
                            storage.get_status('mk_mopheader', '', obs.expnum, "p", obs.ccdnum):
                        with open(outfile, 'a') as ofile:
                            ofile.write('{}\n'.format(obs.expnum))

