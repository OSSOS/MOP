__author__ = 'bannisterm'

'''
    For all objects in the database, check if they have images taken by OSSOS on which they are predicted to fall
    but on which their astrometry/photometry have not yet been measured to generate a recorded MPC line.
    Output a list of objects to work on, and a list of images that have failed processing for any reason.
'''

#import ephem

from ossos import storage, mpc, ssos
from ossos.gui import context, tasks
#from ossos.planning.ObsStatus import query_for_observations
from ossos.planning.plotting import parameters, parsers


if __name__ == '__main__':
    outfile = 'need_to_process.txt'

    working_context = context.get_context(parameters.REAL_KBO_AST_DIR)
    files = working_context.get_listing('ast')

    print('Examining {} object files.'.format(len(files)))

    for fn in files[28:]:
        print('{}'.format(fn))
        parser = ssos.TracksParser(skip_previous=True)
        tracks_data = parser.parse(parameters.REAL_KBO_AST_DIR + fn)

        if len(tracks_data.observations) > 1:  # it is set to always return the discovery image
            print('{} unmeasured observations!'.format(len(tracks_data.observations) - 1))
            obj = parsers.TNO(mpc.MPCReader(parameters.REAL_KBO_AST_DIR + fn))

            for obs in tracks_data.observations:
                discovery_frame = obj.discovery.comment.frame.split('p')[0].strip(' ')
                if obs.expnum != discovery_frame:  # are these new?
                    print('{}'.format(obs.get_mpc_date()))
                    # Any processing-related reason why this image hasn't been measured yet? If so we need to process.
                    assert storage.get_status('update_header', '', obs.expnum, "p", 36)

                    if not storage.get_status('mkpsf', '', obs.expnum, "p", obs.ccdnum) or \
                            storage.get_status('mk_mopheader', '', obs.expnum, "p", obs.ccdnum):
                        with open(outfile, 'a') as ofile:
                            ofile.write('{}\n'.format(obs.expnum))



    # mjd_yesterday = ephem.date(ephem.julian_date(ephem.date(parameters.SURVEY_START))) - 2400000.5
    #
    # obs_table = query_for_observations(mjd_yesterday, 1, parameters.OSSOS_RUNIDS)
    #
    # populated = storage.list_dbimages()
    # for i in range(len(obs_table) - 1, -1, -1):
    #     row = obs_table.data[i]
    #     if row['dataset_name'] in populated:
    #         #
