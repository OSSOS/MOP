
'''Rename the provisional designation to add information on which field
    the object came from, due to a namespace collision at one point on len(2) designations.
'''

import os
from ossos import mpc

field_ids = {
    '15BD+1+1':'E',
    '15BD+1+0':'F',
    '15BD+1-1':'G',
    '15BD+1-2':'H',
    '15BD+0+1':'J',
    '15BD+0+0':'K',
    '15BD+0-1':'L',
    '15BD+0-2':'M',
}

path = '/Users/bannisterm/Dropbox/OSSOS/measure3/2015B-D/reals_sub/D+1/accepted/'
for filename in [n for n in os.listdir(path) if n.endswith('.mpc')]:
        field = filename.split('_')[0]
        if field in field_ids:
            obj = mpc.MPCReader(filename)
            name = obj.mpc_observations[0].provisional_name
            print(filename, name, field, field_ids[field], len(name.lstrip('D')))
            # now we deal with the ones that need to be renamed because of the namespace collision
            if len(name.lstrip('D')) == 2: # let's make this column all have nice consistently 3-alphanumeric IDs
                unique_name = 'O15BD' + field_ids[field] + name.lstrip('D')
            else:
                unique_name = 'O15B' + name

            print name, unique_name

            # now to amend the names
            for obs in obj.mpc_observations:
                obs.provisional_name = unique_name
                obs.comment.source_name = unique_name

            out_filename = filename.rsplit('.')[0] + '.' + unique_name + '.mpc'
            with open(path + 'renamed/' + out_filename, 'w') as outfile:
                writer = mpc.MPCWriter(outfile, auto_flush=False)
                for obs in obj.mpc_observations:
                    writer.write(obs)
                writer.flush()
