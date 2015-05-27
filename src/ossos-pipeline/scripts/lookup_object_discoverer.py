__author__ = 'Michele Bannister   git:@mtbannister'

''' For objects declared discoveries that need submitting to the MPC,
    find the reals.astrom that corresponds to that object,
    the .cands.astrom that sourced that reals.astrom,
    and identify the holder of the #done tag on the .cands.astrom file.
'''

from planning.plotting import parsers, parameters
from ossos import storage

# Had to resort to lookup table as everything got non-standard from L block onward. Ah well.
# also H block seem to not have 'O14BH' at start yet. Check those after a release exists.
paths = {
    'O13AE': '2013A-E',  # where are all the E block .mpc files?
    'O13AO': '2013A-O',
    'O13BL': '2013B-L_redo',
    'O14BH': '2015B-H',
}
files = {}
triplets = {}

designations = {}
# first designation is the most advanced one/matches the release name, so use that as the key
for line in storage.open_vos_or_local(parameters.IDX).read().split('\n'):
    names = line.split()
    if len(names) > 0:
        designations[names[0]] = names[1:]

discoveries = parsers.ossos_release_parser(table=True)
with open('/Users/michele/Desktop/13Adiscoveries.txt', 'w') as outfile:
    outfile.write('{:<12} {:<12} {:<12} {:<12} {:<20} {:<20}\n'.format(
        'Object', 'Provisional', 'First-accept', 'Chip', 'Q-observer', 'Q-coord'))
    for discovery in discoveries['object']:
        # get older designations, which would have been assigned as the original discovery name
        former_names = designations[discovery]
        # cf. ossos.naming.ProvisionalNameGenerator()
        provisional_name = filter(lambda name: (name.startswith('O1') and name.isupper()), former_names)[0]
        outfile.write('{:<12} {:<12}'.format(discovery, provisional_name))

        # build the path that the .cands.astrom files would be in.
        block = paths[provisional_name[0:5]]
        path = '{}/{}/'.format(storage.MEASURE3, block)
        if discovery in ['o3e53', 'o3e54', 'o3e55']:  # deal with the special case: their .cands.astrom are elsewhere
            path = '{}/{}/'.format(storage.MEASURE3, '2013A-E_April9')
        if block not in files:
            if block in ['2013A-E', '2013A-O']:
                # 13AE and 13AO ended up particularly weird. Fortunately, have a local list
                with open('/Users/michele/Desktop/{}_mpclist.txt'.format(block), 'r') as infile:
                    block_files = infile.readlines()
            else:
                print 'starting VOSpace read'  # getting file listings from big listings on VOSpace is slooooowwww...
                block_files = filter(lambda name: not name.startswith('fk'), storage.listdir(path))

            files[block] = block_files
        else:
            block_files = files[block]

        # not all provisional names have the full Oyearsemesterblock pattern. Because counter.
        reals_files = [c for c in block_files if
                       (c.__contains__(provisional_name) or c.__contains__(provisional_name[-3:]))]
        if len(reals_files) > 0:  # ensure it's present in this directory
            chip = reals_files[0].split('.')[0]
            cand_filename = '{}.measure3.cands.astrom'.format(chip)
            # now who set the tag?
            uri = path + cand_filename
            discoverer = storage.vospace.getNode(uri, force=True).props[storage.OSSOS_TAG_URI_BASE + '#done']
            outfile.write('{:<12} {:<12}'.format(discoverer, chip))

            # map the exposure number of the triplet to the header value for the Observer
            # FIXME: doesn't work for 14BH because the block is not called that in the path. Will need to overwrite
            triplet_path = '{}/{}_{}_discovery_expnums.txt'.format(storage.TRIPLETS, block[-1:], block[2:5])
            triplets = storage.open_vos_or_local(triplet_path).read().split('\n')
            triplet = [n for n in triplets if n.endswith(chip.split('_')[0])][0]
            header = storage.get_astheader(triplet.split()[0], chip.split('_')[1].strip('p'))
            outfile.write('{:<20} {:<20}\n'.format(header['QOBSERVE'], header['QCOORD']))
