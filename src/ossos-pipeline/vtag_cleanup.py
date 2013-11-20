__author__ = 'Michele Bannister   git:@mtbannister'

import argparse
from ossos import storage


def check_status(expnum, old_step, step):
    """
    Queries VOSpace node of image for its metadata and acts on metadata accordingly.
    :param expnum:  int, exposure number
    :param old_step:
    :param step:
    :return: :raise:
    """
    message = storage.SUCCESS
    if not storage.get_status(expnum, ccd, step):
        raise IOError(35, "mkpsf_p hasn't run?")
    if storage.get_status(expnum, ccd, prefix+'step1_p') and not :
        continue

    storage.set_status(expnum,
                       ccd,
                       prefix + step,
                       message)
    return

if __name__=='__main__':
    """
    Query VOSpace node of an image and

    """
    parser=argparse.ArgumentParser(
        description='Clean up tags on given exposures.')

    parser.add_argument("expnum",
                        type=int,
                        nargs='+',
                        help="Exposure numbers(s) to process")

    args=parser.parse_args()
    ccdlist = range(0,36)
    singlesteps = ['preproc', 'preproc_o', 'update_header', 'update_header_p']
    steps = ['mkpsf', 'mkpsf_p', 'step1', 'step1_p', 'step2', 'step2_p', 'step3', 'step3_p', 'combine', 'combine_p',
             'scramble', 'scramble_s', 'plant', 'plant_s', 'mkpsf_s', 'step1_s', 'fkstep1', 'fkstep1_s',
             'fkstep2', 'fkstep2_s', 'fkstep3', 'fkstep3_s', 'fkcombine', 'fkcombine_s']

    for expnum in args.expnum:
        for step in singlesteps:


        for ccd in ccdlist:


