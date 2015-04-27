
from ossos import storage
import logging
MKPSF = 'mkpsf'  # name of process that build a PSF


def mkpsf_failures():
    """A simple script to loop over the standard tags for the mkpsf and
    step1 processing steps. If exposure/ccd combo isn't marked as
    'success' then report the failure.
    
    This example uses the vos client directly.
    """

    for expnum in storage.list_dbimages():
        for ccd in range(36):
            if not storage.get_status(MKPSF, "", expnum, "p", ccd):
                # get_status returns FALSE if process didn't succeed,
                # with return_message=True it returns the error message.
                print expnum, ccd, storage.get_status(MKPSF, "", expnum, "p", ccd, return_message=True)


if __name__=='__main__':
    logging.basicConfig(level=logging.CRITICAL)
    mkpsf_failures()
