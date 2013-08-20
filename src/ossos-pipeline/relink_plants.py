"""
Correct the mistake where the planted images linked to the non-scrambled zeropoint/apcor file.
"""
import sys

__author__ = 'jjk'


from ossos import storage


triples=storage.open_vos_or_local('vos:OSSOS/triplets/E_13A_discovery_expnums.txt').read()
for line in triples.split('\n'):
    line = line.strip(' ')
    if not len(line) > 0:
        continue
    for expnum in line.split(' ')[0:3]:
        for ccd in range(36):
            for ext in ['.apcor',
                        '.zeropoint.used',
                        '.psf.fits',
                        '.mopheader',
                        '.fwhm',
                        '.trans.jmp']:
                storage.delete(expnum=expnum, ccd=ccd, version='s', ext=ext, prefix='fk')
                storage.vlink(s_expnum=expnum,
                            s_ccd=ccd,
                            s_version='s',
                            s_prefix='',
                            s_ext=ext,
                            l_expnum=expnum,
                            l_ccd=ccd,
                            l_version='s',
                            l_prefix='fk',
                            l_ext =ext)
        print expnum,ccd,"DONE"
sys.exit()