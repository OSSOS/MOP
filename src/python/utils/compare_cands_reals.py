"""
Select candidates from a .astrom file based on certain qualities.

Takes a .cands.astrom file as argument.

"""
from ossos import astrom
import sys
from astropy.io import ascii
import match
import os
import numpy

parser = astrom.AstromParser()

cands_filename = sys.argv[1]
fake_candidates = parser.parse(cands_filename)

planted_objects = match.PlantedObject(fake_candidates)

reals_filename = cands_filename.replace('cands', 'reals')
fake_reals = parser.parse(reals_filename)


missed_filename = os.path.join("missed", os.path.basename(cands_filename))
missed_fptr = open(missed_filename, 'w+')
missed_cands = astrom.StreamingAstromWriter(missed_fptr, sys_header=fake_candidates.sys_header)


(cands_index, cands_planted_index) = match.planted(fake_candidates, planted_objects, tolerance=10.0)
(reals_index, reals_planted_index) = match.planted(fake_reals, planted_objects, tolerance=10.0)
assert isinstance(cands_index, numpy.ma.MaskedArray)
assert isinstance(reals_index, numpy.ma.MaskedArray)

lost = []
found = []
for idx in range(len(cands_index)):
    if not cands_index.mask[idx]:
        found.append(cands_planted_index[cands_index[idx]])
        if reals_planted_index.mask[cands_index[idx]]:
            lost.append(cands_planted_index[cands_index[idx]])
            missed_cands.write_source(fake_candidates.get_sources()[idx])

if len(found) > 0:
    ascii.write(planted_objects.table[found], os.path.basename(reals_filename)+".fnd")

if len(lost) > 0:
    ascii.write(planted_objects.table[lost], os.path.basename(reals_filename)+".false_negative")
