"""
Select candidates from a .astrom file based on certain qualities.

Takes a .cands.astrom file as argument.

"""
from ossos import astrom
import sys
import os
import match

parser = astrom.AstromParser()

filename = sys.argv[1]
fake_candidates = parser.parse(filename)
planted_objects = match.PlantedObject(fake_candidates)

(fk_index, planted_index) = match.planted(fake_candidates, planted_objects, tolerance=10.0)

print fk_index
print planted_index

detections = fake_candidates.get_sources()

# Write the faint/bright candidates to the faint/bright directory
faint_filename = os.path.join("faint", os.path.basename(filename))
faint_fptr = open(faint_filename, 'w+')
faint_cands = astrom.StreamingAstromWriter(faint_fptr, sys_header=fake_candidates.sys_header)

bright_filename = os.path.join("bright", os.path.basename(filename))
bright_fptr = open(bright_filename, 'w+')
bright_cands = astrom.StreamingAstromWriter(bright_fptr, sys_header=fake_candidates.sys_header)


for idx in range(len(fk_index)):
    # Record source if no matching Object.planted entry or the planted source is faint
    if fk_index.mask[idx] or planted_objects.table['mag'][fk_index[idx]] > 24.00:
        faint_cands.write_source(detections[idx])
    else:
        bright_cands.write_source(detections[idx])


faint_fptr.close()
bright_fptr.close()
