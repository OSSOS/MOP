"""
Select candidates from a .astrom file based on certain qualities.

Takes a .cands.astrom file as argument.

"""
from ossos import astrom
import sys
import os
from . import match


filename = sys.argv[1]

dirname = os.path.dirname(filename)
basename = os.path.basename(filename)
scramble_filename = os.path.join(dirname, basename[3:])


parser = astrom.AstromParser()
fake_candidates = parser.parse(filename)
for observation in fake_candidates.observations:
    observation.fk = ""

planted_objects = match.PlantedObject(fake_candidates)

(fk_index, planted_index) = match.planted(fake_candidates, planted_objects, tolerance=8.0)

print(fk_index.mask)
print(fk_index)
print(planted_index)

detections = fake_candidates.get_sources()

faint_fptr = open(scramble_filename, 'w+')
faint_cands = astrom.StreamingAstromWriter(faint_fptr,
                                           sys_header=fake_candidates.sys_header)

for idx in range(len(fk_index)):
    # Record source if no matching Object.planted entry
    print(detections[idx], fk_index[idx])
    if fk_index.mask[idx]:
        faint_cands.write_source(detections[idx])

faint_fptr.close()
