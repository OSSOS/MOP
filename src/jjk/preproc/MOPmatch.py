#!/usr/bin/env python
# JJK 
# USAGE:  MOPmatch.py files
import sys, os

command="match.csh "
for file in sys.argv[1:]:
    base=os.path.splitext(file)
    file=base[0]
    if not os.access(file+'.mopheader',os.R_OK):
        os.system("stepZjmp -f %s" % ( file, ) )
    if not os.access(file+'.obj.psf',os.R_OK):
        os.system("step0jmp -f %s -w %f -t 5 -m 20000" % ( file, 4) ) 
    if not os.access(file+'.obj.jmp',os.R_OK):
        os.system("step1jmp -f %s -w %f -t 5 -m 20000" % ( file, 4) ) 

    command+=" "+file

print command
os.system(command)

