
import os
import string
import vos
import sys

header = """Universe   = vanilla
Executable = execFile
getenv     = True
notify_user = jjkavelaars@nrc-cnrc.gc.ca
notification = Complete
should_transfer_files = YES
transfer_input_files = /home/jkavelaars/.ssl/cadcproxy.pem
when_to_transfer_output = ON_EXIT
transfer_output_files = /dev/null
RunAsOwner = True
#on_exit_remove = (ExitCode == 0)

Requirements = VMType =?= "MOP"  && \
               Arch == "x86_64" && \
	       Memory >= 4000 && \
               Cpus >= 1 

+VMLoc         = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/vospace/jkavelaars/MOP.img.gz"
+VMMem         = "4096"
+VMCPUCores    = "1"
+VMStorage     = "20"
"""
print header

script = sys.argv.pop(0)
task = sys.argv.pop(0)
ccd = sys.argv.pop()
fileId = sys.argv.pop()

print  "Arguments = ./scripts/%s %s_%s %s --ccd %s" % (task, fileId, ccd, fileId, ccd)
print  "Log = %s_%s_%s.log" % ( task, fileId, ccd)
print  "Error = %s_%s_%s.err"  % ( task, fileId, ccd)
print  "Queue"
print  ""
