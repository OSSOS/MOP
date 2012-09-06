# JJK Nov 2001
# 
# Input => a CFH12k image in MEF format 
# Output => Set of directories with pre-processed single CCD images


procedure cfh12k_preproc (mef)

string mef	{"",prompt="The name of the CFH12k image to pre-process"}
string logfile  {"status",prompt="File which tracks the progress of the processing"}
pset   ccdproc  {"",prompt="CCDPROC paramters"}
pset   movem    {"",prompt="MOVEM parameters"}

begin
	string t_mef, subdir, t_logfile
	bool   ans
	t_mef = mef

	t_logfile = logfile;

	# Correct the cfht headers (so mscred doesn't complain)
	edhead (t_mef)
	# Do the actual pre-processing.
	ccdproc (t_mef)
	# Now send all those images into their subdirs.
	movem (t_mef)


	
end
