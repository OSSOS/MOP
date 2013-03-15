# sunrise.py
# Retrieve last night's observations, check they're fine, start processing if possible.
#
# MTB 2013-03-14


	# retrieve image info from CADC via TAP: incorporate src/jjk/preproc/ObsStatus.py
	ask_cadc_for_observations
	# also getExposures.pl
	# also need vos, VOspace tuned for CADC
	# examine for contiguous image sets, images missing from ends of each field's observing
	# populate OSSOS metadata db, ossuary, with info
	# process info to see if any fields in that batch of observation had three images in the night
	# report via email on result of night's observing
	# \copy postgres_db to stdout with csv header  (this'll be a daily dump of the db to disk)
	# if fields > 0, initiate next step of processing, preproc.
