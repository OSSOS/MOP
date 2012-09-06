#{ PIPELINE - The KBO pipeline processing system

package pipeline


task apcor_munge = "pipeline$scripts/apcor_munge.cl"
task check_pipe = "pipeline$scripts/check_pipe.cl"
task cfh12k_preproc = "pipeline$scripts/cfh12k_preproc.cl"
task hansalign  = "pipeline$scripts/hansalign.cl"
task hfilt  = "pipeline$scripts/hfilt.cl"
task look = "pipeline$scripts/look.cl"
#task lsmop = "pipeline$scripts/lsmop.cl"
task makepsf = "pipeline$scripts/makepsf.cl"
task pipefixpix = "pipeline$scripts/pipefixpix.cl"
task mpcphot = "pipeline$scripts/mpcphot.cl"
task mpcauto = "pipeline$scripts/mpcauto.cl"
task pipeplant = "pipeline$scripts/pipeplant.cl"
task pipeplantjmp = "pipeline$scripts/pipeplantjmp.cl"
task pipescan = "pipeline$scripts/pipescan.cl"
task prepimage = "pipeline$scripts/prepimage.cl"
task dompc = "pipeline$scripts/dompc.cl"
task usno_phot = "pipeline$scripts/usno_phot.cl"
task usno_calib = $usno_calib.pl
task measure3 = $measure3
task newpopulate = "pipeline$scripts/newpopulate.cl"
task jmpmakepsf = "pipeline$scripts/jmpmakepsf.cl"
task jmpprepimage = "pipeline$scripts/jmpprepimage.cl"

clbye()
