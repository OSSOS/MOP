#{ KINKY.CL -- The KINKY package set.  Add task declarations here
# for any kinky subpackages.

cl < "kinky$lib/zzsetenv.def"

package kinky, bin = kinkybin$

task $step0matt	= $step0matt
task $stepZjmp	= $stepZjmp
task $step0jmp	= $step0jmp
task $step1jmp	= $step1jmp
task $kbo_gen = $kbo_gen
task $rmdir  =  $rmdir
task astrom.pkg     = "astrom$astrom.cl"
task deep.pkg     = "deep$deep.cl"
task fkbo.pkg     = "fkbo$fkbo.cl"
task kboplant.pkg     = "kboplant$kboplant.cl"
task lstools.pkg     = "lstools$lstools.cl"
task pipeline.pkg     = "pipeline$pipeline.cl"
task utils.pkg     = "utils$utils.cl"



cl < kinky$kinky.load
keep 

clbye()
#if (motd)
#type kinky$kinky.motd
#;


