

To submit a step3.py job, for example:

( vcat vos:OSSOS/triplets/E_13A_discovery_expnums.txt | while read exp1 exp2 exp3 field ; do for ccd in {0..35} ; do ./submit_job.sh `date +%s`_step3_${exp1}_${ccd} step3.py --force ${exp1} ${exp2} ${exp3} --ccd ${ccd} ; done ; done  ) &> submout.out &
