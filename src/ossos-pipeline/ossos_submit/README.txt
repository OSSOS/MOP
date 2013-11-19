

To submit a step3.py job, for example:

(be careful, these examples have newlines in them)

( vcat vos:OSSOS/triplets/E_13A_discovery_expnums.txt | while read exp1 exp2 exp3 field ; do for ccd in {0..35} ;
do ./submit_job.sh `date -u +%Y-%m-%dT%H:%M:%S`_step3_${exp1}_${ccd} step3.py --force ${exp1} ${exp2} ${exp3} --ccd ${ccd} ;
 done ; done  ) &> submit.out &


for expnum in `vls vos:OSSOS/dbimages` ; do for ccd in $(seq -f "%02g" 0 35) ;
do status=`vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#mkpsf_${ccd}` ;
[ $status != 'success' ] || echo ${expnum}, ${ccd}, ${status} ; done ; done ;

cat ~/Desktop/13Nov_ids.txt | while read exp1 ;
do ./submit_job.sh `date -u +%Y-%m-%dT%H:%M:%S`_preproc_${ccd} preproc.sh ${exp1} ; done
