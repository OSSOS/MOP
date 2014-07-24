

To submit a step3.py job, for example:

(be careful, these examples have newlines in them)

( vcat vos:OSSOS/triplets/E_13A_discovery_expnums.txt | while read exp1 exp2 exp3 field ; do for ccd in {0..35} ;
do ./submit_job.sh `date -u +%Y-%m-%dT%H:%M:%S`_step3_${exp1}_${ccd} step3.py --force ${exp1} ${exp2} ${exp3} --ccd ${ccd} ;
 done ; done  ) &> submit.out &


for expnum in `vls vos:OSSOS/dbimages` ; do for ccd in $(seq -f "%02g" 0 35) ;
do status=`vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#mkpsf_${ccd}` ;
[ $status != 'success' ] || echo ${expnum}, ${ccd}, ${status} ; done ; done ;

Log formatting now EXPOSURE_CCD_SCRIPT_DATE

cat ~/Desktop/13Nov_ids.txt | while read exp1 ;
do ./submit_job.sh ${exp1}_preproc_`date -u +%Y-%m-%dT%H:%M:%S` preproc.sh ${exp1} ; done

 cat ~/Desktop/OSSOS\ misc/13B_ids.txt |
 while read exp1 ;
 do for ccd in {0..35} ;
 do ./submit_job.sh `date -u +%Y-%m-%dT%H:%M:%S`_step1_${exp1}_${ccd} step1.py ${exp1} --ccd ${ccd} ;
 done ;
 done

# e.g. reset tags if everything went spla somehow

cat decdarkrun.txt | while read exp1 ; do vtag vos:OSSOS/dbimages/${exp1} ivo://canfar.uvic.ca/ossos#preproc_o36= ; done


cat undone.txt | while read exp1 ; do ./submit_job.sh ${exp1}_update_header_`date -u +%Y-%m-%dT%H:%M:%S` update_header.sh ${exp1} ; done



For 2013B we must set the angle of search and planting (the default
values for those scripts are for spring fields). Based on the rate.pl
script (part of the MOP pipeline) we should be searching at angles of
between -56 and +10 degrees (so -23 +/- 33 degrees)

thus, plant and step3 become :  

plant.py --ang -23 --width 33  ....

step3.py --angle -23 --wdith 33  ....


