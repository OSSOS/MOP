## where is the file that contains a list of triples?
export proc_service="https://www.canfar.phys.uvic.ca/proc/pub"
export proxy_pem=${HOME}/.ssl/cadcproxy.pem
export triples=vos:OSSOS/triplets/E_13A_discovery_expnums.txt
export task=mop.sh
export condor_submit=condor_submit.in
export condor_header=condor_header.txt

# create a fresh payload
payload=`./create_payload.sh` || exit 1

# build the submit file and post to proc_service
vcat ${triples} | while read exp1 exp2 exp3 job_id
do 
  for ccd in {0..35} ; 
  do 
    cat ${condor_header} | sed "s/PAYLOAD/${payload}/" > ${condor_submit}
    echo "" >> ${condor_submit}
    echo "Arguments = ${job_id}_${ccd} $task $exp1 $exp2 $exp3 $ccd "  >> ${condor_submit}
    echo "QUEUE" >> ${condor_submit}
    echo "" >> ${condor_submit}
    echo "submitting ${job_id} $task ${job_id}_${ccd}"
    curl -v -k -E ${proxy_pem} \
            -X POST \
            -F "condor_submit=<${condor_submit}" \
            -F "payload=<${payload}" \
            "${proc_service}?job=${condor_submit},param:condor_submit&exec=${payload},param:payload"
    exit 0
  done
done
