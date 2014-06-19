#!python
import os
import args
import tarfile
import cadc_certificates

## Set up the service variables
PROC_SERVICE_URL="https://www.canfar.phys.uvic.ca/proc/pub"
PROXY_CERT="payload/cadcproxy.pem".format(HOME)
CONDOR_HEADER="condor_header.txt"

def create_payload(filename='payload.bsx'):
    
    cadc_certificates.getCert(certfile=PROXY_CERT)

    tar = tarfile.open('payload.tgz', mode='w:gz')
    for name in [PROXY_CERT, payload]
    cd payload
    tar czf ../payload.tgz ./*
    cd ..
    if [ -e "payload.tgz" ]; then
    cat decompress.sh payload.tgz > ${outfile}
else
echo "payload.tgz does not exist"
    exit 1
fi

echo ${outfile}
exit 0



## Grad the command to run and arguments
## Usage: jobid task args
jobid=$1
shift
task=$1
shift
args=$@
condor_submit=${jobid}.in

# create a fresh payload
payload=`./create_payload.sh` || exit 1



# build the submit file and post to proc_service
cat ${condor_header} | sed "s/PAYLOAD/${payload}/" > ${condor_submit}
echo "" >> ${condor_submit}
echo "Arguments = ${jobid} $task $args "  >> ${condor_submit}
echo "Log = ${jobid}.log" >> ${condor_submit}
echo "Error = ${jobid}.err" >> ${condor_submit}
echo "QUEUE" >> ${condor_submit}
echo "" >> ${condor_submit}
echo "submitting ${job_id} $task $args"
status=1
while [ $status -ne 0 ] ;  do 
   /usr/local/opt/curl/bin/curl --fail -k -E ${proxy_pem} \
    -X POST \
    -F "condor_submit=<${condor_submit}" \
    -F "payload=<${payload}" \
    "${proc_service}?job=${condor_submit},param:condor_submit&exec=${payload},param:payload"
  status=$?
  [ $status -ne 0 ]  && sleep 3
  echo "Submission status: $status ($jobid)"
  done

rm ${condor_submit}
