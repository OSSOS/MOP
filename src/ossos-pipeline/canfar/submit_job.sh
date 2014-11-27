#!/bin/bash

## Set up the service variables
export proc_service="http://www.canfar.phys.uvic.ca/proc/auth"
export proxy_pem=${HOME}/.ssl/cadcproxy.pem
export condor_header=condor_header.txt

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
   echo "/usr/local/bin/curl --fail -X POST -F condor_submit=<${condor_submit} -F payload=<${payload} ${proc_service}?job=${condor_submit},param:condor_submit&exec=${payload},param:payload"
   /usr/local/bin/curl --fail --user jkavelaars:junkb0nd \
    -X POST \
    -F "condor_submit=<${condor_submit}" \
    -F "payload=<${payload}" \
    "${proc_service}?job=${condor_submit},param:condor_submit&exec=${payload},param:payload"
  status=$?
  [ $status -ne 0 ]  && sleep 3
  echo "Submission status: $status ($jobid)"
  done

rm ${condor_submit}
