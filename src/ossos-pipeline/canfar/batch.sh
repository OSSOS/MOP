#!/bin/bash

## Set up the service variables
export proc_service="https://www.canfar.phys.uvic.ca/proc/pub"
export proxy_pem=${HOME}/.ssl/cadcproxy.pem
export condor_header=condor_header.txt

## Grad the command to run and arguments
## Usage: jobid task inputfile
jobid=$1
shift
task=$1
shift
inputfile=$1
shift
condor_submit=${jobid}.in

# create a fresh payload
payload=`./create_payload.sh` || exit 1


# build the submit file and post to proc_service
cat ${condor_header} | sed "s/PAYLOAD/${payload}/" > ${condor_submit}
echo "" >> ${condor_submit}
while read line
  do
  jobid=`echo $line | awk ' { print $1 } ' `
  echo "Arguments = ${jobid} $task ${line} $@ "  >> ${condor_submit}
  echo "Log = ${jobid}.log" >> ${condor_submit}
  echo "Error = ${jobid}.err" >> ${condor_submit}
  echo "QUEUE" >> ${condor_submit}
  echo "" >> ${condor_submit}
done < ${inputfile}
