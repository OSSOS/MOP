./build_job.sh
curl -v -k -E ~/.ssl/cadcproxy.pem -X POST -F 'job_sub=<condor_submit.in' -F 'exec_ls=<selfextract.bsx' "https://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/proc/pub?job=jobFile,param:job_sub&exec=execFile,param:exec_ls"
