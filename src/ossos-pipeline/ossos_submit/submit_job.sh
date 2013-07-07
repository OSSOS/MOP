task=$1
./build_job.sh 
for fileId in `vcat vos:OSSOS/triplets/E_13A_discovery_expnums.txt` ; 
do 
  expr  $fileId : 'E.*'  && continue
  for ccd in {0..35} ; 
  do 
    python build_condor_submit.py $task $fileId $ccd > condor_submit.in
    echo "submitting $task $fileId"
    curl -k -E ~/.ssl/cadcproxy.pem -X POST -F 'job_sub=<condor_submit.in' -F 'exec_ls=<selfextract.bsx' "https://www.canfar.phys.uvic.ca/proc/pub?job=jobFile,param:job_sub&exec=execFile,param:exec_ls"
  done
done
