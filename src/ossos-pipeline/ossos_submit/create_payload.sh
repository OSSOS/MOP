#!/bin/bash
outfile=payload.bsx
cd payload
cp ${HOME}/.ssl/cadcproxy.pem ./
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
