#!/bin/bash
cd payload
tar czf ../payload.tgz ./*
cd ..
if [ -e "payload.tgz" ]; then
    cat decompress.sh payload.tar.gz > selfextract.bsx
else
    echo "payload.tar.gz does not exist"
    exit 1
fi

echo "selfextract.bsx created"
exit 0
