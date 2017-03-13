#!/bin/bash
echo ""
echo "Self Extracting Installer"
echo ""

ARCHIVE=`awk '/^__ARCHIVE_BELOW__/ {print NR + 1; exit 0; }' $0`

tail -n+$ARCHIVE $0 | tar xzv -C ./

ls -l
./runner.sh $@
exit 0

__ARCHIVE_BELOW__
