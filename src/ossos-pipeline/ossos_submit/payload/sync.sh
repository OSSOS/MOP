#!/bin/bash
while [ -e log_capture_on ] ; do vcp $1 $2 ; sleep 10 ; done

