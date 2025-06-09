#! /bin/bash

basedir=`pwd`
syst=`uname -s`

export PREFIX=/usr/local/
CATDIR=${PREFIX}
export CATDIR
USNODIR=${PREFIX}/USNO
export USNODIR


export CC="gcc"
export FC="gfortran"

export ARCH=LINUX

BINDIR=${PREFIX}/bin 
export BINDIR
BINDIRA=${PREFIX}/bin/
export BINDIRA
LIBDIRA=${PREFIX}/lib/
export LIBDIRA
LIBDIR=${PREFIX}/lib/
export LIBDIR
INCLUDEDIR=${PREFIX}/include
export INCLUDEDIR 
CONFIGDIR=${PREFIX}/config
export CONFIGDIR 
USNODIR=${PREFIX}/USNO
export USNODIR
CATDIR=${PREFIX}
export CATDIR

if [ ! -d ${PREFIX} ]; then
    mkdir ${PREFIX}
fi

if [ ! -d ${BINDIR} ]; then
    mkdir ${BINDIR}
fi

if [ ! -d ${LIBDIR} ]; then
    mkdir ${LIBDIR}
fi

if [ ! -d ${BINDIRA} ]; then
    mkdir -p ${BINDIRA}
fi

if [ ! -d ${LIBDIRA} ]; then
    mkdir -p ${LIBDIRA}
fi

if [ ! -d ${INCLUDEDIR} ]; then
    mkdir -p ${INCLUDEDIR}
fi

if [ ! -d ${CONFIGDIR} ]; then
    mkdir -p ${CONFIGDIR}
fi

