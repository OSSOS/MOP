#! /bin/bash

basedir=`pwd`
syst=`uname -s`
            
while [ $# -gt 0 ]; do
   case "$1" in     
  -p)
    shift
    PREFIX=$1
    export PREFIX
    shift
    ;;
  --prefix=*)
    PREFIX=`echo $1 | sed 's/--prefix=//'`    
    export PREFIX
    shift
    ;;
  esac
done

if [ X"${PREFIX}" == X ]; then
    echo "What is the root install directory?"
    read ans
    PREFIX=$ans
    export PREFIX
fi

echo $PREFIX
cd $PREFIX
if [ -f MOP-SOURCE-DIR ]; then
    cd ..
fi
PREFIX=`pwd`
export PREFIX
CATDIR=${PREFIX}
export CATDIR
USNODIR=${PREFIX}/USNO
export USNODIR
cd $basedir

CC="gcc"
export CC
FC="gfortran"
export FC

case "$syst"
in
SunOS)
    ARCH=SOLARIS
    if [ X"${FC}" == X ]; then
      FC="f77 -O"
      export FC
    fi
    ;;
Linux)
    ARCH=LINUX
    if [ X"${FC}" == X ]; then
      which g77 > /dev/null 2>&1
      g77exists=$?
      if [ "$g77exists" -eq "0" ]; then
        FC="`which g77` -O2 "
        export FC
      fi
    fi
    if [ X"${FC}" == X ]; then
      which cf77 > /dev/null 2>&1
      cf77exists=$?
      if [ "$cf77exists" -eq "0" ]; then
        FC="`which cf77` -O6"
        export FC
      fi
    fi
    ;;
Darwin)
    if [ X"${FC}" == X ]; then
      FC="gfortran -fPIC -fno-common "
      export FC
    fi
    CC="gcc"
    #CFLAGS="-ftree-vectorize-maltivec -maltivec -lstdc++"
    ARCH=DARWIN
    #export CFLAGS
    export CC
    ;;
esac
export ARCH

BINDIR=${PREFIX}/bin 
export BINDIR
BINDIRA=${PREFIX}/bin/$ARCH
export BINDIRA
LIBDIRA=${PREFIX}/lib/$ARCH
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

