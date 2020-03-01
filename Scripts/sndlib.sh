#!/bin/bash

cd `dirname $0`
cd ../

if [ ! -f "Grace.jucer" ] ; then
  echo "sndlib.sh is not running from the correct directory."
  echo "Try calling this script from the top-level cm directory."
  exit 1
fi

USAGE="
    Usage: sndlib.sh  [--nobuild]

    installs and builds sndlib as a static library on OSX Linux and Windows
    OPTIONS:
       --nobuild    download sources but do not build library
       --download   (same as nobuild)
"

JACK=
NOBUILD=0

for arg in "$@" ; do
    case $arg in
        --with-jack) JACK="--with-jack" ;;
        --nobuild|download) NOBUILD=1 ;;
        --help|*) echo "usage:" $USAGE
           exit 1 ;;
    esac    
done

TOP=`pwd`

if [ -f "sndlib/lib/libsndlib.a" ] ; then
    exit 0
fi

if [ ! -e "sndlib/sndlib.h" ] ; then
    if ! which curl >/dev/null; then
	echo "*** Error: Cannot find curl on your path."
	echo "Install sndlib sources in ${TOP}/sndlib and try premake4 again."
	exit 1
    fi
    echo "=== Downloading Sndlib into ${TOP}/sndlib ===="
    mkdir sndlib
    curl ftp://ccrma-ftp.stanford.edu/pub/Lisp/sndlib.tar.gz | tar -zx
fi

if [[ $NOBUILD == 1 ]] ; then
    exit 1
fi

cd sndlib

if [ ! -f "Makefile" ] ; then
    echo "=== Configuring Sndlib ===="
    echo "premake4 --with-g++"
    premake4 --with-g++
fi

echo "=== Making Sndlib ===="
make verbose=1

exit 0
