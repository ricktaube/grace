#!/bin/sh

# Guarantee the current directory is the top level directory
cd `dirname $0`
cd ..
if [ ! -f "Grace.jucer" ] ; then
  echo "Script is not running from the correct directory."
  echo "Try calling this script from the top-level grace directory."
  exit 1
fi

USAGE="
    Usage: Scripts/snap.sh [filename] 

    Creates a zip file of all modified sources under the top-level
    grace directory.  If filename is specified it will be used as
    the name of zip file otherwise the file will be named
    'grace-ymdhm.zip', where y is year, m is month, d is day, etc."

FILE=

if [[ "$#" == 0 ]] ; then
    FILE=`basename \`pwd\``
    FILE=`date +"${FILE}-%y%m%d_%H%M.zip"`
elif [[ "$#" > 1 || $1 == "--help" || $1 == "-h" ]] ; then
    echo "${USAGE}"
    exit 1
else
   FILE="$1"
fi

git status -s | grep ^[" M""A"] | awk '{print $2;}' | zip ${FILE} -@

