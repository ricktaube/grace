#!/bin/sh

use="cmembed [scm] [doc] [ins] [ffi]\n       Embeds the specified resources in the CM Source tree.\n       Must be run from the top-level CM directory.\n       Requires BinaryBuilder and csi\n"
cwd=`pwd`
actscm=
actdoc=
actins=
actffi=

for arg in "$@" ; do
    case $arg in
        scm) actscm=1 ;;
        doc) actdoc=1 ;;
        ins) actins=1 ;;
        ffi) actffi=1 ;;
        *) echo "usage:" $use
           exit 1 ;;
    esac    
done

if [[ "$#" == 0 ]] ; then
    echo "usage:" $use
    exit 1
fi

if [ ! -f "$cwd/Grace.jucer" ] ; then
    echo "not in toplevel Grace directory"
    exit 1
fi

if [[ "$actscm" == 1 ]] ; then
    echo "embedding scheme sources"
    BinaryBuilder Resources/Scheme Source SchemeSources
fi

if [[ "$actdoc" == 1 ]] ; then
    echo "embedding documentation..."
    cd Resources/
    mkdir tmp
    cd Documentation
    cp doc.xml ../tmp
    zip doc.zip *.html *.css *.scm *.sal
    mv doc.zip ../tmp/
    cd ..
    BinaryBuilder tmp/ ../Source/ Resources
    rm -r tmp
fi

if [[ "$actins" == 1 ]] ; then
    echo "embedding instruments..."
    cd Resources/
    mkdir tmp
    cd ins
    cp ins.xml ../tmp
    zip ins.zip *.scm *.scd
    mv ins.zip ../tmp/
    cd ..
    BinaryBuilder tmp/ ../Source/ Instruments
    rm -r tmp
fi

if [[ "$actffi" == 1 ]] ; then
    csi -b -e '(begin (load "scm/genffi.scm") (s7ffi "src/SndLibBridge.cpp") (exit))'
fi

