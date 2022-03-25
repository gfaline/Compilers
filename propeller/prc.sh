#!/usr/bin/env bash

LLC="llc"
CC="cc"
PROPC=$(dirname "$0")/propeller.native
UNAME=$(uname -s)

Usage() {
  echo "Usage: $0 [options] <*.pr>"
  echo "-k    Keep intermediate files"
  echo "-h    Print this help"
  exit 1
}

if [ $# -lt 1 ]
then
  Usage $0
fi

keep=0

while getopts kh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage $0
	    ;;
    esac
done

shift `expr $OPTIND - 1`

SRC=$1

if [[ !( $SRC == *.pr ) ]]
then
  echo "Input must be a propeller source code file."
  exit 1
fi

if [ ! -f $SRC ]
then
  echo "$SRC is not a regular file."
  exit 1
fi

BASENAME=${SRC%.pr}
LLVMIRS=$BASENAME.ll
ASM=$BASENAME.s
OUTPUT=$BASENAME.out
[[ $UNAME =~ (CYGWIN|MINGW|MSYS).* ]] && OUTPUT=$BASENAME.exe

INTERMEDIATES="$LLVMIRS $ASM"

$PROPC $SRC > $LLVMIRS || exit 1
$LLC -relocation-model=pic $LLVMIRS > $ASM
$CC -o $OUTPUT $ASM

[ $keep -eq 1 ] || rm $INTERMEDIATES

