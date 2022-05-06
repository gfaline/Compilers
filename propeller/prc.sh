#!/usr/bin/env bash

LLC="llc"
CC="cc"
PROPC=$(dirname "$0")/propeller.native
UNAME=$(uname -s)

Usage() {
  echo "Usage: $0 [options] <*.pr>"
  echo "-k    Keep intermediate files"
  echo "-r    Specify a runtime to link against (without the .o extension ), defaults to 'default'"
  echo "-h    Print this help"
  exit 1
}

if [ $# -lt 1 ]
then
  Usage $0
fi

keep=0
runtime="default"

while getopts "kr:h" c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
        r) # Runtime
            runtime=$OPTARG
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

CleanUpAndFail() {
  [ $keep -eq 1 ] || rm -f $INTERMEDIATES
  exit 1
}

$PROPC $SRC > $LLVMIRS || CleanUpAndFail
$LLC -relocation-model=pic $LLVMIRS > $ASM || CleanUpAndFail
$CC -o $OUTPUT $ASM "$(dirname "$0")/runtime/${runtime}.rto"|| CleanUpAndFail

[ $keep -eq 1 ] || rm -f $INTERMEDIATES

