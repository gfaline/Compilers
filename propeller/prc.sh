#!/bin/bash

LLC="llc"
CC="cc"
PROPC="./propeller.native"
UNAME=$(uname -s)

Usage() {
  echo "Usage: $0 [options] <*.pr>"
  exit 1
}

if [ $# -lt 1 ]
then
  Usage $0
fi

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

$PROPC $SRC > $LLVMIRS
$LLC -relocation-model=pic $LLVMIRS > $ASM
$CC -o $OUTPUT $ASM

rm $INTERMEDIATES

