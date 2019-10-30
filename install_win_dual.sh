#!/bin/bash

OS_TARGET=$1
destdir=$2

if [ -z "$OS_TARGET=" ]; then
   export OS_TARGET==win32
fi

if [ -z "$destdir" ]; then
   export destdir=/tmp/alpaca_encoder/Data
fi

echo Install alpaca_encoder $OS_TARGET to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/doc


if [ $OS_TARGET = win32 ]; then 
  strip -v -o $destdir/../Prog/alpaca_encoder.exe src/alpaca_encoder.exe 
fi
if [ $OS_TARGET = win64 ]; then
  strip -v -o $destdir/../Prog/alpaca_encoder-x64.exe src/alpaca_encoder.exe 
fi

install -v -m 644 doc/encoder.html    $destdir/doc/encoder.html
install -v -m 644 doc/encoder1.png    $destdir/doc/encoder1.png
install -v -m 644 doc/encoder2.png    $destdir/doc/encoder2.png
install -v -m 644 doc/encoder3.png    $destdir/doc/encoder3.png
install -v -m 644 doc/encoder4.png    $destdir/doc/encoder4.png

