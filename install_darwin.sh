#!/bin/bash

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/alpaca_encoder
fi

echo Install alpaca_encoder to $destdir

install -d -m 755 $destdir
install -d -m 755 $destdir/alpaca_encoder.app
install -d -m 755 $destdir/alpaca_encoder.app/Contents
install -d -m 755 $destdir/alpaca_encoder.app/Contents/MacOS
install -d -m 755 $destdir/alpaca_encoder.app/Contents/Resources
install -v -m 644 packages/MacOSX/pkg/alpaca_encoder.app/Contents/Info.plist $destdir/alpaca_encoder.app/Contents/
install -v -m 644 packages/MacOSX/pkg/alpaca_encoder.app/Contents/PkgInfo $destdir/alpaca_encoder.app/Contents/
install -v -m 755 -s src/alpaca_encoder  $destdir/alpaca_encoder.app/Contents/MacOS/alpaca_encoder
install -v -m 644 packages/MacOSX/pkg/alpaca_encoder.app/Contents/Resources/README.rtf $destdir/alpaca_encoder.app/Contents/Resources/
install -v -m 644 packages/MacOSX/pkg/alpaca_encoder.app/Contents/Resources/alpaca_encoder.icns $destdir/alpaca_encoder.app/Contents/Resources/

install -d -m 755 $destdir/doc

install -v -m 644 doc/encoder.html    $destdir/doc/encoder.html
install -v -m 644 doc/encoder1.png    $destdir/doc/encoder1.png
install -v -m 644 doc/encoder2.png    $destdir/doc/encoder2.png
install -v -m 644 doc/encoder3.png    $destdir/doc/encoder3.png
install -v -m 644 doc/encoder4.png    $destdir/doc/encoder4.png

