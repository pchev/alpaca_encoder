#!/bin/bash

destdir=$1

cpu_target=$2

if [ -z "$destdir" ]; then
   export destdir=/tmp/alpaca_encoder
fi

echo Install alpaca_encoder to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/bin
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/applications
install -m 755 -d $destdir/share/metainfo
install -m 755 -d $destdir/share/doc
install -m 755 -d $destdir/share/doc/alpaca_encoder
install -m 755 -d $destdir/share/pixmaps
install -m 755 -d $destdir/share/icons
install -m 755 -d $destdir/share/icons/hicolor
install -m 755 -d $destdir/share/icons/hicolor/48x48
install -m 755 -d $destdir/share/icons/hicolor/48x48/apps
install -m 755 -d $destdir/share/icons/hicolor/scalable
install -m 755 -d $destdir/share/icons/hicolor/scalable/apps
install -m 755 -d $destdir/share/alpaca_encoder
install -m 755 -d $destdir/share/alpaca_encoder/doc

install -v -m 755 -s src/alpaca_encoder  $destdir/bin/alpaca_encoder
install -v -m 644 doc/encoder.html $destdir/share/alpaca_encoder/doc/encoder.html
install -v -m 644 doc/encoder1.png $destdir/share/alpaca_encoder/doc/encoder1.png
install -v -m 644 doc/encoder2.png $destdir/share/alpaca_encoder/doc/encoder2.png
install -v -m 644 doc/encoder3.png $destdir/share/alpaca_encoder/doc/encoder3.png
install -v -m 644 doc/encoder4.png $destdir/share/alpaca_encoder/doc/encoder4.png
install -v -m 644 packages/Linux/share/applications/alpaca_encoder.desktop $destdir/share/applications/alpaca_encoder.desktop
install -v -m 644 packages/Linux/share/metainfo/alpaca_encoder.appdata.xml $destdir/share/metainfo/alpaca_encoder.appdata.xml
install -v -m 644 packages/Linux/share/doc/alpaca_encoder/changelog $destdir/share/doc/alpaca_encoder/changelog
install -v -m 644 packages/Linux/share/doc/alpaca_encoder/copyright $destdir/share/doc/alpaca_encoder/copyright
install -v -m 644 packages/Linux/share/pixmaps/alpaca_encoder.png $destdir/share/pixmaps/alpaca_encoder.png
install -v -m 644 packages/Linux/share/icons/hicolor/48x48/apps/alpaca_encoder.png $destdir/share/icons/hicolor/48x48/apps/alpaca_encoder.png
