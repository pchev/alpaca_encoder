#!/bin/bash 

version=$(grep 'encoder_version' src/pu_encoderclient.pas |head -1| cut -d\' -f2)

basedir=/tmp/alpaca_encoder   # Be sure this is set to a non existent directory, it is removed after the run!

builddir=$basedir/alpaca_encoder

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

wd=`pwd`

currentrev=$(git rev-list --count --first-parent HEAD)

# delete old files
  rm alpaca_encoder*.dmg
  rm -rf $basedir

# make x86_64  Mac version
  ./configure $configopt prefix=$builddir target=x86_64-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 clean
  make CPU_TARGET=x86_64
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install CPU_TARGET=x86_64
  if [[ $? -ne 0 ]]; then exit 1;fi
  # pkg
  sed -i.bak "18s/1.0/$version/"  $builddir/alpaca_encoder.app/Contents/Info.plist
  rm $builddir/alpaca_encoder.app/Contents/Info.plist.bak
  cp packages/MacOSX/alpaca_encoder64.pkgproj $basedir
  cp packages/MacOSX/readme.txt $basedir
  cd $basedir
  sed -i.bak "s/alpaca_encoder_version/$version/g" alpaca_encoder64.pkgproj 
  rm alpaca_encoder64.pkgproj.bak
  sed -i.bak "s/alpaca_encoder_version/$version/" readme.txt
  rm readme.txt.bak
  mv alpaca_encoder "AlpacaEncoder"
  packagesbuild -v alpaca_encoder64.pkgproj
  if [[ $? -ne 0 ]]; then exit 1;fi
  cp readme.txt build/
  hdiutil create -anyowners -volname alpaca_encoder-$version-$currentrev-x86_64-macosx -imagekey zlib-level=9 -format UDZO -srcfolder ./build alpaca_encoder-$version-$currentrev-x86_64-macosx.dmg
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv alpaca_encoder*.dmg $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $basedir

