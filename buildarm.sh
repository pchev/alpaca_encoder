#!/bin/bash 

version=$(grep 'encoder_version' src/pu_encoderclient.pas |head -1| cut -d\' -f2)

builddir=/tmp/alpaca_encoder  # Be sure this is set to a non existent directory, it is removed after the run!

arch=$(arch)

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

unset extratarget
if [[ $arch == aarch64 ]]; then
  extratarget=",aarch64-linux"
fi

make_linuxarm=1

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

save_PATH=$PATH
wd=`pwd`

currentrev=$(git rev-list --count --first-parent HEAD)

echo $version - $currentrev


# delete old files
  rm alpaca_encoder-*_armhf.tar.bz2
  rm alpaca-encoder_*_armhf.deb
  rm -rf $builddir

# make Linux arm version
if [[ $make_linuxarm ]]; then
  ./configure $configopt prefix=$builddir target=arm-linux$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=arm OS_TARGET=linux clean
  make CPU_TARGET=arm OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install CPU_TARGET=arm
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvjf alpaca_encoder-$version-$currentrev-linux_arm.tar.bz2 alpaca_encoder
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv alpaca_encoder*.tar.bz2 $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn packages/Linux/debian $builddir
  cd $builddir
  mkdir debian/alpaca-encoderarm/usr/
  mv bin debian/alpaca-encoderarm/usr/
  mv share debian/alpaca-encoderarm/usr/
  cd debian
  sz=$(du -s alpaca-encoderarm/usr | cut -f1)
  sed -i "s/%size%/$sz/" alpaca-encoderarm/DEBIAN/control
  sed -i "/Version:/ s/0/$version-$currentrev/" alpaca-encoderarm/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build alpaca-encoderarm .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv alpaca-encoder*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir
fi


