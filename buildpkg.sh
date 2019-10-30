#!/bin/bash 

version=$(grep 'encoder_version' src/pu_encoderclient.pas |head -1| cut -d\' -f2)

builddir=/tmp/alpaca_encoder  # Be sure this is set to a non existent directory, it is removed after the run!
innosetup="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="Z:\tmp\alpaca_encoder" # Change to match builddir, Z: is defined in ~/.wine/dosdevices

arch=$(arch)

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

unset extratarget

unset make_linux32
unset make_linux64
unset make_win_dual

if [[ $arch == i686 ]]; then 
   make_linux32=1
fi
if [[ $arch == x86_64 ]]; then 
   make_linux64=1
   make_win_dual=1
   extratarget=",x86_64-linux"
fi

# For win32 and win64 target you must also install the corresponding mingw-w64 to build the C library
#mingw32=/opt/mingw-w32/bin/
#mingw64=/opt/mingw-w64/bin/

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
  rm alpaca_encoder*.xz
  rm alpaca-encoder*.deb
  rm alpaca_encoder*.rpm
  rm alpaca_encoder*.zip
  rm alpaca_encoder*.exe
  rm -rf $builddir

# make Linux i386 version
if [[ $make_linux32 ]]; then
  ./configure $configopt prefix=$builddir target=i386-linux$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=i386 OS_TARGET=linux clean
  make CPU_TARGET=i386 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvJf alpaca_encoder-$version-$currentrev-linux_i386.tar.xz alpaca_encoder
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv alpaca_encoder*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn packages/Linux/debian $builddir
  cd $builddir
  mkdir debian/alpaca-encoder/usr/
  mv bin debian/alpaca-encoder/usr/
  mv share debian/alpaca-encoder/usr/
  cd debian
  sz=$(du -s alpaca-encoder/usr | cut -f1)
  sed -i "s/%size%/$sz/" alpaca-encoder/DEBIAN/control
  sed -i "/Version:/ s/0/$version-$currentrev/" alpaca-encoder/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build alpaca-encoder .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv alpaca-encoder*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # rpm
  cd $wd
  rsync -a --exclude=.svn packages/Linux/rpm $builddir
  cd $builddir
  mkdir -p rpm/RPMS/x86_64
  mkdir -p rpm/RPMS/i386
  mkdir rpm/SRPMS
  mkdir rpm/tmp
  mkdir -p rpm/alpaca_encoder/usr/
  mv debian/alpaca-encoder/usr/* rpm/alpaca_encoder/usr/
  cd rpm
  sed -i "/Version:/ s/0/$version/"  SPECS/alpaca_encoder.spec
  sed -i "/Release:/ s/1/$currentrev/" SPECS/alpaca_encoder.spec
  setarch i386 fakeroot rpmbuild  --buildroot "$builddir/rpm/alpaca_encoder" --define "_topdir $builddir/rpm/" --define "_binary_payload w7.xzdio" -bb SPECS/alpaca_encoder.spec
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv RPMS/i386/alpaca_encoder*.rpm $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir
fi

# make Linux x86_64 version
if [[ $make_linux64 ]]; then
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 OS_TARGET=linux clean
  make CPU_TARGET=x86_64 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvJf alpaca_encoder-$version-$currentrev-linux_x86_64.tar.xz alpaca_encoder
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv alpaca_encoder*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn packages/Linux/debian $builddir
  cd $builddir
  mkdir debian/alpaca-encoder64/usr/
  mv bin debian/alpaca-encoder64/usr/
  mv share debian/alpaca-encoder64/usr/
  cd debian
  sz=$(du -s alpaca-encoder64/usr | cut -f1)
  sed -i "s/%size%/$sz/" alpaca-encoder64/DEBIAN/control
  sed -i "/Version:/ s/0/$version-$currentrev/" alpaca-encoder64/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build alpaca-encoder64 .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv alpaca-encoder*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # rpm
  cd $wd
  rsync -a --exclude=.svn packages/Linux/rpm $builddir
  cd $builddir
  mkdir -p rpm/RPMS/x86_64
  mkdir -p rpm/RPMS/i386
  mkdir rpm/SRPMS
  mkdir rpm/tmp
  mkdir -p rpm/alpaca_encoder/usr/
  mv debian/alpaca-encoder64/usr/* rpm/alpaca_encoder/usr/
  cd rpm
  sed -i "/Version:/ s/0/$version/"  SPECS/alpaca_encoder64.spec
  sed -i "/Release:/ s/1/$currentrev/" SPECS/alpaca_encoder64.spec
# rpm 4.7
  fakeroot rpmbuild  --buildroot "$builddir/rpm/alpaca_encoder" --define "_topdir $builddir/rpm/" --define "_binary_payload w7.xzdio"  -bb SPECS/alpaca_encoder64.spec
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv RPMS/x86_64/alpaca_encoder*.rpm $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir
fi

# make Windows dual x86_64 and i386 version
if [[ $make_win_dual ]]; then
  rsync -a --exclude=.svn packages/Windows/installer/alpaca_encoder/* $builddir
  mkdir $builddir/Data
  mkdir $builddir/Prog
  # i386
  export PATH=$mingw32:$save_PATH
  ./configure $configopt prefix=$builddir/Data target=i386-win32$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make OS_TARGET=win32 CPU_TARGET=i386 clean
  make OS_TARGET=win32 CPU_TARGET=i386
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win_dual32
  if [[ $? -ne 0 ]]; then exit 1;fi
  # x86_64
  export PATH=$mingw64:$save_PATH
  ./configure $configopt prefix=$builddir/Data target=x86_64-win64$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make OS_TARGET=win64 CPU_TARGET=x86_64 clean
  make OS_TARGET=win64 CPU_TARGET=x86_64
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win_dual64
  if [[ $? -ne 0 ]]; then exit 1;fi
  # exe
  cd $builddir
  sed -i "/AppVerName/ s/V1/V$version/" alpaca_encoder_dual.iss
  sed -i "/OutputBaseFilename/ s/windows/$version-$currentrev-windows/" alpaca_encoder_dual.iss
  sed -i "s/ccdciel_version/$version/" Presetup/readme.txt
  wine "$innosetup" "$wine_build\alpaca_encoder_dual.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/alpaca_encoder*.exe $wd

  cd $wd
  rm -rf $builddir
fi

cd $wd
rm -rf $builddir

