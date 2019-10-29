Summary: ASCOM Alpaca driver for telescope encoder
Name: alpaca_encoder
Version: 0
Release: 1
Group: Sciences/Astronomy
License: GPLv2+
URL: https://github.com/pchev/alpaca_encoder
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: i386
Provides: alpaca_encoder
Requires: gtk2 glib2 pango libjpeg libpng
AutoReqProv: no

%description
ASCOM Alpaca driver for telescope encoder working with Tangent,Ouranos, NGC MAX,MicroGuider,..
and all system using the same protocol. 

%files
%defattr(-,root,root)
/usr/bin/alpaca_encoder
/usr/share/alpaca_encoder
/usr/share/metainfo/alpaca_encoder.appdata.xml
/usr/share/applications/alpaca_encoder.desktop
/usr/share/pixmaps/alpaca_encoder.png
/usr/share/icons/hicolor/48x48/apps/alpaca_encoder.png
/usr/share/doc/alpaca_encoder

