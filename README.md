# Alpaca Encoder

ASCOM Alpaca driver for telescope digital circle encoder.

This driver can be use with encoder controller using the Tangent protocol like the Ouranos , AAM or NGC-MAX box.<br/>
It use a floating two stars alignment method based on the article by [Toshimi Taki](http://www.takitoshimi.shop/) in [February 1989 S&T](https://archive.org/details/Sky_and_Telescope_1989-02-pdf/page/n85).

The [Alpaca server](https://ascom-standards.org/Developer/Alpaca.htm) is embedded in this driver, allowing to connect any client application such planetarium using the ASCOM Remote Client on Windows, or by using the internal Alpaca client feature of the application like [HNSKY](https://www.hnsky.org/software.htm) or [Cartes du Ciel](https://www.ap-i.net/skychart/start) on Windows, Linux or macOS.<br/>
The Alpaca Encoder driver itself can run on Windows, Linux (intel or arm) and macOS.<br/> 
Because of the networking capability of the Alpaca protocol you can use different operating system for the driver and client. For example run the driver on Linux in a Raspberry Pi and connect from an application on Windows.

## Compilation

The code can be compiled with FreePascal/Lazarus https://www.lazarus-ide.org/ 
