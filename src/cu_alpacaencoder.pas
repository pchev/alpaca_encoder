unit cu_alpacaencoder;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{
Copyright (C) 2019 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

interface

uses  cu_alpacatelescope, cu_alpacadevice, pu_encoderclient,
  Forms, Classes, SysUtils;

type

  T_AlpacaEncoder = class(T_AlpacaTelescope)
    protected
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      function  GetGuid: string; override;
      function  GetSetupPage: string; override;
      function  Action( actionName, actionParameters: string):string; override;
      procedure CommandBlind( command: string;  raw: boolean = false); override;
      function  CommandBool(command: string;  raw: boolean = false):boolean; override;
      function  CommandString(command: string;  raw: boolean = false):string; override;
      function  Connected:boolean; override;
      procedure SetConnected(value:boolean); override;
      function  Description:string; override;
      function  DriverInfo:string; override;
      function  DriverVersion:string; override;
      function  InterfaceVersion: integer; override;
      function  Name:string; override;
      function  SupportedActions:TStringList; override;
      function  alignmentmode: integer; override;
      function  altitude: double; override;
      function  aperturearea: double; override;
      function  aperturediameter: double; override;
      function  athome: boolean; override;
      function  atpark: boolean; override;
      function  azimuth: double; override;
      function  canfindhome: boolean; override;
      function  canpark: boolean; override;
      function  canunpark: boolean; override;
      function  canpulseguide: boolean; override;
      function  cansetdeclinationrate: boolean; override;
      function  cansetguiderates: boolean; override;
      function  cansetpark: boolean; override;
      function  cansetpierside: boolean; override;
      function  cansetrightascensionrate: boolean; override;
      function  cansettracking: boolean; override;
      function  canslew: boolean; override;
      function  canslewaltaz: boolean; override;
      function  canslewaltazasync: boolean; override;
      function  canslewasync: boolean; override;
      function  cansync: boolean; override;
      function  cansyncaltaz: boolean; override;
      function  declination: double; override;
      function  declinationrate: double; override;
      procedure setdeclinationrate(value: double); override;
      function  doesrefraction: boolean; override;
      procedure setdoesrefraction(value: boolean); override;
      function  equatorialsystem: integer; override;
      function  focallength: double; override;
      function  guideratedeclination: double; override;
      procedure setguideratedeclination(value: double); override;
      function  guideraterightascension: double; override;
      procedure setguideraterightascension(value: double); override;
      function  ispulseguiding: boolean; override;
      function  rightascension: double; override;
      function  rightascensionrate: double; override;
      procedure setrightascensionrate(value: double); override;
      function  sideofpier: integer; override;
      procedure setsideofpier(value: integer); override;
      function  siderealtime: double; override;
      function  siteelevation: double; override;
      procedure setsiteelevation(value: double); override;
      function  sitelatitude: double; override;
      procedure setsitelatitude(value: double); override;
      function  sitelongitude: double; override;
      procedure setsitelongitude(value: double); override;
      function  slewing: boolean; override;
      function  slewsettletime: integer; override;
      procedure setslewsettletime(value: integer); override;
      function  targetdeclination: double; override;
      procedure settargetdeclination(value: double); override;
      function  targetrightascension: double; override;
      procedure settargetrightascension(value: double); override;
      function  tracking: boolean; override;
      procedure settracking(value: boolean); override;
      function  trackingrate: integer; override;
      procedure settrackingrate(value: integer); override;
      function  trackingrates: TTrackingRates; override;
      function  utcdate: string; override;
      procedure setutcdate(value: string); override;
      procedure abortslew; override;
      function  axisrates(axis:integer): TAxisRates; override;
      function  canmoveaxis(axis:integer): boolean; override;
      function  destinationsideofpier(ra,dec: double):integer; override;
      procedure findhome; override;
      procedure moveaxis(axis:integer;rate:double); override;
      procedure park; override;
      procedure pulseguide(direction,duration: integer); override;
      procedure setpark; override;
      procedure slewtoaltaz(az,alt: double); override;
      procedure slewtoaltazasync(az,alt: double); override;
      procedure slewtocoordinates(ra,dec: double); override;
      procedure slewtocoordinatesasync(ra,dec: double); override;
      procedure slewtotarget; override;
      procedure slewtotargetasync; override;
      procedure synctoaltaz(az,alt: double); override;
      procedure synctocoordinates(ra,dec: double); override;
      procedure synctotarget; override;
      procedure unpark; override;
  end;

implementation

const guid='beb20602-8709-48c9-a902-c3870b8f5e6c';

constructor T_AlpacaEncoder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor  T_AlpacaEncoder.Destroy;
begin
  inherited Destroy;
end;

function  T_AlpacaEncoder.GetGuid: string;
begin
  result:=guid;
end;

function  T_AlpacaEncoder.Action( actionName, actionParameters: string):string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

procedure T_AlpacaEncoder.CommandBlind( command: string;  raw: boolean = false);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.CommandBool(command: string;  raw: boolean = false):boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_AlpacaEncoder.CommandString(command: string;  raw: boolean = false):string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

function  T_AlpacaEncoder.Connected:boolean;
begin
  result:=FConnected;
end;

procedure  T_AlpacaEncoder.SetConnected(value:boolean);
var ok: boolean;
    txt:string;
begin
  if value then begin
    if pop_encoder.ScopeConnected then begin
      // try to keep previous alignment if any
      FConnected:=true;
    end
    else begin
      txt:='';
      TargetRA:=NullCoord;
      TargetDEC:=NullCoord;
      pop_encoder.ScopeConnect(ok);
      FConnected:=ok;
      if ok then begin
        if pop_encoder.CheckBoxUnattended.Checked then begin
          txt:=pop_encoder.doInit90;
          if txt<>'' then begin
            FErrorNumber:=ERR_DRIVER_ERROR;
            FErrorMessage:=txt;
          end;
        end;
      end
      else begin
        FErrorNumber:=ERR_DRIVER_ERROR;
        FErrorMessage:='Connection error: '+pop_encoder.statusbar1.SimpleText;
      end;
    end;
  end
  else begin
    if pop_encoder.CheckBoxUnattended.Checked then begin
      pop_encoder.ScopeDisconnect(ok);
      if not ok then begin
        FErrorNumber:=ERR_DRIVER_ERROR;
        FErrorMessage:='Disconnect error';
      end;
    end
    else begin
      // if not unattended do not disconnect the device to not lost the alignment
      FConnected:=false;
    end;
  end;
end;

function  T_AlpacaEncoder.Description:string;
begin
  result:='Encoder Alpaca driver, based on Cartes du Ciel driver.';
end;

function  T_AlpacaEncoder.DriverInfo:string;
begin
  result:=pop_encoder.ScopeGetStatus;
end;

function  T_AlpacaEncoder.DriverVersion:string;
begin
  result:=pu_encoderclient.encoder_version;
end;

function  T_AlpacaEncoder.InterfaceVersion: integer;
begin
  result:=3;
end;

function  T_AlpacaEncoder.Name:string;
begin
  result:='Encoder '+pop_encoder.DeviceName;
end;

function  T_AlpacaEncoder.SupportedActions:TStringList;
begin
  result:=TStringList.Create;
  result.Clear;
end;

function  T_AlpacaEncoder.alignmentmode: integer;
begin
  result:=pop_encoder.AlignmentMode;
end;

function  T_AlpacaEncoder.altitude: double;
var alt, az: double;
    ok: boolean;
begin
  pop_encoder.ScopeGetAltAz(alt,az,ok);
  Result:=alt;
end;

function  T_AlpacaEncoder.aperturearea: double;
begin
 FErrorNumber:=ERR_NOT_IMPLEMENTED;
 FErrorMessage:=MSG_NOT_IMPLEMENTED;
 result:=0;
end;

function  T_AlpacaEncoder.aperturediameter: double;
begin
 FErrorNumber:=ERR_NOT_IMPLEMENTED;
 FErrorMessage:=MSG_NOT_IMPLEMENTED;
 result:=0;
end;

function  T_AlpacaEncoder.athome: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.atpark: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.azimuth: double;
var alt, az: double;
    ok: boolean;
begin
  pop_encoder.ScopeGetAltAz(alt,az,ok);
  Result:=az;
end;

function  T_AlpacaEncoder.canfindhome: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.canpark: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.canunpark: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.canpulseguide: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.cansetdeclinationrate: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.cansetguiderates: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.cansetpark: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.cansetpierside: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.cansetrightascensionrate: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.cansettracking: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.canslew: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.canslewaltaz: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.canslewaltazasync: boolean;
begin
  result:=false;
end;

function T_AlpacaEncoder.canslewasync: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.cansync: boolean;
begin
  result:=true;
end;

function  T_AlpacaEncoder.cansyncaltaz: boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.declination: double;
var ra, de: double;
    ok: boolean;
begin
  pop_encoder.ScopeGetRaDec(ra,de,ok);
  Result:=de;
end;

function  T_AlpacaEncoder.declinationrate: double;
begin
  result:=0;
end;

procedure T_AlpacaEncoder.setdeclinationrate(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.doesrefraction: boolean;
begin
  result:=true;
end;

procedure T_AlpacaEncoder.setdoesrefraction(value: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.equatorialsystem: integer;
begin
  result:=pop_encoder.ScopeGetEqSys;
end;

function  T_AlpacaEncoder.focallength: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_AlpacaEncoder.guideratedeclination: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_AlpacaEncoder.setguideratedeclination(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.guideraterightascension: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_AlpacaEncoder.setguideraterightascension(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.ispulseguiding: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=False;
end;

function  T_AlpacaEncoder.rightascension: double;
var ra, de: double;
    ok: boolean;
begin
  pop_encoder.ScopeGetRaDec(ra,de,ok);
  Result:=ra;
end;

function  T_AlpacaEncoder.rightascensionrate: double;
begin
  result:=0;
end;

procedure T_AlpacaEncoder.setrightascensionrate(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.sideofpier: integer;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_AlpacaEncoder.setsideofpier(value: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.siderealtime: double;
begin
  result:=rad2deg*pop_encoder.ScopeGetSideralTime/15;
end;

function  T_AlpacaEncoder.siteelevation: double;
begin
  result:=pop_encoder.SiteAltitude;
end;

procedure T_AlpacaEncoder.setsiteelevation(value: double);
begin
  if (value>=-300)and(value<=10000) then
     pop_encoder.SiteAltitude:=value
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' siteelevation='+ FormatFloat('0.000',value);
  end;
end;

function  T_AlpacaEncoder.sitelatitude: double;
begin
  result:=pop_encoder.SiteLatitude;
end;

procedure T_AlpacaEncoder.setsitelatitude(value: double);
begin
  if (value>=-90)and(value<=90) then
     pop_encoder.SiteLatitude:=value
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' sitelatitude='+ FormatFloat('0.000',value);
  end;
end;

function  T_AlpacaEncoder.sitelongitude: double;
begin
  result:=-pop_encoder.SiteLongitude;
end;

procedure T_AlpacaEncoder.setsitelongitude(value: double);
begin
  if (value>=-180)and(value<=180) then
     pop_encoder.SiteLongitude:=-value
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' sitelongitude='+ FormatFloat('0.000',value);
  end;
end;

function  T_AlpacaEncoder.slewing: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_AlpacaEncoder.slewsettletime: integer;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_AlpacaEncoder.setslewsettletime(value: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.targetdeclination: double;
begin
  if TargetDEC>NullCoord then
    result:=TargetDEC
  else begin
    result:=0;
    FErrorNumber:=ERR_VALUE_NOT_SET;
    FErrorMessage:=MSG_VALUE_NOT_SET;
  end;
end;

procedure T_AlpacaEncoder.settargetdeclination(value: double);
begin
  if (value>=-90)and(value<=90) then
     TargetDEC:=value
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' dec='+ FormatFloat('0.000',value);
  end;
end;

function  T_AlpacaEncoder.targetrightascension: double;
begin
  if TargetDEC>NullCoord then
    result:=TargetRA
  else begin
    result:=0;
    FErrorNumber:=ERR_VALUE_NOT_SET;
    FErrorMessage:=MSG_VALUE_NOT_SET;
  end;
end;

procedure T_AlpacaEncoder.settargetrightascension(value: double);
begin
  if (value>=0)and(value<=24) then
     TargetRA:=value
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' ra='+ FormatFloat('0.000',value);
  end;
end;

function  T_AlpacaEncoder.tracking: boolean;
begin
  result:=true;
end;

procedure T_AlpacaEncoder.settracking(value: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.trackingrate: integer;
begin
  result:=0;
end;

procedure T_AlpacaEncoder.settrackingrate(value: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.trackingrates: TTrackingRates;
begin
  SetLength(Result,1);
  result[0]:=0;
end;

function  T_AlpacaEncoder.utcdate: string;
begin
  result:=pop_encoder.UTCDate;
end;

procedure T_AlpacaEncoder.setutcdate(value: string);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.abortslew;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_AlpacaEncoder.axisrates(axis:integer): TAxisRates;
begin
  setlength(result,0);
end;

function  T_AlpacaEncoder.canmoveaxis(axis:integer): boolean;
begin
  result:=false;
end;

function  T_AlpacaEncoder.destinationsideofpier(ra,dec: double):integer;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_AlpacaEncoder.findhome;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.moveaxis(axis:integer;rate:double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.park;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.pulseguide(direction,duration: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.setpark;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.slewtoaltaz(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.slewtoaltazasync(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.slewtocoordinates(ra,dec: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.slewtocoordinatesasync(ra,dec: double);
begin
   FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.slewtotarget;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.slewtotargetasync;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.synctoaltaz(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_AlpacaEncoder.synctocoordinates(ra,dec: double);
var txt: string;
begin
  if (dec>=-90)and(dec<=90)and(ra>=0)and(ra<=24)  then begin
    TargetRA:=ra;
    TargetDEC:=dec;
    txt:=pop_encoder.ScopeSync(ra,dec);
    if txt<>'' then begin
      FErrorNumber:=ERR_DRIVER_ERROR;
      FErrorMessage:=txt;
    end;
  end
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' ra='+FormatFloat('0.000',ra)+' dec='+FormatFloat('0.000',ra);
  end;
end;

procedure T_AlpacaEncoder.synctotarget;
var txt: string;
begin
  if (TargetRA<>NullCoord)and(TargetDEC<>NullCoord) then begin
    txt:=pop_encoder.ScopeSync(TargetRA,TargetDEC);
    if txt<>'' then begin
      FErrorNumber:=ERR_DRIVER_ERROR;
      FErrorMessage:=txt;
    end;
  end
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' TargetRightAscension or TargetDeclination not set.';
  end;
end;

procedure T_AlpacaEncoder.unpark;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function   T_AlpacaEncoder.GetSetupPage: string;
begin
  result:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'+
       '<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8">'+
       '<title>Alpaca Encoder driver</title></head><body text>'+
       '<H1>Alpaca Encoder driver Setup</H1><br/>'+
       'To setup this Alpaca driver you must use the GUI of the main program.<br/><br/>'+
       'See detail with the Help button.<br/>'+
       '</body></html>';
end;

end.

