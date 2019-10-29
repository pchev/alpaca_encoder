unit cu_alpacaserver;

{$mode objfpc}{$H+}
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

uses cu_alpacadevice, cu_tcpserver, synautil, math,
  contnrs, Classes, SysUtils;

type

  T_AlpacaDeviceElement = class(TObject)
    public
      devtype: TAlpacaDeviceType;
      device: T_AlpacaDevice;
      devicenum: integer;
      devicename: string;
      devicepath: string;
  end;

  T_AlpacaDeviceList = class(TObjectList)
  private
    function GetItem(Index: integer): T_AlpacaDeviceElement;
    procedure SetItem(Index: integer; AValue: T_AlpacaDeviceElement);
  public
    property Items[Index: integer]: T_AlpacaDeviceElement read GetItem write SetItem; default;
  end;


  T_AlpacaServer = class(TComponent)
    protected
      TCPDaemon: TTCPDaemon;
      DeviceList: T_AlpacaDeviceList;
      ServerTransactionID: LongWord;
      FIPAddr, FIPPort : string;
      FShowMsg: TStringProc;
      FShowError: TStringProc;
      FPortMsg: TStringProc;
      procedure ShowError(var msg:string);
      procedure ShowMsg(var msg:string);
      procedure ShowSocket(var msg:string);
      function ProcessGet(HttpRequest: string): string;
      function ProcessPut(HttpRequest,arg: string): string;
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      procedure StartServer;
      procedure StopServer;
      procedure AddDevice(devtype:TAlpacaDeviceType; device:T_AlpacaDevice);
      property IPAddr:string read FIPAddr write FIPAddr;
      property IPPort:string read FIPPort write FIPPort;
      property onShowError: TStringProc read FShowError write FShowError;
      property onShowMsg: TStringProc read FShowMsg write FShowMsg;
      property onPortMsg: TStringProc read FPortMsg write FPortMsg;
  end;

implementation

//////////////////////// T_AlpacaDeviceList ////////////////////////
function T_AlpacaDeviceList.GetItem(Index: integer): T_AlpacaDeviceElement;
begin
  Result := T_AlpacaDeviceElement(inherited Items[Index]);
end;

procedure T_AlpacaDeviceList.SetItem(Index: integer; AValue: T_AlpacaDeviceElement);
begin
  inherited Items[Index] := AValue;
end;

//////////////////////// T_AlpacaServer ////////////////////////

constructor T_AlpacaServer.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FIPAddr := '0.0.0.0';
  FIPPort := '22222';
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator := ',';
  DefaultFormatSettings.DateSeparator := '/';
  DefaultFormatSettings.TimeSeparator := ':';
  TCPDaemon:=TTCPDaemon.Create;
  DeviceList:=T_AlpacaDeviceList.Create;
  TCPDaemon.onShowError := @ShowError;
  TCPDaemon.onShowMsg := @ShowMsg;
  TCPDaemon.onShowSocket := @ShowSocket;
  TCPDaemon.onProcessGet:=@ProcessGet;
  TCPDaemon.onProcessPut:=@ProcessPut;
  ServerTransactionID := 0;
end;

destructor  T_AlpacaServer.Destroy;
begin
  DeviceList.Free;
end;

procedure T_AlpacaServer.AddDevice(devtype:TAlpacaDeviceType; device:T_AlpacaDevice);
var elem: T_AlpacaDeviceElement;
    i,n: integer;
begin
  n:=0;
  for i:=0 to DeviceList.Count-1 do begin
    if DeviceList[i].devtype=devtype then
      n:=max(n,DeviceList[i].devicenum+1);
  end;
  elem:=T_AlpacaDeviceElement.Create;
  elem.device:=device;
  elem.devtype:=devtype;
  elem.devicenum:=n;
  elem.devicename:=AlpacaDeviceName[ord(devtype)];
  elem.devicepath:='/api/'+ApiVersion+'/'+elem.devicename+'/'+IntToStr(elem.devicenum)+'/';
  elem.device.Path:=elem.devicepath;
  DeviceList.Add(elem);
end;

procedure T_AlpacaServer.StartServer;
begin
  TCPDaemon.IPaddr := FIPAddr;
  TCPDaemon.IPport := FIPPort;
  TCPDaemon.Start;
end;

procedure T_AlpacaServer.StopServer;
begin
  TCPDaemon.Terminate;
end;

procedure T_AlpacaServer.ShowError(var msg:string);
begin
   if assigned(FShowError) then FShowError(msg);
end;

procedure T_AlpacaServer.ShowMsg(var msg:string);
begin
  if assigned(FShowMsg) then FShowMsg(msg);
end;

procedure T_AlpacaServer.ShowSocket(var msg:string);
begin
   if assigned(FPortMsg) then FPortMsg(msg);
end;

function T_AlpacaServer.ProcessGet(HttpRequest: string): string;
var req,doc: string;
    i,p,n,httpstatus: integer;
begin
  try
  ShowMsg(HttpRequest);
  req:=HttpRequest;
  n:=-1;
  for i:=0 to DeviceList.Count-1 do begin
    p:=pos(DeviceList[i].devicepath,req);
    if p>0 then begin
      n:=i;
      Delete(req,1,p-1);
      break;
    end;
  end;
  if n>=0 then begin
    inc(ServerTransactionID);
    doc:=DeviceList[n].device.ProcessGetRequest(req,ServerTransactionID,httpstatus) + CRLF;
    ShowMsg(doc);
    if httpstatus=200 then begin
    result:='HTTP/1.0 200' + CRLF
           +'Connection: close' + CRLF
           +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
           +'Content-type: application/json; charset=utf-8' + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ASCOM Alpaca Server - Freepascal-Synapse' + CRLF
           +'' + CRLF
           +doc;
    end
    else begin
      result:='HTTP/1.0 '+inttostr(httpstatus) + CRLF
             +'Connection: close' + CRLF
             +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ASCOM Alpaca Server - Freepascal-Synapse' + CRLF
             +'' + CRLF
             +doc;
    end;
  end
  else begin
    result:='HTTP/1.0 400' + CRLF
           +'Connection: close' + CRLF
           +'Content-type: text/html; charset=utf-8' + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ASCOM Alpaca Server - Freepascal-Synapse' + CRLF
           +'' + CRLF
           +'Not Found' + CRLF;
  end;
  except
    on E: Exception do begin
      result:='HTTP/1.0 500' + CRLF
             +'Connection: close' + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ASCOM Alpaca Server - Freepascal-Synapse' + CRLF
             +'' + CRLF
             +'Unexpected error: '+ E.Message + CRLF;
    end;
  end;
end;

function T_AlpacaServer.ProcessPut(HttpRequest,arg: string): string;
var req,doc: string;
    i,p,n,httpstatus: integer;
begin
  try
  req:=HttpRequest;
  n:=-1;
  for i:=0 to DeviceList.Count-1 do begin
    p:=pos(DeviceList[i].devicepath,req);
    if p>0 then begin
      n:=i;
      Delete(req,1,p-1);
      break;
    end;
  end;
  if n>=0 then begin
    inc(ServerTransactionID);
    doc:=DeviceList[n].device.ProcessPutRequest(req,arg,ServerTransactionID,httpstatus) + CRLF;
    if httpstatus=200 then begin
    result:='HTTP/1.0 200' + CRLF
           +'Connection: close' + CRLF
           +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
           +'Content-type: application/json; charset=utf-8' + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ASCOM Alpaca Server - Freepascal-Synapse' + CRLF
           +'' + CRLF
           +doc;
    end
    else begin
      result:='HTTP/1.0 '+inttostr(httpstatus) + CRLF
             +'Connection: close' + CRLF
             +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ASCOM Alpaca Server - Freepascal-Synapse' + CRLF
             +'' + CRLF
             +doc;
    end;
  end
  else begin
    result:='HTTP/1.0 400' + CRLF
           +'Connection: close' + CRLF
           +'Content-type: text/html; charset=utf-8' + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ASCOM Alpaca Server - Freepascal-Synapse' + CRLF
           +'' + CRLF
           +'Not Found' + CRLF;
  end;
  except
    on E: Exception do begin
      result:='HTTP/1.0 500' + CRLF
             +'Connection: close' + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ASCOM Alpaca Server - Freepascal-Synapse' + CRLF
             +'' + CRLF
             +'Unexpected error: '+ E.Message + CRLF;
    end;
  end;
end;

end.

