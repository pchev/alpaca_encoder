unit cu_alpacaserver;

{$mode objfpc}{$H+}

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
      FErrorMsg: TStringProc;
      FPortMsg: TStringProc;
      procedure ErrorMsg(var msg:string);
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
      property onErrorMsg: TStringProc read FErrorMsg write FErrorMsg;
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
  TCPDaemon.onErrorMsg := @ErrorMsg;
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

procedure T_AlpacaServer.ErrorMsg(var msg:string);
begin
  if assigned(FErrorMsg) then FErrorMsg(msg);
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

