unit pu_server;

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
{
 The reason for this unit is to avoid circular reference in pu_encoderclient
 that is a the same time the main form and the implementation of the device.
}
interface

uses cu_alpacaserver, cu_alpacadevice, cu_alpacaencoder, pu_encoderclient,
  LazFileUtils, IniFiles,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { Tf_server }

  Tf_server = class(TForm)
    PanelTop: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    AlpacaIPAddr, AlpacaIPPort : string;
    ConfigDir, ConfigFile: string;
    AlpacaServer : T_AlpacaServer;
    Encoder: T_AlpacaEncoder;
    procedure ShowError(var msg:string);
    procedure ShowMsg(var msg:string);
    procedure ShowSocket(var msg:string);
    procedure GetAppDir;
    procedure ReadConfig;
  public
    procedure Stop;
  end;

var
  f_server: Tf_server;

implementation

{$R *.lfm}

procedure Tf_server.FormCreate(Sender: TObject);
begin
  GetAppDir;
  ReadConfig;
  pop_encoder.IPAddr.Text:=AlpacaIPAddr;
  pop_encoder.IPPort.Text:=AlpacaIPPort;
  AlpacaServer:=T_AlpacaServer.Create(self);
  AlpacaServer.onShowError:=@ShowError;
  AlpacaServer.onShowMsg:=@ShowMsg;
  AlpacaServer.onPortMsg:=@ShowSocket;
  Encoder:=T_AlpacaEncoder.Create(nil);
  AlpacaServer.AddDevice(telescope,Encoder);
  AlpacaServer.IPAddr:=AlpacaIPAddr;
  AlpacaServer.IPPort:=AlpacaIPPort;
  AlpacaServer.StartServer;
end;

procedure Tf_server.FormDestroy(Sender: TObject);
begin
  Stop;
end;

procedure Wait(wt:single=5);
var endt: TDateTime;
begin
  endt:=now+wt/SecsPerDay;
  while now<endt do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
  end;
end;

procedure Tf_server.Stop;
begin
  Encoder.SetConnected(false);
  wait(1);
  AlpacaServer.StopServer;
  wait(1);
end;


function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(result),1)<>PathDelim then result:=result+PathDelim;
end;

procedure Tf_server.GetAppDir;
begin
  ConfigDir:=GetAppConfigDirUTF8(false,true);
  ConfigFile:=slash(ConfigDir)+'alpaca.ini';
end;

procedure Tf_server.ReadConfig;
var
  ini: tinifile;
begin
  ini := tinifile.Create(Configfile);
  AlpacaIPAddr := ini.ReadString('alpaca', 'ipaddr', '0.0.0.0');
  AlpacaIPPort := ini.ReadString('alpaca', 'ipport', '11122');
  ini.Free;
end;

procedure Tf_server.ShowError(var msg:string);
begin
  pop_encoder.MemoAlpaca.Lines.Add('Error: '+msg);
  pop_encoder.StatusBar1.SimpleText:=msg;
end;

procedure Tf_server.ShowMsg(var msg:string);
begin
  if pop_encoder.ShowTrace.Checked then pop_encoder.MemoAlpaca.Lines.Add(msg);
end;

procedure Tf_server.ShowSocket(var msg:string);
var buf: string;
begin
  pop_encoder.LabelPort.Caption:='Server running on port '+msg;
  if msg<>pop_encoder.IPPort.Text then begin
     buf:='Alpaca configured on port '+pop_encoder.IPPort.Text+' but running on '+msg;
     ShowError(buf);
  end;
end;


end.

