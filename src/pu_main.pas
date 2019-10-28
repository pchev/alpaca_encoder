unit pu_main;

{$mode objfpc}{$H+}

interface

uses cu_alpacaserver, cu_alpacadevice, cu_alpacaencoder, IniFiles,
  LazFileUtils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_main }

  Tf_main = class(TForm)
    Button2: TButton;
    IPAddr: TEdit;
    IPPort: TEdit;
    LabelPort: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IPAddrChange(Sender: TObject);
    procedure IPPortChange(Sender: TObject);
  private
    AlpacaIPAddr, AlpacaIPPort : string;
    ConfigDir, ConfigFile: string;
    AlpacaServer : T_AlpacaServer;
    Encoder: T_AlpacaEncoder;
    procedure ErrorMsg(var msg:string);
    procedure ShowSocket(var msg:string);
    procedure GetAppDir;
    procedure ReadConfig;
    procedure SaveConfig;
  public

  end;

var
  f_main: Tf_main;

implementation

{$R *.lfm}

procedure Tf_main.FormCreate(Sender: TObject);
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator := ',';
  DefaultFormatSettings.DateSeparator := '/';
  DefaultFormatSettings.TimeSeparator := ':';
  GetAppDir;
  ReadConfig;
  IPAddr.Text:=AlpacaIPAddr;
  IPPort.Text:=AlpacaIPPort;
  AlpacaServer:=T_AlpacaServer.Create(self);
  AlpacaServer.onErrorMsg:=@ErrorMsg;
  AlpacaServer.onPortMsg:=@ShowSocket;
  Encoder:=T_AlpacaEncoder.Create(self);
  AlpacaServer.AddDevice(telescope,Encoder);
  AlpacaServer.IPAddr:=AlpacaIPAddr;
  AlpacaServer.IPPort:=AlpacaIPPort;
  AlpacaServer.StartServer;
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

procedure Tf_main.IPAddrChange(Sender: TObject);
begin
   AlpacaIPAddr:=IPAddr.Text;
end;

procedure Tf_main.IPPortChange(Sender: TObject);
begin
  AlpacaIPPort:=IPPort.Text;
end;

procedure Tf_main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
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

procedure Tf_main.GetAppDir;
begin
  ConfigDir:=GetAppConfigDirUTF8(false,true);
  ConfigFile:=slash(ConfigDir)+'alpaca.ini';
end;

procedure Tf_main.ReadConfig;
var
  ini: tinifile;
begin
  ini := tinifile.Create(Configfile);
  AlpacaIPAddr := ini.ReadString('alpaca', 'ipaddr', '0.0.0.0');
  AlpacaIPPort := ini.ReadString('alpaca', 'ipport', '22222');
  ini.Free;
end;

procedure Tf_main.SaveConfig;
var
  ini: tinifile;
begin
  ini := tinifile.Create(Configfile);
  ini.WriteString('alpaca', 'ipaddr', AlpacaIPAddr);
  ini.WriteString('alpaca', 'ipport', AlpacaIPPort);
  ini.UpdateFile;
  ini.Free;
end;

procedure Tf_main.ErrorMsg(var msg:string);
begin
  memo1.Lines.Add(msg);
end;

procedure Tf_main.ShowSocket(var msg:string);
begin
  if msg<>AlpacaIPPort then LabelPort.Caption:=msg;
end;

procedure Tf_main.Button1Click(Sender: TObject);
begin

end;

procedure Tf_main.Button2Click(Sender: TObject);
begin
  encoder.SetupDialog();
end;



end.

