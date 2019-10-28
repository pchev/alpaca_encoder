program alpaca_encoder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pu_main, cu_tcpserver, synapse, cu_alpacaserver, cu_alpacadevice, sysutils,
  cu_alpacatelescope, cu_alpacaencoder, cu_encoderprotocol, cu_serial, cu_taki, pu_encoderclient;

{$R *.res}

begin
  {$ifdef USEHEAPTRC}
    {$ifdef mswindows}
      DeleteFile('C:\Temp\alpaca_encoder_heap.trc');
      SetHeapTraceOutput('C:\Temp\alpaca_encoder_heap.trc');
    {$else}
      DeleteFile('/tmp/alpaca_encoder_heap.trc');
      SetHeapTraceOutput('/tmp/alpaca_encoder_heap.trc');
    {$endif}
  {$endif}

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tf_main, f_main);
  Application.CreateForm(Tpop_encoder, pop_encoder);
  Application.Run;
end.

