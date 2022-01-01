program alpaca_encoder;

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pu_server, synapse, sysutils,
  cu_alpacaencoder, cu_encoderprotocol, cu_taki, pu_encoderclient;

{$R *.res}

begin
(*  {$ifdef USEHEAPTRC}
    {$ifdef mswindows}
      DeleteFile('C:\Temp\alpaca_encoder_heap.trc');
      SetHeapTraceOutput('C:\Temp\alpaca_encoder_heap.trc');
    {$else}
      DeleteFile('/tmp/alpaca_encoder_heap.trc');
      SetHeapTraceOutput('/tmp/alpaca_encoder_heap.trc');
    {$endif}
  {$endif}   *)

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tpop_encoder, pop_encoder);
  Application.CreateForm(Tf_server, f_server);
  Application.Run;
end.

