unit UScaleDPI;

{$mode objfpc}{$H+}

interface

uses
  Math, Types, StdCtrls,
  Forms, Graphics, Controls, ComCtrls, Grids, LCLType;

procedure SetScale(cnv: TCanvas);
procedure ScaleDPI(Control: TControl);
function DoScaleX(Size: integer): integer;
function DoScaleY(Size: integer): integer;
function scale: double;

var
  UseScaling: boolean = True;
  DesignDPI: integer = 96;
  RunDPI: integer = 96;

implementation

procedure SetScale(cnv: TCanvas);
var
  rs: TSize;
  sc: double;
const
  teststr = 'The Lazy Fox Jumps';
  designlen = 125;
  designhig = 18;
begin
  {$ifdef SCALE_BY_DPI_ONLY}
  RunDPI := Screen.PixelsPerInch;
  {$else}
  rs := cnv.TextExtent(teststr);
  sc := rs.cx / designlen;
  sc := max(sc, rs.cy / designhig);
  if abs(1 - sc) < 0.02 then
    sc := 1;
  RunDPI := round(DesignDPI * sc);
  {$endif}
end;

function scale: double;
begin
  Result := UScaleDPI.RunDPI / UScaleDPI.DesignDPI;
  if Result < 1 then
    Result := 1;
end;

function DoScaleX(Size: integer): integer;
begin
  if (not UseScaling) or (RunDPI <= DesignDPI) then
    Result := Size
  else
    Result := MulDiv(Size, RunDPI, DesignDPI);
end;

function DoScaleY(Size: integer): integer;
begin
  if (not UseScaling) or (RunDPI <= DesignDPI) then
    Result := Size
  else
    Result := MulDiv(Size, RunDPI, DesignDPI);
end;


procedure ScaleDPI(Control: TControl);
var
  n: integer;
  WinControl: TWinControl;
begin
  if (not UseScaling) or (RunDPI <= DesignDPI) then
    exit;

  if Control is TUpDown then
  begin
      if TUpDown(Control).Associate <> nil then
      begin
        WinControl := TUpDown(Control).Associate;
        TUpDown(Control).Associate := nil;
        TUpDown(Control).Associate := WinControl;
        exit;
      end;
  end;

  with Control do
  begin
    Left := DoScaleX(Left);
    Top := DoScaleY(Top);
    Width := DoScaleX(Width);
    Height := DoScaleY(Height);
    Constraints.MaxHeight := DoScaleX(Constraints.MaxHeight);
    Constraints.MaxWidth := DoScaleX(Constraints.MaxWidth);
    Constraints.MinHeight := DoScaleX(Constraints.MinHeight);
    Constraints.MinWidth := DoScaleX(Constraints.MinWidth);
  end;

  if Control is TToolBar then
  begin
    with TToolBar(Control) do
    begin
      ButtonWidth := DoScaleX(ButtonWidth);
      ButtonHeight := DoScaleY(ButtonHeight);
    end;
    exit;
  end;

  if Control is TStringGrid then
  begin
    with TStringGrid(Control) do
    begin
      for n := 0 to ColCount - 1 do
      begin
        ColWidths[n] := DoScaleX(ColWidths[n]);
      end;
    end;
    exit;
  end;

  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount > 0 then
    begin
      for n := 0 to WinControl.ControlCount - 1 do
      begin
        if WinControl.Controls[n] is TControl then
        begin
          ScaleDPI(WinControl.Controls[n]);
        end;
      end;
    end;
  end;
end;

end.
