unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  cKenneyMalePersonWalk;

type
  TfrmMain = class(TForm)
    cadKenneyMalePersonWalk1: TcadKenneyMalePersonWalk;
    Switch1: TSwitch;
    procedure Switch1Switch(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  TOlfSVGBitmapList.ClearCache;
end;

procedure TfrmMain.Switch1Switch(Sender: TObject);
begin
  cadKenneyMalePersonWalk1.IsStarted := Switch1.IsChecked;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
