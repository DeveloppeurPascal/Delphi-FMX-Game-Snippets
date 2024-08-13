/// <summary>
/// ***************************************************************************
///
/// Delphi FMX Game Snippets
///
/// Copyright 2021-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Examples of what is done when developing video games: sprite management,
/// background music, sound effects, animations, ...
///
/// Projects are developed under Delphi with its FireMonkey multiplatform
/// framework to run our projects under Windows, macOS, iOS, Android and Linux
/// from the same code base.
///
/// Not all images and musics used in this repository are free of charge.
/// Reuse them only if you have a license. They remain the property of their
/// respective authors and are only present in the programs for demo purposes.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://fmxgamesnippets.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets
///
/// ***************************************************************************
/// File last update : 2024-08-13T12:23:00.000+02:00
/// Signature : 64eaae7257d94a1f693ee172ce52910f1cb235f9
/// ***************************************************************************
/// </summary>

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
