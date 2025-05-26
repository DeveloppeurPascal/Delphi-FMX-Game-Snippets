(* C2PP
  ***************************************************************************

  Delphi FMX Game Snippets

  Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  Examples of what is done when developing video games: sprite management,
  background music, sound effects, animations, ...

  Projects are developed under Delphi with its FireMonkey multiplatform
  framework to run our projects under Windows, macOS, iOS, Android and Linux
  from the same code base.

  Not all images and musics used in this repository are free of charge.
  Reuse them only if you have a license. They remain the property of their
  respective authors and are only present in the programs for demo purposes.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://fmxgamesnippets.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets

  ***************************************************************************
  File last update : 2025-02-09T11:12:38.304+01:00
  Signature : 958bcb9f2aef875e3896569a45bb65b064d5f483
  ***************************************************************************
*)

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
  FMX.Objects,
  uStarFieldData,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TfrmMain = class(TForm)
    Image1: TImage;
    LoopAnim: TTimer;
    Label1: TLabel;
    procedure LoopAnimTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
  private
  public
    StarField: TStarsList;
    SpeedX, SpeedY, SpeedZ: Single;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  StarField := TStarsList.Create;
  SpeedX := 0;
  SpeedY := 0;
  SpeedZ := 1;
end;

destructor TfrmMain.Destroy;
begin
  StarField.free;
  inherited;
end;

procedure TfrmMain.FormHide(Sender: TObject);
begin
  LoopAnim.Enabled := false;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkLeft then
  begin
    Key := 0;
    SpeedX := SpeedX - 1;
  end
  else if Key = vkRight then
  begin
    Key := 0;
    SpeedX := SpeedX + 1;
  end
  else if Key = vkup then
  begin
    Key := 0;
    SpeedY := SpeedY - 1;
  end
  else if Key = vkDown then
  begin
    Key := 0;
    SpeedY := SpeedY + 1;
  end
  else if (Key = vkEscape) or (Key = vkHardwareBack) then
  begin
    Key := 0;
    close;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LoopAnim.Enabled := true;
end;

procedure TfrmMain.LoopAnimTimer(Sender: TObject);
var
  BMP: TBitmap;
  BMPData: TBitmapData;
  i: integer;
  X, Y: integer;
  CenterX, CenterY: integer;
  r, g, b, a: byte;
begin
  StarField.Move(round(SpeedX), round(SpeedY), round(SpeedZ));
  // création d'un bitmap
  BMP := TBitmap.Create(160, 100);
  try
    BMP.Clear(talphacolors.Darkblue);
    // TODO : don't forget the BitmapScale
    CenterX := round((BMP.width / 2) + SpeedX);
    CenterY := round((BMP.height / 2) - SpeedY);
    if BMP.Map(TMapAccess.ReadWrite, BMPData) then
      try
        // parcourt de la liste des étoiles pour affichage de celles qui sont devant nous
        for i := 0 to StarField.count - 1 do
          if (StarField[i].z > 0) and (StarField[i].z < 256) then
          begin
            X := CenterX + round(StarField[i].X / StarField[i].z);
            Y := CenterY - round(StarField[i].Y / StarField[i].z);
            if (X >= 0) and (X < BMP.width) and (Y >= 0) and (Y < BMP.height)
            then
            begin
              a := 0;
              r := 256 - StarField[i].z;
              g := r;
              b := r;
              BMPData.SetPixel(X, Y, ((a * 256 + r) * 256 + g) * 256 + b);
            end;
{$IFDEF DEBUG}
            if i = 0 then
              Label1.Text := 'Move with the arrow keys - StarX=' + StarField[i]
                .X.ToString + ', StarY=' + StarField[i].Y.ToString + ', StarZ='
                + StarField[i].z.ToString + ', Red=' + r.ToString + ', ScreenX='
                + X.ToString + ', ScreenY=' + Y.ToString;
{$ENDIF}
          end;
      finally
        BMP.Unmap(BMPData);
      end;
    // switch du bitmap avec celui de l'image
    Image1.Bitmap.Assign(BMP);
    // Label1.Text := 'x=' + StarField[0].X.ToString + ', y=' + StarField[0]
    // .Y.ToString + ', z=' + StarField[0].z.ToString;
  finally
    BMP.free;
  end;
  if SpeedZ < 20 then
    SpeedZ := SpeedZ * 1.1;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
