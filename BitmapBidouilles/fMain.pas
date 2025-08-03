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
  File last update : 2025-08-03T19:37:24.000+02:00
  Signature : 1af459c991896b6bc790ab9ec54be75153d7a547
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
  FMX.Layouts,
  FMX.Effects,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TfrmMain = class(TForm)
    HorzScrollBox1: THorzScrollBox;
    SelectedShipEffect: TGlowEffect;
    tbZoom: TTrackBar;
    tbRotation: TTrackBar;
    Image1: TImage;
    procedure tbZoomTracking(Sender: TObject);
    procedure tbRotationTracking(Sender: TObject);
    procedure Image1Resized(Sender: TObject);
  private
    FSelectedImage: TImage;
    procedure SetSelectedimage(const Value: TImage);
  protected
    procedure SelectShipClick(Sender: TObject);
    procedure DoRepaintShipImage;
  public
    property SelectedImage: TImage read FSelectedImage write SetSelectedimage;
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.Math,
  udmShips;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
var
  img: TImage;
  i: integer;
begin
  inherited;

  FSelectedImage := nil;

  for i := 0 to dmShips.ImageList.Count - 1 do
  begin
    img := TImage.Create(self);
    try
      img.Parent := HorzScrollBox1;
      img.Margins.Top := 5;
      img.Margins.right := 5;
      img.Margins.bottom := 5;
      img.Margins.left := 5;
      img.Align := TAlignLayout.left;
      img.Width := img.Height;
      img.Bitmap.Assign(dmShips.ImageList.Bitmap(tsizef.Create(img.Width,
        img.Height), i));
      img.OnClick := SelectShipClick;
      img.tag := i;
    except
      img.free;
    end;
  end;

  tbZoom.Value := 100;
  tbRotation.Value := 0;
end;

procedure TfrmMain.DoRepaintShipImage;
var
  bmp: tbitmap;
  BitmapScale: single;
  ShipSize: single;
  X, Y: single;
begin
  if assigned(SelectedImage) then
  begin
    BitmapScale := Image1.Bitmap.BitmapScale;

    ShipSize := min(Image1.Width, Image1.Height) * tbZoom.Value / tbZoom.Max;

    bmp := tbitmap.Create;
    try
      // BMP needs real (physics) pixels to be copied to logical pixels in the TImage
      bmp.Assign(dmShips.ImageList.Bitmap(tsizef.Create(ShipSize * BitmapScale,
        ShipSize * BitmapScale), SelectedImage.tag));

      bmp.Rotate(tbRotation.Value);

      Image1.Bitmap.Canvas.BeginScene;
      try
        Image1.Bitmap.Canvas.Clear(talphacolors.Null);
        X := (Image1.Bitmap.Width - bmp.Width) / 2;
        Y := (Image1.Bitmap.Height - bmp.Height) / 2;
        Image1.Bitmap.Canvas.DrawBitmap(bmp, bmp.BoundsF,
          trectf.Create(X / BitmapScale, Y / BitmapScale,
          (X + bmp.Width) / BitmapScale, (Y + bmp.Height) / BitmapScale), 1);
      finally
        Image1.Bitmap.Canvas.EndScene;
      end;
    finally
      bmp.free;
    end;
  end
  else
    Image1.Bitmap.Clear(talphacolors.Null);
end;

procedure TfrmMain.Image1Resized(Sender: TObject);
begin
  Image1.Bitmap.SetSize(round(Image1.Width * Image1.Bitmap.BitmapScale),
    round(Image1.Height * Image1.Bitmap.BitmapScale));
  DoRepaintShipImage;
end;

procedure TfrmMain.SelectShipClick(Sender: TObject);
begin
  if (Sender is TImage) then
    SelectedImage := Sender as TImage;
end;

procedure TfrmMain.SetSelectedimage(const Value: TImage);
begin
  FSelectedImage := Value;
  if assigned(FSelectedImage) then
  begin
    SelectedShipEffect.Enabled := false;
    SelectedShipEffect.Parent := FSelectedImage;
    SelectedShipEffect.Enabled := true;
  end
  else
  begin
    SelectedShipEffect.Enabled := false;
    SelectedShipEffect.Parent := HorzScrollBox1;
  end;
  DoRepaintShipImage;
end;

procedure TfrmMain.tbRotationTracking(Sender: TObject);
begin
  DoRepaintShipImage;
end;

procedure TfrmMain.tbZoomTracking(Sender: TObject);
begin
  DoRepaintShipImage;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
