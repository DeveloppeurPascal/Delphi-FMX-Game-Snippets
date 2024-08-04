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
///      Patrick PREMARTIN
///
/// Site :
///      https://fmxgamesnippets.developpeur-pascal.fr
///
/// Project site :
///      https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets
///
/// ***************************************************************************
/// File last update : 07/07/2024 08:50:40
/// Signature : ba7f6c6b70d34b007c325dd523afbfcd2dae5dec
/// ***************************************************************************
/// </summary>

unit Unit11;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm11 = class(TForm)
    btnExploseCanard: TButton;
    Image1: TImage;
    BitmapListAnimation1: TBitmapListAnimation;
    btnAnimationSurBackgroundfiche: TButton;
    procedure btnExploseCanardClick(Sender: TObject);
    procedure BitmapListAnimation1Finish(Sender: TObject);
    procedure btnAnimationSurBackgroundficheClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form11: TForm11;

implementation

{$R *.fmx}

procedure TForm11.BitmapListAnimation1Finish(Sender: TObject);
begin
  BitmapListAnimation1.Enabled := false;
end;

procedure TForm11.btnExploseCanardClick(Sender: TObject);
begin
  BitmapListAnimation1.Enabled := true;
end;

procedure TForm11.btnAnimationSurBackgroundficheClick(Sender: TObject);
begin
  Form11.Fill.Kind := TBrushKind.Bitmap;
  Form11.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  BitmapListAnimation1.parent := Form11;
  BitmapListAnimation1.PropertyName := 'Fill.Bitmap.Bitmap';
  BitmapListAnimation1.Enabled := true;
  Form11.Transparency := true;
end;

end.
