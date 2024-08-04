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
/// Signature : d700cd206d03fc5b787970b0155ae9e6fc5f3f34
/// ***************************************************************************
/// </summary>

unit Unit8;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Ani, Gamolf.FMX.MusicLoop;

type
  TForm8 = class(TForm)
    MonCanardQuiSouffre: TRectangle;
    animRotation: TFloatAnimation;
    animScale: TFloatAnimation;
    procedure animRotationFinish(Sender: TObject);
    procedure animScaleProcess(Sender: TObject);
    procedure animScaleFinish(Sender: TObject);
    procedure MonCanardQuiSouffreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    audioCouic: TMusicLoop;
  public
    { Déclarations publiques }
  end;

var
  Form8: TForm8;

implementation

{$R *.fmx}

procedure TForm8.animRotationFinish(Sender: TObject);
begin
  animRotation.enabled := false;
end;

procedure TForm8.animScaleFinish(Sender: TObject);
begin
  animScale.enabled := false;
end;

procedure TForm8.animScaleProcess(Sender: TObject);
begin
  MonCanardQuiSouffre.scale.y := MonCanardQuiSouffre.scale.x;
  MonCanardQuiSouffre.opacity :=
    1 - (MonCanardQuiSouffre.scale.x / animScale.StopValue);
  // TODO: déplacer le canard au prorata de sa position initiale
  // MonCanardQuiSouffre.Position.x := MonCanardQuiSouffre.Position.x -
  // (MonCanardQuiSouffre.Width / 2) * MonCanardQuiSouffre.scale.x;
  // MonCanardQuiSouffre.Position.y := MonCanardQuiSouffre.Position.y -
  // (MonCanardQuiSouffre.height / 2) * MonCanardQuiSouffre.scale.y;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  audioCouic := TMusicLoop.Current;
{$IFDEF MSWINDOWS}
  audioCouic.Load('..\..\..\assets\TheGameCreators\SoundMatter\DuckyOuch.wav');
{$ENDIF}
end;

procedure TForm8.MonCanardQuiSouffreClick(Sender: TObject);
begin
  animRotation.enabled := true;
  animScale.enabled := true;
  audioCouic.PlaySound;
end;

end.
