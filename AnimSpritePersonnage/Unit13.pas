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
  File last update : 2025-02-09T11:12:38.225+01:00
  Signature : b659fe03bfaad6bb912ed514dca753327db45783
  ***************************************************************************
*)

unit Unit13;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Ani, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TForm13 = class(TForm)
    Rectangle1: TRectangle;
    Layout1: TLayout;
    Image1: TImage;
    animWalking: TBitmapListAnimation;
    animPositionX: TFloatAnimation;
    animJump: TBitmapListAnimation;
    btnSaute: TButton;
    Memo1: TMemo;
    procedure animPositionXFinish(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure animJumpFinish(Sender: TObject);
    procedure btnSauteClick(Sender: TObject);
    procedure animJumpProcess(Sender: TObject);
  private
    { Déclarations privées }
    procedure NinjaAvance;
  public
    { Déclarations publiques }
  end;

var
  Form13: TForm13;

implementation

{$R *.fmx}

procedure TForm13.animJumpFinish(Sender: TObject);
begin
  animJump.Enabled := false;
  animWalking.Enabled := true;
  Image1.Position.y := Image1.TagFloat;
end;

procedure TForm13.animJumpProcess(Sender: TObject);
var
  pourcent: single;
begin
  pourcent := animJump.CurrentTime * 100 / animJump.Duration;
  Memo1.lines.insert(0, pourcent.tostring + ' ' +
    animJump.AnimationCount.tostring + ' ' + animJump.CurrentTime.tostring + ' '
    + animJump.Duration.tostring);
  if (pourcent < 20) then
    Image1.Position.y := Image1.Position.y - 3
  else if (pourcent < 40) then
    Image1.Position.y := Image1.Position.y - 1.5
  else if (pourcent < 60) then
    Image1.Position.y := Image1.Position.y
  else if (pourcent < 80) then
    Image1.Position.y := Image1.Position.y + 1.5
  else
    Image1.Position.y := Image1.Position.y + 3;
end;

procedure TForm13.animPositionXFinish(Sender: TObject);
begin
  animPositionX.Enabled := false;
  if Image1.Position.x > width then
    Image1.Position.x := -Image1.width;
  NinjaAvance;
end;

procedure TForm13.btnSauteClick(Sender: TObject);
begin
  animWalking.Enabled := false;
  Image1.TagFloat := Image1.Position.y;
  animJump.Enabled := true;
end;

procedure TForm13.FormCreate(Sender: TObject);
begin
  NinjaAvance;
end;

procedure TForm13.NinjaAvance;
begin
  animPositionX.StartValue := Image1.Position.x;
  animPositionX.StopValue := Image1.Position.x + Image1.width / 3;
  animPositionX.Enabled := true;
end;

end.
