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
  File last update : 2025-02-09T11:12:38.107+01:00
  Signature : c7f12c5837f99a127a1050db8f33ca85c2a33abd
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts;

type
  TForm21 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Circle1: TCircle;
    Timer1: TTimer;
    Circle2: TCircle;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCircleActive: TCircle;
    procedure OnOffAnimation(Circle: TCircle);
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form21: TForm21;

implementation

{$R *.fmx}

procedure TForm21.Button1Click(Sender: TObject);
begin
  OnOffAnimation(Circle1);
end;

procedure TForm21.Button2Click(Sender: TObject);
begin
  OnOffAnimation(Circle2);
end;

procedure TForm21.FormCreate(Sender: TObject);
begin
  FCircleActive := nil;
  Circle1.Visible := false;
  Circle2.Visible := false;
  Timer1.Enabled := false;
end;

procedure TForm21.OnOffAnimation(Circle: TCircle);
begin
  if Timer1.Enabled and Circle.Visible then
  begin
    Timer1.Enabled := false;
    // FCircleActive.Visible := false;
    Circle.Visible := false;
  end
  else
  begin
    if assigned(FCircleActive) then
      FCircleActive.Visible := false;
    FCircleActive := Circle;
    FCircleActive.Visible := true;
    Timer1.Enabled := true;
  end;
end;

procedure TForm21.Timer1Timer(Sender: TObject);
begin
  FCircleActive.RotationAngle := FCircleActive.RotationAngle + 10;
  while (FCircleActive.RotationAngle > 359) do
    FCircleActive.RotationAngle := FCircleActive.RotationAngle - 360;
end;

end.
