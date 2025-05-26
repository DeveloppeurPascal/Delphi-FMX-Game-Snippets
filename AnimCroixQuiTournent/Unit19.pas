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
  File last update : 2025-02-09T11:12:38.090+01:00
  Signature : 7c1853e6050cb4dd95d79f402423fca0e1525f69
  ***************************************************************************
*)

unit Unit19;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, Radiant.Shapes, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm19 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    FirstTime: boolean;
    procedure FinRotation(Sender: TObject);
  public
    { Déclarations publiques }
  end;

const
  CTailleCroix = 100;

var
  Form19: TForm19;

implementation

{$R *.fmx}

procedure TForm19.FinRotation(Sender: TObject);
var
  croix: tradiantplus;
  Ani: tfloatanimation;
begin
  if (Sender is tfloatanimation) then
  begin
    Ani := (Sender as tfloatanimation);
    if (Ani.parent is tradiantplus) then
    begin
      croix := Ani.parent as tradiantplus;
      croix.TagFloat := 0;
      tthread.ForceQueue(nil,
        procedure
        begin
          Ani.free;
        end);
    end
    else
      raise exception.create('couac');
  end
  else
    raise exception.create('couic');
end;

procedure TForm19.FormCreate(Sender: TObject);
begin
  FirstTime := true;
end;

procedure TForm19.FormShow(Sender: TObject);
var
  croix: tradiantplus;
  x, y: single;
  XDeDepart: single;
  CouleurBlanche: boolean;
  CouleurBlancheAuDepart: boolean;
begin
  if FirstTime then
  begin
    FirstTime := false;
    y := -CTailleCroix * 2 / 3;
    XDeDepart := -CTailleCroix;
    CouleurBlancheAuDepart := true;
    while y < clientheight do
    begin
      x := XDeDepart;
      CouleurBlanche := CouleurBlancheAuDepart;
      while x < clientwidth do
      begin
        // background utilisé lors de la rotation
        croix := tradiantplus.create(Self);
        croix.parent := Self;
        croix.Position.Point := pointf(x, y);
        croix.BarSize.ScaleFactor := 0.33;
        croix.Width := CTailleCroix;
        croix.Height := CTailleCroix;
        if not CouleurBlanche then // background donc couleur inversée
        begin
          croix.Fill.Color := talphacolors.white;
          croix.tag := 1; // couleur blanche / noire
        end
        else
        begin
          croix.Fill.Color := talphacolors.black;
          croix.tag := 0; // couleur blanche / noire
        end;
        croix.stroke.Kind := tbrushkind.None;
        // croix.stroke.Color := croix.Fill.Color;
        croix.TagFloat := -1; // ne doit pas bouger

        // croix qui subit la rotation
        croix := tradiantplus.create(Self);
        croix.parent := Self;
        croix.Position.Point := pointf(x, y);
        croix.BarSize.ScaleFactor := 0.33;
        croix.Width := CTailleCroix;
        croix.Height := CTailleCroix;
        if CouleurBlanche then
        begin
          croix.Fill.Color := talphacolors.white;
          croix.tag := 1; // couleur blanche / noire
        end
        else
        begin
          croix.Fill.Color := talphacolors.black;
          croix.tag := 0; // couleur blanche / noire
        end;
        croix.stroke.Kind := tbrushkind.None;
        // croix.stroke.Color := croix.Fill.Color;
        croix.TagFloat := 0; // animee ou pas

        // passage à la suivante sur la même ligne
        x := x + CTailleCroix * 5 / 3;
        CouleurBlanche := not CouleurBlanche;
      end;
      y := y + CTailleCroix * 1 / 3;
      CouleurBlancheAuDepart := not CouleurBlancheAuDepart;
      XDeDepart := XDeDepart + CTailleCroix * 2 / 3;
      while XDeDepart > 0 do
      begin
        XDeDepart := XDeDepart - CTailleCroix * 5 / 3;
        CouleurBlancheAuDepart := not CouleurBlancheAuDepart;
      end;
    end;
  end;
end;

procedure TForm19.Timer1Timer(Sender: TObject);
var
  croix: tradiantplus;
  obj: tfmxobject;
  Ani: tfloatanimation;
begin
  if childrencount > 0 then
  begin
    obj := Children[random(childrencount)];
    if obj is tradiantplus then
    begin
      croix := obj as tradiantplus;
      if croix.TagFloat = 0 then
      begin
        croix.TagFloat := 1;
        croix.BringToFront;
        Ani := tfloatanimation.create(Self);
        Ani.parent := croix;
        Ani.PropertyName := 'RotationAngle';
        Ani.StartFromCurrent := false;
        Ani.Duration := 0.5;
        Ani.StartValue := croix.RotationAngle;
        case croix.tag of
          0:
            Ani.StopValue := croix.RotationAngle + 90;
          1:
            Ani.StopValue := croix.RotationAngle - 90;
        end;
        Ani.OnFinish := FinRotation;
        Ani.Start;
      end;
    end;
  end;
end;

initialization

randomize;
{$IFDEF DEBUG}
ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
