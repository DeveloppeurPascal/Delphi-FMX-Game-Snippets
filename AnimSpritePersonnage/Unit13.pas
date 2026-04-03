(* C2PP
  ***************************************************************************

  Delphi FMX Game Snippets
  Copyright (c) 2021-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  https://codeberg.org/PatrickPremartin/Delphi-FMX-Game-Snippets

  ***************************************************************************
  File last update : 2025-10-25T20:59:46.592+02:00
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
