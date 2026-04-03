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
  File last update : 2025-10-25T20:59:46.522+02:00
  Signature : 7d42fda96428647a6aeb8db2a422fdb7db6657f3
  ***************************************************************************
*)

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
