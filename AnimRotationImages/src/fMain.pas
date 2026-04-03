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
  File last update : 2025-10-25T20:59:46.571+02:00
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
