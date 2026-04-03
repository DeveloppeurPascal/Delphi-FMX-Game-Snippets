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
  File last update : 2025-10-25T20:59:46.529+02:00
  Signature : 8b5fcb44840fde01e0e573e621c065f4a1dca070
  ***************************************************************************
*)

unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, uSpriteCanard, FMX.Layouts;

type
  TForm3 = class(TForm)
    btnAjoutCanard: TButton;
    zoneBas: TLayout;
    zoneHaut: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAjoutCanardClick(Sender: TObject);
    procedure zoneBasResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    ListeCanards: TSpriteCanardList;
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.btnAjoutCanardClick(Sender: TObject);
var
  canard: TSpriteCanard;
begin
  canard := TSpriteCanard.Create(Form3);
  case random(2) of
    0:
      canard.parent := zoneHaut;
  else
    canard.parent := zoneBas;
  end;
  ListeCanards.Add(canard);
  canard.InitialiseZoneDeDeplacement;
  canard.BougeLeCanardDeGaucheADroite;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  ListeCanards := TSpriteCanardList.Create;
  zoneBas.BringToFront;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  ListeCanards.free;
end;

procedure TForm3.zoneBasResize(Sender: TObject);
var
  zone: TLayout;
  i: integer;
begin
  if (Sender is TLayout) then
  begin
    zone := Sender as TLayout;
    for i := 0 to zone.ChildrenCount - 1 do
      if (zone.Children[i] is TSpriteCanard) then
        (zone.Children[i] as TSpriteCanard).InitialiseZoneDeDeplacement;
  end;
end;

end.
