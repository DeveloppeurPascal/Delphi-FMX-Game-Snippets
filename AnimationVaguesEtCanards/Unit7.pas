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
  File last update : 2025-10-25T20:59:46.547+02:00
  Signature : addb389bb6ba83ca7511c47c667a1d57c6c2a513
  ***************************************************************************
*)

unit Unit7;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Unit6, FMX.Ani, FMX.Objects, FMX.Layouts, FMX.Controls.Presentation,
  uSpriteCanard;

type
  TForm7 = class(TForm6)
    TimerInitCanards: TTimer;
    procedure Vagues01Resize(Sender: TObject);
    procedure TimerInitCanardsTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    ListeCanards: TSpriteCanardList;
  public
    { Déclarations publiques }
    function DecalageHauteurCanard(canard: TSpriteCanard): integer;
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

const
  CNbMaxCanards = 10;

function TForm7.DecalageHauteurCanard(canard: TSpriteCanard): integer;
begin
  result := -110 { on remonte le canard de sa haute pour voir le socle }
    + random(3) - 1;
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  inherited;
  ListeCanards := TSpriteCanardList.Create;
end;

procedure TForm7.FormDestroy(Sender: TObject);
begin
  inherited;
  ListeCanards.free;
end;

procedure TForm7.TimerInitCanardsTimer(Sender: TObject);
var
  canard: TSpriteCanard;
begin
  if (ListeCanards.Count < CNbMaxCanards) then
  begin
    canard := TSpriteCanard.Create(Self);
    case random(3) of
      0:
        canard.parent := vagues01;
      1:
        canard.parent := vagues02;
    else
      canard.parent := vagues03;
    end;
    canard.InitialiseZoneDeDeplacement;
    case random(2) of
      0:
        canard.BougeLeCanardDeGaucheADroite;
    else
      canard.BougeLeCanardDeDroiteAGauche;
    end;
    canard.onGetDecalageHauteurCanard := DecalageHauteurCanard;
    ListeCanards.Add(canard);
  end;
end;

procedure TForm7.Vagues01Resize(Sender: TObject);
var
  zone: TRectangle;
  i: integer;
begin
  inherited;
  if (Sender is TRectangle) then
  begin
    zone := Sender as TRectangle;
    for i := 0 to zone.ChildrenCount - 1 do
      if (zone.Children[i] is TSpriteCanard) then
        (zone.Children[i] as TSpriteCanard).InitialiseZoneDeDeplacement;
  end;

end;

initialization

randomize;

end.
