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
  File last update : 2025-10-25T20:59:46.755+02:00
  Signature : a58e6b6892efbd533dd7285f7b64ad71c504e13d
  ***************************************************************************
*)

unit uStarFieldData;

interface

uses
  System.Generics.Collections;

type
  TStar = class(TObject)
  private
    FZ: integer;
    FX: integer;
    FY: integer;
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    procedure SetZ(const Value: integer);
  protected
  public
    property X: integer read FX write SetX;
    property Y: integer read FY write SetY;
    property Z: integer read FZ write SetZ;
    class function GetNewStar(const MaxX, MaxY, MaxZ: integer): TStar;
  end;

  TStarsList = class(TObjectList<TStar>)
  private
    FMaxX, FMaxY, FMaxZ: integer;
  protected
  public
    property MaxX: integer read FMaxX;
    property MaxY: integer read FMaxY;
    property MaxZ: integer read FMaxZ;
    constructor Create(const ANbStars: integer = 100;
      const AMaxX: integer = 500; const AMaxY: integer = 500;
      const AMaxZ: integer = 500); virtual;
    destructor Destroy; override;
    procedure Move(VX, VY, VZ: integer);
  end;

implementation

{ TStar }

class function TStar.GetNewStar(const MaxX, MaxY, MaxZ: integer): TStar;
begin
  result := TStar.Create;
  result.X := random(2 * MaxX) - MaxX;
  result.Y := random(2 * MaxY) - MaxY;
  result.Z := random(2 * MaxZ) - MaxZ;
end;

procedure TStar.SetX(const Value: integer);
begin
  FX := Value;
end;

procedure TStar.SetY(const Value: integer);
begin
  FY := Value;
end;

procedure TStar.SetZ(const Value: integer);
begin
  FZ := Value;
end;

{ TStarsList }

constructor TStarsList.Create(const ANbStars, AMaxX, AMaxY, AMaxZ: integer);
var
  i: integer;
begin
  inherited Create;

  FMaxX := AMaxX;
  FMaxY := AMaxY;
  FMaxZ := AMaxZ;

  for i := 1 to ANbStars do
    self.Add(TStar.GetNewStar(FMaxX, FMaxY, FMaxZ));
end;

destructor TStarsList.Destroy;
begin
  // TODO : à compléter ou supprimer
  inherited;
end;

procedure TStarsList.Move(VX, VY, VZ: integer);
var
  i: integer;
begin
  for i := 0 to self.Count - 1 do
  begin
    self[i].X := self[i].X - VX;
    while (self[i].X > FMaxX) do
      self[i].X := self[i].X - 2 * FMaxX;
    while (self[i].X < -FMaxX) do
      self[i].X := self[i].X + 2 * FMaxX;
    self[i].Y := self[i].Y - VY;
    while (self[i].Y > FMaxY) do
      self[i].Y := self[i].Y - 2 * FMaxY;
    while (self[i].Y < -FMaxY) do
      self[i].Y := self[i].Y + 2 * FMaxY;
    self[i].Z := self[i].Z - VZ;
    while (self[i].Z > FMaxZ) do
      self[i].Z := self[i].Z - 2 * FMaxZ;
    while (self[i].Z < -FMaxZ) do
      self[i].Z := self[i].Z + 2 * FMaxZ;
  end;
end;

end.
