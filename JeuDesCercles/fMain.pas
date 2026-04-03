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
  File last update : 2025-10-25T20:59:46.699+02:00
  Signature : acc220820dac07d3d425ecc9e4282005f6bf418f
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uSpriteCercle;

const
  NbCerclesMax = 10;

type
  TfrmMain = class(TForm)
    TimerGenerateur: TTimer;
    TimerBoucleDuJeu: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerGenerateurTimer(Sender: TObject);
    procedure TimerBoucleDuJeuTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    CerclesEnJeu: TSpriteCercleList;
    procedure AjouteUnCercle;
    procedure ClicSurCercle(Cercle: TSpriteCercle);
    procedure CollisionSurCercle(Cercle: TSpriteCercle);
    procedure SupprimeLeCercle(Cercle: TSpriteCercle);
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.AjouteUnCercle;
var
  c: TSpriteCercle;
begin
  c := TSpriteCercle.Create(self);
  c.Parent := self;
  c.width := random(5) * 30 + 30;
  c.position.x := random(width);
  c.position.y := random(height);
  c.vx := random(11) - 5; // -5 à +5
  c.vy := random(11) - 5; // -5 à +5
  c.onSpriteClic := ClicSurCercle;
  c.onSpriteCollision := CollisionSurCercle;
  CerclesEnJeu.Add(c);
end;

procedure TfrmMain.TimerBoucleDuJeuTimer(Sender: TObject);
begin
  for var e in Children do
    if (e is TSpriteCercle) then
      (e as TSpriteCercle).Bouge;
end;

procedure TfrmMain.ClicSurCercle(Cercle: TSpriteCercle);
begin
  // TODO : traiter clic sur cercle
  SupprimeLeCercle(Cercle);
end;

procedure TfrmMain.CollisionSurCercle(Cercle: TSpriteCercle);
begin
  // TODO : traiter la collision
  SupprimeLeCercle(Cercle);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CerclesEnJeu := TSpriteCercleList.Create;
  TimerGenerateur.Enabled := true;
  TimerBoucleDuJeu.Enabled := true;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  CerclesEnJeu.Free;
end;

procedure TfrmMain.SupprimeLeCercle(Cercle: TSpriteCercle);
begin
  tthread.ForceQueue(nil,
    procedure
    begin
      CerclesEnJeu.Remove(Cercle);
    end);
end;

procedure TfrmMain.TimerGenerateurTimer(Sender: TObject);
begin
  if (CerclesEnJeu.Count < NbCerclesMax) and (random(100) < 50) then
    AjouteUnCercle;
end;

initialization

randomize;

{$IFDEF DEBUG}
ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
