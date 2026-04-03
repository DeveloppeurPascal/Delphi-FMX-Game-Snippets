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
  File last update : 2025-10-25T20:59:46.611+02:00
  Signature : a4b0831d1256382bf01c9e41dbe7a31d507002db
  ***************************************************************************
*)

unit Unit9;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uSpriteCanard,
  FMX.Objects;

type
  TForm9 = class(TForm)
    SpriteCanard3: TSpriteCanard;
    SpriteCanard1: TSpriteCanard;
    SpriteCanard2: TSpriteCanard;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ClicSurCanard(CanardClique: TSpriteCanard);
    procedure ClicSurBaton(CanardClique: TSpriteCanard);
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

uses
  System.Threading, FMX.ani;

procedure TForm9.ClicSurBaton(CanardClique: TSpriteCanard);
begin
  if CanardClique.isEnMouvement then
    CanardClique.ImmobiliseLeCanard
  else
    CanardClique.BougeLeCanard;
end;

procedure TForm9.ClicSurCanard(CanardClique: TSpriteCanard);
const
  CDureeAnimation = 1; // en secondes
begin
  CanardClique.RotationCenter.x := 0.5;
  CanardClique.RotationCenter.y := 1;
  tanimator.AnimateFloat(CanardClique, 'rotationangle', 90, CDureeAnimation);
  ttask.run(
    procedure
    begin
      sleep(CDureeAnimation * 2000);
      tthread.ForceQueue(nil,
        procedure
        begin
          if csDestroying in CanardClique.ComponentState then
            exit;

          CanardClique.RotationAngle := 0;
          if random(100) < 50 then
          begin
            // TODO : position.X ne devrait pas avoir à être changée car nouvelle animation du canard
            // CanardClique.position.x := width;
            CanardClique.BougeLeCanardDeDroiteAGauche
          end
          else
          begin
            // CanardClique.position.x := -CanardClique.width;
            CanardClique.BougeLeCanardDeGaucheADroite;
          end;
        end);
    end);
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  SpriteCanard1.InitialiseZoneDeDeplacement;
  SpriteCanard1.Visible := false;
  SpriteCanard1.onClicSurCanard := ClicSurCanard;
  SpriteCanard1.onClicSurBaton := ClicSurBaton;
  SpriteCanard2.InitialiseZoneDeDeplacement;
  SpriteCanard2.Visible := false;
  SpriteCanard2.onClicSurCanard := ClicSurCanard;
  SpriteCanard2.onClicSurBaton := ClicSurBaton;
  SpriteCanard3.InitialiseZoneDeDeplacement;
  SpriteCanard3.Visible := false;
  SpriteCanard3.onClicSurCanard := ClicSurCanard;
  SpriteCanard3.onClicSurBaton := ClicSurBaton;
  ttask.run(
    procedure
    begin
      sleep(random(3000) + 100);
      tthread.Queue(nil,
        procedure
        begin
          SpriteCanard1.Visible := true;
          if random(100) < 50 then
            SpriteCanard1.BougeLeCanardDeDroiteAGauche
          else
            SpriteCanard1.BougeLeCanardDeGaucheADroite;
        end);
    end);
  ttask.run(
    procedure
    begin
      sleep(random(3000) + 100);
      tthread.Queue(nil,
        procedure
        begin
          SpriteCanard2.Visible := true;
          if random(100) < 50 then
            SpriteCanard2.BougeLeCanardDeDroiteAGauche
          else
            SpriteCanard2.BougeLeCanardDeGaucheADroite;
        end);
    end);
  ttask.run(
    procedure
    begin
      sleep(random(3000) + 100);
      tthread.Queue(nil,
        procedure
        begin
          SpriteCanard3.Visible := true;
          if random(100) < 50 then
            SpriteCanard3.BougeLeCanardDeDroiteAGauche
          else
            SpriteCanard3.BougeLeCanardDeGaucheADroite;
        end);
    end);
end;

procedure TForm9.FormResize(Sender: TObject);
begin
  SpriteCanard1.InitialiseZoneDeDeplacement;
  SpriteCanard2.InitialiseZoneDeDeplacement;
  SpriteCanard3.InitialiseZoneDeDeplacement;
end;

initialization

randomize;

end.
