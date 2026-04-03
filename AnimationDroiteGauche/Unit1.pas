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
  File last update : 2025-10-25T20:59:46.516+02:00
  Signature : a471c6ceea9b18ccf5cb413c5f9de00004406160
  ***************************************************************************
*)

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Ani;

type
  TForm1 = class(TForm)
    Baton: TRectangle;
    Canard_VersLaDroite: TRectangle;
    Cible: TLayout;
    DeLaGaucheVersLaDroite: TFloatAnimation;
    DeLaDroiteVersLaGauche: TFloatAnimation;
    Canard_VersLaGauche: TRectangle;
    procedure DeLaDroiteVersLaGaucheFinish(Sender: TObject);
    procedure DeLaGaucheVersLaDroiteFinish(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure BougeLeCanardDeGaucheADroite;
    procedure BougeLeCanardDeDroiteAGauche;
    procedure InitialiseZoneDeDeplacement;
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DeLaDroiteVersLaGaucheFinish(Sender: TObject);
begin
  BougeLeCanardDeGaucheADroite;
end;

procedure TForm1.DeLaGaucheVersLaDroiteFinish(Sender: TObject);
begin
  BougeLeCanardDeDroiteAGauche;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitialiseZoneDeDeplacement;
  BougeLeCanardDeGaucheADroite;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  InitialiseZoneDeDeplacement;
end;

procedure TForm1.BougeLeCanardDeGaucheADroite;
begin
  DeLaDroiteVersLaGauche.Enabled := false;
  DeLaGaucheVersLaDroite.Enabled := true;
  Canard_VersLaGauche.Visible := DeLaDroiteVersLaGauche.Enabled;
  Canard_VersLaDroite.Visible := DeLaGaucheVersLaDroite.Enabled;
end;

procedure TForm1.BougeLeCanardDeDroiteAGauche;
begin
  DeLaGaucheVersLaDroite.Enabled := false;
  DeLaDroiteVersLaGauche.Enabled := true;
  Canard_VersLaGauche.Visible := DeLaDroiteVersLaGauche.Enabled;
  Canard_VersLaDroite.Visible := DeLaGaucheVersLaDroite.Enabled;
end;

procedure TForm1.InitialiseZoneDeDeplacement;
begin
  Cible.Position.y := height - Cible.height;
  DeLaGaucheVersLaDroite.StartValue := -Cible.Width;
  DeLaGaucheVersLaDroite.StopValue := Width;
  DeLaDroiteVersLaGauche.StartValue := DeLaGaucheVersLaDroite.StopValue;
  DeLaDroiteVersLaGauche.StopValue := DeLaGaucheVersLaDroite.StartValue;
end;

end.
