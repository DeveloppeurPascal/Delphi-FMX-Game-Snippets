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
  File last update : 2025-10-25T20:59:46.576+02:00
  Signature : 172c57fe38a65df37341addf4fca641e50251d5f
  ***************************************************************************
*)

unit Unit16;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts;

type
  TForm16 = class(TForm)
    lblScore: TLabel;
    btnAjouteDesPoints: TButton;
    btnEnleveDesPoints: TButton;
    Layout1: TLayout;
    animAffichageScore: TTimer;
    procedure btnAjouteDesPointsClick(Sender: TObject);
    procedure btnEnleveDesPointsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure animAffichageScoreTimer(Sender: TObject);
  private
    FScore: integer;
    FScoreAffiche: integer;
    procedure SetScore(const Value: integer);
    procedure SetScoreAffiche(const Value: integer);
    { Déclarations privées }
  public
    { Déclarations publiques }
    property Score: integer read FScore write SetScore;
    property ScoreAffiche: integer read FScoreAffiche write SetScoreAffiche;
  end;

var
  Form16: TForm16;

implementation

{$R *.fmx}

procedure TForm16.animAffichageScoreTimer(Sender: TObject);
begin
  if FScoreAffiche > FScore then
    ScoreAffiche := ScoreAffiche - 1
  else if FScoreAffiche < FScore then
    ScoreAffiche := ScoreAffiche + 1;
end;

procedure TForm16.btnAjouteDesPointsClick(Sender: TObject);
begin
  Score := Score + random(96) + 5; // ajoute de 5 à 100 points lors d'un clic
end;

procedure TForm16.btnEnleveDesPointsClick(Sender: TObject);
begin
  Score := Score - random(96) + 5; // retire de 5 à 100 points lors d'un clic
end;

procedure TForm16.FormCreate(Sender: TObject);
begin
  ScoreAffiche := 0;
  Score := 0;
end;

procedure TForm16.SetScore(const Value: integer);
begin
  FScore := Value;
end;

procedure TForm16.SetScoreAffiche(const Value: integer);
begin
  FScoreAffiche := Value;
  lblScore.Text := 'Score : ' + FScoreAffiche.ToString;
end;

end.
