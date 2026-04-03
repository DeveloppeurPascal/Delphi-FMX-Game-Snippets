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
  File last update : 2025-10-25T20:59:46.564+02:00
  Signature : 1d05fe847217798567e6e1387a9be0aafe629df2
  ***************************************************************************
*)

unit Unit15;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Ani;

type
  TForm15 = class(TForm)
    Button1: TButton;
    lblScore: TLabel;
    animPointsAjoutes: TFloatAnimation;
    txtPointsAjoutes: TText;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure animPointsAjoutesFinish(Sender: TObject);
  private
    FScore: Integer;
    procedure SetScore(const Value: Integer);
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure AfficheScore;
    property Score: Integer read FScore write SetScore;
  end;

var
  Form15: TForm15;

implementation

{$R *.fmx}

procedure TForm15.AfficheScore;
begin
  lblScore.Text := 'Score : ' + FScore.ToString;
end;

procedure TForm15.Button1Click(Sender: TObject);
begin
  Score := Score + random(96) + 5; // 5 à ((96-1)+5) = 100 à chaque fois
end;

procedure TForm15.animPointsAjoutesFinish(Sender: TObject);
begin
  animPointsAjoutes.Enabled := false;
  txtPointsAjoutes.Visible := false;
  AfficheScore;
  Button1.Enabled := true;
end;

procedure TForm15.FormCreate(Sender: TObject);
begin
  txtPointsAjoutes.Visible := false;
  FScore := 0;
  AfficheScore;
end;

procedure TForm15.SetScore(const Value: Integer);
begin
  Button1.Enabled := false;
  FScore := Value;
  txtPointsAjoutes.Text := '+' + Value.ToString;
  txtPointsAjoutes.Position.Point :=
    pointf(Button1.Position.x + (Button1.Width - txtPointsAjoutes.Width) / 2,
    Button1.Position.y + (Button1.height - txtPointsAjoutes.height) / 2);
  txtPointsAjoutes.Visible := true;
  animPointsAjoutes.Start; // enabled := true
end;

end.
