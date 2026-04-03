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
  File last update : 2025-10-25T20:59:46.729+02:00
  Signature : 83dcd1764d26eae2f9aa0f194c6a0bff5687648f
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  cMatch3Game,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Effects;

type
  TfrmMain = class(TForm)
    cadMatch3Game1: TcadMatch3Game;
    btnPlay: TButton;
    GridPanelLayout1: TGridPanelLayout;
    lblScore: TLabel;
    lblLives: TLabel;
    GlowEffect1: TGlowEffect;
    GameContainer: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure GameContainerResized(Sender: TObject);
  private
    FLives: integer;
    FScore: integer;
    procedure SetLives(const Value: integer);
    procedure SetScore(const Value: integer);
  public
    procedure AfterConstruction; override;
    property Score: integer read FScore write SetScore;
    property Lives: integer read FLives write SetLives;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  USVGMatch3Items;

procedure TfrmMain.AfterConstruction;
begin
  inherited;
  lblScore.TextSettings.Font.Size := lblScore.TextSettings.Font.Size * 2;
  lblScore.TextSettings.Font.Style := [tfontstyle.fsbold];
  lblLives.TextSettings.Font.Size := lblLives.TextSettings.Font.Size * 2;
  lblLives.TextSettings.Font.Style := [tfontstyle.fsbold];
end;

procedure TfrmMain.btnPlayClick(Sender: TObject);
begin
  GridPanelLayout1.Visible := true;
  Score := 0;
  Lives := 3;
  cadMatch3Game1.FitInParent;
  cadMatch3Game1.Initialize;
  cadMatch3Game1.OnMatch3Proc := procedure(const Nb, Item: integer)
    begin
      Score := Score + Nb * Item;
    end;
  cadMatch3Game1.OnMoveButNoMatch3Proc := procedure
    begin
      Lives := Lives - 1;
    end;
  cadMatch3Game1.StartGame;
  btnPlay.Visible := false;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
const
  CDefaultBlocSize = 256;
var
  i: integer;
begin
  GridPanelLayout1.Visible := false;
  cadMatch3Game1.Clear;
  cadMatch3Game1.NbCol := 10;
  cadMatch3Game1.NbRow := 7;
  cadMatch3Game1.BackgroundColor := TAlphaColors.Darkcyan;
  cadMatch3Game1.SelectedBackgroundColor := TAlphaColors.cyan;
  for i := 0 to TSVGMatch3Items.Count - 1 do
    cadMatch3Game1.SVGItems[i] := TSVGMatch3Items.SVG(i);
  cadMatch3Game1.Width := cadMatch3Game1.NbCol * CDefaultBlocSize;
  cadMatch3Game1.Height := cadMatch3Game1.NbRow * CDefaultBlocSize;
end;

procedure TfrmMain.GameContainerResized(Sender: TObject);
begin
  cadMatch3Game1.FitInParent;
end;

procedure TfrmMain.SetLives(const Value: integer);
begin
  FLives := Value;
  lblLives.Text := 'Lives : ' + FLives.tostring;
  if FLives < 1 then
  begin
    cadMatch3Game1.StopGame;
    ShowMessage('Game Over' + slinebreak + 'Final score : ' + FScore.tostring);
    btnPlay.Visible := true;
  end;
end;

procedure TfrmMain.SetScore(const Value: integer);
begin
  FScore := Value;
  lblScore.Text := 'Score : ' + FScore.tostring;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
