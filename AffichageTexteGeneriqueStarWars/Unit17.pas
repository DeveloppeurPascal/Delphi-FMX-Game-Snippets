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
  File last update : 2026-04-02T17:53:30.000+02:00
  Signature : d1066e6f819e2f2fa9fd8d5372f91b5f7b3250d7
  ***************************************************************************
*)

unit Unit17;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Viewport3D,
  System.Math.Vectors, FMX.Ani, FMX.Objects, FMX.Controls3D, FMX.Layers3D,
  FMX.Layouts;

type
  TForm17 = class(TForm)
    Viewport3D1: TViewport3D;
    txtGenerique: TText;
    zoneTexteGenerique2D: TLayout;
    zoneTexteGenerique3D: TLayer3D;
    animTexteGenerique: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure animTexteGeneriqueTimer(Sender: TObject);
  private
    { Déclarations privées }
    procedure AnimationGeneriqueDemarrer;
    procedure AnimationGeneriqueTerminee;
    procedure LanceAudio(FichierMP3: string);
    procedure DeclencheLecture(fichier: string);
  public
    { Déclarations publiques }
  end;

var
  Form17: TForm17;

implementation

{$R *.fmx}

uses
  System.IOUtils, u_download, Gamolf.FMX.MusicLoop;

procedure TForm17.AnimationGeneriqueDemarrer;
var
  fichier: string;
begin
  // chemin Windows en dur, à adapter selon la plateforme
  txtGenerique.Width := zoneTexteGenerique2D.Width;
  // https://raw.githubusercontent.com/DeveloppeurPascal/DelphiFMXGameSnippets/main/README.md
  fichier := tpath.combine(tpath.GetTempPath, 'README.md');
  if not tfile.Exists(fichier) then
    tdownload_file.download
      ('https://codeberg.org/DeveloppeurPascal/Delphi-FMX-Game-Snippets/raw/branch/main/README.md',
      fichier,
      procedure
      begin
        DeclencheLecture(fichier);
      end,
      procedure
      begin
        showmessage(fichier + ' non trouvé');
      end)
  else
    DeclencheLecture(fichier);
end;

procedure TForm17.AnimationGeneriqueTerminee;
begin
  animTexteGenerique.Enabled := false;
  showmessage('Générique terminé.');
end;

procedure TForm17.animTexteGeneriqueTimer(Sender: TObject);
begin
  if (txtGenerique.Position.y > -txtGenerique.Height) then
    txtGenerique.Position.y := txtGenerique.Position.y - 5
    // txtGenerique.TextSettings.Font.Size / 5
  else
    AnimationGeneriqueTerminee;
end;

procedure TForm17.DeclencheLecture(fichier: string);
begin
  txtGenerique.Text := tfile.ReadAllText(fichier, TEncoding.UTF8);
  txtGenerique.AutoSize := true;
  txtGenerique.Position.y := zoneTexteGenerique2D.Height;
  zoneTexteGenerique2D.Visible := true;
  animTexteGenerique.Enabled := true;
end;

procedure TForm17.FormCreate(Sender: TObject);
var
  fichier: string;
begin
  zoneTexteGenerique2D.Visible := false;
  // https://www.soundboard.com/sb/sound/918028
  // => https://www.soundboard.com/track/download/918028
  //
  // http://soundfxcenter.com/download-sound/star-wars-main-theme-song/
  // => http://soundfxcenter.com/movies/star-wars/8d82b5_Star_Wars_Main_Theme_Song.mp3
  fichier := tpath.combine(tpath.GetTempPath, 'soundboard_sound_918028.mp3');
  if not tfile.Exists(fichier) then
    tdownload_file.download
      ('https://www.soundboard.com/track/download/918028', fichier,
      procedure
      begin
        LanceAudio(fichier);
      end)
  else
    LanceAudio(fichier);
end;

procedure TForm17.FormShow(Sender: TObject);
begin
  if not animTexteGenerique.Enabled then
    AnimationGeneriqueDemarrer;
end;

procedure TForm17.LanceAudio(FichierMP3: string);
begin
  musicloop.Play(FichierMP3);
end;

end.
