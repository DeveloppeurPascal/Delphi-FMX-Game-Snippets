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
  File last update : 2025-10-25T20:59:46.740+02:00
  Signature : df322e4fbe0817bb317ac6d93a04d60eae706c5e
  ***************************************************************************
*)

unit Unit20;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, Gamolf.FMX.MusicLoop;

type
  TForm20 = class(TForm)
    swMusic: TSwitch;
    swSon: TSwitch;
    lblMusic: TLabel;
    lblSound: TLabel;
    tbMusic: TTrackBar;
    tbSon: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure swMusicSwitch(Sender: TObject);
    procedure swSonSwitch(Sender: TObject);
    procedure tbMusicTracking(Sender: TObject);
    procedure tbSonTracking(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    MusicMP3: TMusicLoop;
    SonWAV: TMusicLoop;
    function getMusicLoop(FileName: string): TMusicLoop;
  public
    { Déclarations publiques }
    procedure LoadFileFromURL(URL: string; FileName: string;
      OnSuccessProc: TProc < string >= nil);
  end;

var
  Form20: TForm20;

implementation

{$R *.fmx}

uses
  System.IOUtils, u_download;

procedure TForm20.FormClose(Sender: TObject; var Action: TCloseAction);
var
  fichier: string;
begin
  fichier := MusicMP3.FileName;
  if tfile.exists(fichier) then
    tfile.Delete(fichier);
  fichier := SonWAV.FileName;
  if tfile.exists(fichier) then
    tfile.Delete(fichier);
end;

procedure TForm20.FormCreate(Sender: TObject);
begin
  lblSound.enabled := false;
  lblMusic.enabled := false;

  // son : https://opengameart.org/content/win-sound-effect
  // https://opengameart.org/sites/default/files/Win%20sound.wav
  LoadFileFromURL('https://opengameart.org/sites/default/files/Win%20sound.wav',
    'son.wav',
    procedure(FileName: string)
    begin
      SonWAV := getMusicLoop(FileName);
      lblSound.enabled := SonWAV.IsActive;
      tbSon.Value := SonWAV.Volume;
    end);

  // music : https://opengameart.org/content/battle-theme-b-for-rpg
  // https://opengameart.org/sites/default/files/battleThemeB.mp3
  LoadFileFromURL
    ('https://opengameart.org/sites/default/files/battleThemeB.mp3',
    'music.mp3',
    procedure(FileName: string)
    begin
      MusicMP3 := getMusicLoop(FileName);
      lblMusic.enabled := MusicMP3.IsActive;
      tbMusic.Value := MusicMP3.Volume;
    end);
end;

procedure TForm20.FormDestroy(Sender: TObject);
begin
  if assigned(MusicMP3) then
    MusicMP3.free;
  if assigned(SonWAV) then
    SonWAV.free;
end;

function TForm20.getMusicLoop(FileName: string): TMusicLoop;
begin
  result := TMusicLoop.Create;
  result.Load(FileName);
end;

procedure TForm20.LoadFileFromURL(URL, FileName: string;
OnSuccessProc: TProc<string>);
var
  fichier: string;
begin
  fichier := tpath.combine(tpath.GetTempPath, FileName);
  if not tfile.exists(fichier) then
    tdownload_file.download(URL, fichier,
      procedure
      begin
        if assigned(OnSuccessProc) then
          OnSuccessProc(fichier);
      end,
      procedure
      begin
        ShowMessage('Téléchargement non effectué.');
      end)
  else if assigned(OnSuccessProc) then
    OnSuccessProc(fichier);
end;

procedure TForm20.swMusicSwitch(Sender: TObject);
begin
  if assigned(MusicMP3) then
    if swMusic.IsChecked then
      MusicMP3.Play
    else
      MusicMP3.Stop;
end;

procedure TForm20.swSonSwitch(Sender: TObject);
begin
  if assigned(SonWAV) then
    if swSon.IsChecked then
      SonWAV.PlaySound
    else
      SonWAV.Stop;
end;

procedure TForm20.tbMusicTracking(Sender: TObject);
begin
  MusicMP3.Volume := round(tbMusic.Value);
end;

procedure TForm20.tbSonTracking(Sender: TObject);
begin
  SonWAV.Volume := round(tbSon.Value);
end;

end.
