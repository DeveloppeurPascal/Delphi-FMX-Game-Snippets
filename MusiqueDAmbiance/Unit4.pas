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
  File last update : 2025-10-25T20:59:46.747+02:00
  Signature : 1e4b4df46550fbe3192748194bfb98c98b1e3b93
  ***************************************************************************
*)

unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, Gamolf.FMX.MusicLoop;

type
  TForm4 = class(TForm)
    btnChoisirFichierMP3: TButton;
    btnPlay: TButton;
    btnStop: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Layout1: TLayout;
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnChoisirFichierMP3Click(Sender: TObject);
  private
    { Déclarations privées }
  public
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses System.ioutils;

procedure TForm4.btnChoisirFichierMP3Click(Sender: TObject);
begin
  if OpenDialog1.Execute and tfile.exists(OpenDialog1.filename) and
    (tpath.GetExtension(OpenDialog1.filename).ToLower = '.mp3') then
  begin
    MusicLoop.Play(OpenDialog1.filename);
    Memo1.lines.Insert(0, 'Lecture de ' + OpenDialog1.filename);
  end;
end;

procedure TForm4.btnPlayClick(Sender: TObject);
begin
  if not MusicLoop.IsPlaying then
  begin
    MusicLoop.Play;
    Memo1.lines.Insert(0, 'Play');
  end;
end;

procedure TForm4.btnStopClick(Sender: TObject);
begin
  if MusicLoop.IsPlaying then
  begin
    MusicLoop.stop;
    Memo1.lines.Insert(0, 'Stop');
  end;
end;

end.
