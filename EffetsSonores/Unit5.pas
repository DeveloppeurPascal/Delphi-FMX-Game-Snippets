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
  Signature : 02a37526d9f5ff61fb60f14a8bb6489a7c0d6785
  ***************************************************************************
*)

unit Unit5;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

uses Gamolf.FMX.MusicLoop;

var
  mus1: TMusicLoop;

procedure TForm5.Button1Click(Sender: TObject);
begin
  if not assigned(mus1) then
  begin
    mus1 := TMusicLoop.Create;
    mus1.load('..\..\..\assets\GSP_500_Noises\16AXEHOR.WAV');
  end;
  mus1.play('', false);
end;

var
  mus2: TMusicLoop;

procedure TForm5.Button2Click(Sender: TObject);
begin
  if not assigned(mus2) then
  begin
    mus2 := TMusicLoop.Create;
    mus2.load('..\..\..\assets\GSP_500_Noises\16BEEROP.WAV');
  end;
  mus2.play(false);
end;

var
  mus3: TMusicLoop;

procedure TForm5.Button3Click(Sender: TObject);
begin
  if not assigned(mus3) then
  begin
    mus3 := TMusicLoop.Create;
    mus3.load('..\..\..\assets\GSP_500_Noises\16BELSL1.WAV');
  end;
  mus3.PlaySound;
end;

var
  mus5: TMusicLoop;

procedure TForm5.Button5Click(Sender: TObject);
begin
  if not assigned(mus5) then
  begin
    mus5 := TMusicLoop.Create;
    mus5.load('..\..\..\assets\GSP_500_Noises\16BURNER.WAV');
  end;
  mus5.PlaySound;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

mus1 := nil;
mus2 := nil;
mus3 := nil;
mus5 := nil;

finalization

if assigned(mus1) then
  mus1.Free;
if assigned(mus2) then
  mus2.Free;
if assigned(mus3) then
  mus3.Free;
if assigned(mus5) then
  mus5.Free;

end.
