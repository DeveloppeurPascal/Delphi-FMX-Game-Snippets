﻿(* C2PP
  ***************************************************************************

  Delphi FMX Game Snippets

  Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

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
  https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets

  ***************************************************************************
  File last update : 2025-02-09T11:12:38.254+01:00
  Signature : e5edd634b7e3c457a6c0938555d7e4a6bd0b3443
  ***************************************************************************
*)

program EffetsSonores;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit5 in 'Unit5.pas' {Form5},
  Gamolf.FMX.MusicLoop in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.FMX.MusicLoop.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
