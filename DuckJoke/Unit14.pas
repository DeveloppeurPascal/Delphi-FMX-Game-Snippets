(* C2PP
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
  Signature : 5b01b9a1b94d4de03afeae90b31a3f428d2b6cfc
  ***************************************************************************
*)

unit Unit14;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uSpriteCanard;

type
  TfrmDuckJoke = class(TForm)
    SpriteCanard1: TSpriteCanard;
    Timer1: TTimer;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    TempsEcoule: Boolean;
  public
    { Déclarations publiques }
  end;

var
  frmDuckJoke: TfrmDuckJoke;

implementation

{$R *.fmx}

procedure TfrmDuckJoke.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := TempsEcoule;
end;

procedure TfrmDuckJoke.FormCreate(Sender: TObject);
begin
  TempsEcoule := false;
  Timer1.Interval := 5 * 60 * 1000;
  Timer1.Enabled := true;
  SpriteCanard1.InitialiseZoneDeDeplacement;
  SpriteCanard1.DeLaGaucheVersLaDroite.Duration := 2 * width / 100;
  SpriteCanard1.DeLaDroiteVersLaGauche.Duration := 2 * width / 100;
  if random(100) < 50 then
    SpriteCanard1.BougeLeCanardDeGaucheADroite
  else
    SpriteCanard1.BougeLeCanardDeDroiteAGauche;
end;

procedure TfrmDuckJoke.FormResize(Sender: TObject);
begin
  SpriteCanard1.InitialiseZoneDeDeplacement;
  SpriteCanard1.DeLaGaucheVersLaDroite.Duration := 2 * width / 100;
  SpriteCanard1.DeLaDroiteVersLaGauche.Duration := 2 * width / 100;
end;

procedure TfrmDuckJoke.Timer1Timer(Sender: TObject);
begin
  TempsEcoule := true;
  SpriteCanard1.Opacity := 0.8;
  Timer1.Enabled := false;
end;

initialization

randomize;

end.
