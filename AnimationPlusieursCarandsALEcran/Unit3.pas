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
  File last update : 2025-02-09T11:12:38.233+01:00
  Signature : 8b5fcb44840fde01e0e573e621c065f4a1dca070
  ***************************************************************************
*)

unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, uSpriteCanard, FMX.Layouts;

type
  TForm3 = class(TForm)
    btnAjoutCanard: TButton;
    zoneBas: TLayout;
    zoneHaut: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAjoutCanardClick(Sender: TObject);
    procedure zoneBasResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    ListeCanards: TSpriteCanardList;
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.btnAjoutCanardClick(Sender: TObject);
var
  canard: TSpriteCanard;
begin
  canard := TSpriteCanard.Create(Form3);
  case random(2) of
    0:
      canard.parent := zoneHaut;
  else
    canard.parent := zoneBas;
  end;
  ListeCanards.Add(canard);
  canard.InitialiseZoneDeDeplacement;
  canard.BougeLeCanardDeGaucheADroite;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  ListeCanards := TSpriteCanardList.Create;
  zoneBas.BringToFront;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  ListeCanards.free;
end;

procedure TForm3.zoneBasResize(Sender: TObject);
var
  zone: TLayout;
  i: integer;
begin
  if (Sender is TLayout) then
  begin
    zone := Sender as TLayout;
    for i := 0 to zone.ChildrenCount - 1 do
      if (zone.Children[i] is TSpriteCanard) then
        (zone.Children[i] as TSpriteCanard).InitialiseZoneDeDeplacement;
  end;
end;

end.
