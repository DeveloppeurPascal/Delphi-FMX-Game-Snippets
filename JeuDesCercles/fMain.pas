/// <summary>
/// ***************************************************************************
///
/// Delphi FMX Game Snippets
///
/// Copyright 2021-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Examples of what is done when developing video games: sprite management,
/// background music, sound effects, animations, ...
///
/// Projects are developed under Delphi with its FireMonkey multiplatform
/// framework to run our projects under Windows, macOS, iOS, Android and Linux
/// from the same code base.
///
/// Not all images and musics used in this repository are free of charge.
/// Reuse them only if you have a license. They remain the property of their
/// respective authors and are only present in the programs for demo purposes.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://fmxgamesnippets.developpeur-pascal.fr
///
/// Project site :
///      https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets
///
/// ***************************************************************************
/// File last update : 07/07/2024 08:50:40
/// Signature : fde4fe44513e81f40aa25d3948c3b8b806c8c597
/// ***************************************************************************
/// </summary>

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uSpriteCercle;

const
  NbCerclesMax = 10;

type
  TfrmMain = class(TForm)
    TimerGenerateur: TTimer;
    TimerBoucleDuJeu: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerGenerateurTimer(Sender: TObject);
    procedure TimerBoucleDuJeuTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    CerclesEnJeu: TSpriteCercleList;
    procedure AjouteUnCercle;
    procedure ClicSurCercle(Cercle: TSpriteCercle);
    procedure CollisionSurCercle(Cercle: TSpriteCercle);
    procedure SupprimeLeCercle(Cercle: TSpriteCercle);
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.AjouteUnCercle;
var
  c: TSpriteCercle;
begin
  c := TSpriteCercle.Create(self);
  c.Parent := self;
  c.width := random(5) * 30 + 30;
  c.position.x := random(width);
  c.position.y := random(height);
  c.vx := random(11) - 5; // -5 à +5
  c.vy := random(11) - 5; // -5 à +5
  c.onSpriteClic := ClicSurCercle;
  c.onSpriteCollision := CollisionSurCercle;
  CerclesEnJeu.Add(c);
end;

procedure TfrmMain.TimerBoucleDuJeuTimer(Sender: TObject);
begin
  for var e in Children do
    if (e is TSpriteCercle) then
      (e as TSpriteCercle).Bouge;
end;

procedure TfrmMain.ClicSurCercle(Cercle: TSpriteCercle);
begin
  // TODO : traiter clic sur cercle
  SupprimeLeCercle(Cercle);
end;

procedure TfrmMain.CollisionSurCercle(Cercle: TSpriteCercle);
begin
  // TODO : traiter la collision
  SupprimeLeCercle(Cercle);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CerclesEnJeu := TSpriteCercleList.Create;
  TimerGenerateur.Enabled := true;
  TimerBoucleDuJeu.Enabled := true;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  CerclesEnJeu.Free;
end;

procedure TfrmMain.SupprimeLeCercle(Cercle: TSpriteCercle);
begin
  tthread.ForceQueue(nil,
    procedure
    begin
      CerclesEnJeu.Remove(Cercle);
    end);
end;

procedure TfrmMain.TimerGenerateurTimer(Sender: TObject);
begin
  if (CerclesEnJeu.Count < NbCerclesMax) and (random(100) < 50) then
    AjouteUnCercle;
end;

initialization

randomize;

{$IFDEF DEBUG}
ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
