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
/// Signature : 36425f4b1a29016926217286e8bce6d4f9e2320a
/// ***************************************************************************
/// </summary>

unit uSpriteCercle;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, System.Generics.Collections;

type
  TSpriteCercle = class;
  TSpriteEvent = procedure(Sender: TSpriteCercle) of object;
{$SCOPEDENUMS ON}
  TSpriteTypeRebond = (Interieur, Exterieur);

  TSpriteCercle = class(TFrame)
    Circle1: TCircle;
    procedure FrameResize(Sender: TObject);
    procedure Circle1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    FonSpriteClic: TSpriteEvent;
    FonSpriteCollision: TSpriteEvent;
    Fvx: integer;
    Fvy: integer;
    FTypeRebond: TSpriteTypeRebond;
    FonSpriteRebond: TSpriteEvent;
    procedure SetonSpriteClic(const Value: TSpriteEvent);
    procedure SetonSpriteCollision(const Value: TSpriteEvent);
    procedure Setvx(const Value: integer);
    procedure Setvy(const Value: integer);
    procedure SetTypeRebond(const Value: TSpriteTypeRebond);
    function GetCentreDuCercle: TPointF;
    function GetRayon: single;
    procedure SetonSpriteRebond(const Value: TSpriteEvent);
    { Déclarations privées }
  protected
    CollisionDejaTraitee: boolean;
    procedure DoSpriteCollision;
    function getZoneDAffichageHeight: single;
    function getZoneDAffichageWidth: single;
  public
    constructor Create(AOwner: TComponent); override;
    { Déclarations publiques }
    property onSpriteClic: TSpriteEvent read FonSpriteClic
      write SetonSpriteClic;
    property onSpriteCollision: TSpriteEvent read FonSpriteCollision
      write SetonSpriteCollision;
    property onSpriteRebond: TSpriteEvent read FonSpriteRebond
      write SetonSpriteRebond;
    property vx: integer read Fvx write Setvx;
    property vy: integer read Fvy write Setvy;
    property Rayon: single read GetRayon;
    property CentreDuCercle: TPointF read GetCentreDuCercle;
    property TypeRebond: TSpriteTypeRebond read FTypeRebond write SetTypeRebond;
    procedure Bouge;
    procedure InverseDirection;
    function CheckCollisionCercle(Cercle: TSpriteCercle): boolean;
  end;

  TSpriteCercleList = TObjectList<TSpriteCercle>;

implementation

{$R *.fmx}
{ TFrame1 }

procedure TSpriteCercle.Bouge;
var
  CollisionDetectee: boolean;
  RebondDetecte: boolean;
begin
  position.Point := pointf(position.x + vx, position.y + vy);

  // test sortie écran
  RebondDetecte := false;
  if (TypeRebond = TSpriteTypeRebond.Exterieur) then
  begin
    if (position.x > getZoneDAffichageWidth) or (position.x + width < 0) then
    begin
      vx := -vx;
      RebondDetecte := false;
    end;
    if (position.y > getZoneDAffichageHeight) or (position.y + height < 0) then
    begin
      vy := -vy;
      RebondDetecte := false;
    end;
  end
  else
  begin
    if (position.x + width > getZoneDAffichageWidth) or (position.x < 0) then
    begin
      vx := -vx;
      RebondDetecte := false;
    end;
    if (position.y + height > getZoneDAffichageHeight) or (position.y < 0) then
    begin
      vy := -vy;
      RebondDetecte := false;
    end;
  end;
  if RebondDetecte and assigned(onSpriteRebond) then
    onSpriteRebond(self);

  // test collision
  CollisionDetectee := false;
  for var e in parent.Children do
    if (e is TSpriteCercle) and (e <> self) and
      CheckCollisionCercle(e as TSpriteCercle) then
    // (IntersectRect(self.BoundsRect, (e as TSpriteCercle).BoundsRect)) then
    begin
      CollisionDetectee := true;
      (e as TSpriteCercle).DoSpriteCollision;
    end;
  if CollisionDetectee then
    DoSpriteCollision;
end;

function TSpriteCercle.CheckCollisionCercle(Cercle: TSpriteCercle): boolean;
var
  ab, ac, bc: single;
begin
  ab := abs(CentreDuCercle.x - Cercle.CentreDuCercle.x);
  ac := abs(CentreDuCercle.y - Cercle.CentreDuCercle.y);
  bc := sqrt(sqr(ab) + sqr(ac));
  result := bc <= (Rayon + Cercle.Rayon);
end;

procedure TSpriteCercle.Circle1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if assigned(onSpriteClic) then
    onSpriteClic(self);
end;

constructor TSpriteCercle.Create(AOwner: TComponent);
begin
  inherited;
  name := '';
  CollisionDejaTraitee := false;
  vx := 0;
  vy := 0;
  TypeRebond := TSpriteTypeRebond.Interieur;
end;

procedure TSpriteCercle.DoSpriteCollision;
begin
  if CollisionDejaTraitee then
    exit;
  CollisionDejaTraitee := true;
  if assigned(onSpriteCollision) then
    onSpriteCollision(self);
end;

procedure TSpriteCercle.FrameResize(Sender: TObject);
begin
  if width <> height then
    height := width;
end;

function TSpriteCercle.GetCentreDuCercle: TPointF;
begin
  result.x := position.x + Rayon;
  result.y := position.y + Rayon;
end;

function TSpriteCercle.GetRayon: single;
begin
  result := width / 2; // ou height/2 (car cercle)
end;

function TSpriteCercle.getZoneDAffichageHeight: single;
begin
  if assigned(parent) and (parent is tcontrol) then
    result := (parent as tcontrol).height
  else if assigned(parent) and (parent is tcommoncustomform) then
    result := (parent as tcommoncustomform).clientheight
  else
    result := height;
end;

function TSpriteCercle.getZoneDAffichageWidth: single;
begin
  if assigned(parent) and (parent is tcontrol) then
    result := (parent as tcontrol).width
  else if assigned(parent) and (parent is tcommoncustomform) then
    result := (parent as tcommoncustomform).clientwidth
  else
    result := width;
end;

procedure TSpriteCercle.InverseDirection;
begin
  vx := -vx;
  vy := -vy;
end;

procedure TSpriteCercle.SetonSpriteClic(const Value: TSpriteEvent);
begin
  FonSpriteClic := Value;
end;

procedure TSpriteCercle.SetonSpriteCollision(const Value: TSpriteEvent);
begin
  FonSpriteCollision := Value;
end;

procedure TSpriteCercle.SetonSpriteRebond(const Value: TSpriteEvent);
begin
  FonSpriteRebond := Value;
end;

procedure TSpriteCercle.SetTypeRebond(const Value: TSpriteTypeRebond);
begin
  FTypeRebond := Value;
end;

procedure TSpriteCercle.Setvx(const Value: integer);
begin
  Fvx := Value;
end;

procedure TSpriteCercle.Setvy(const Value: integer);
begin
  Fvy := Value;
end;

end.
