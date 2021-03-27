unit uSpriteCanard;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Objects, FMX.Layouts, System.Generics.Collections;

type
  tonGetDecalageHauteurCanard = function(x, y: single): integer of object;

  TSpriteCanard = class(TFrame)
    Cible: TLayout;
    Baton: TRectangle;
    Canard_VersLaDroite: TRectangle;
    DeLaGaucheVersLaDroite: TFloatAnimation;
    DeLaDroiteVersLaGauche: TFloatAnimation;
    Canard_VersLaGauche: TRectangle;
    procedure DeLaDroiteVersLaGaucheFinish(Sender: TObject);
    procedure DeLaGaucheVersLaDroiteFinish(Sender: TObject);
    procedure ActionPendantLeDeplacement(Sender: TObject);
  private
    FonGetDecalageHauteurCanard: tonGetDecalageHauteurCanard;
    PosY, DecalageY: single;
    function getZoneDAffichageHeight: single;
    function getZoneDAffichageWidth: single;
    procedure SetonGetDecalageHauteurCanard(const Value
      : tonGetDecalageHauteurCanard);
    property ZoneDAffichageWidth: single read getZoneDAffichageWidth;
    property ZoneDAffichageHeight: single read getZoneDAffichageHeight;
  public
    property onGetDecalageHauteurCanard: tonGetDecalageHauteurCanard
      read FonGetDecalageHauteurCanard write SetonGetDecalageHauteurCanard;
    constructor Create(AOwner: TComponent); override;
    procedure BougeLeCanardDeGaucheADroite;
    procedure BougeLeCanardDeDroiteAGauche;
    procedure InitialiseZoneDeDeplacement;
  end;

  TSpriteCanardList = TObjectList<TSpriteCanard>;

implementation

{$R *.fmx}
{ TFrame3 }

procedure TSpriteCanard.ActionPendantLeDeplacement(Sender: TObject);
begin
  if assigned(onGetDecalageHauteurCanard) then
  begin
    DecalageY := onGetDecalageHauteurCanard(Position.x, Position.y);
    Position.y := PosY + DecalageY;
  end;
end;

procedure TSpriteCanard.BougeLeCanardDeDroiteAGauche;
begin
  DeLaGaucheVersLaDroite.Enabled := false;
  DeLaDroiteVersLaGauche.Enabled := true;
  Canard_VersLaGauche.Visible := DeLaDroiteVersLaGauche.Enabled;
  Canard_VersLaDroite.Visible := DeLaGaucheVersLaDroite.Enabled;
end;

procedure TSpriteCanard.BougeLeCanardDeGaucheADroite;
begin
  DeLaDroiteVersLaGauche.Enabled := false;
  DeLaGaucheVersLaDroite.Enabled := true;
  Canard_VersLaGauche.Visible := DeLaDroiteVersLaGauche.Enabled;
  Canard_VersLaDroite.Visible := DeLaGaucheVersLaDroite.Enabled;
end;

constructor TSpriteCanard.Create(AOwner: TComponent);
begin
  inherited;
  name := '';
  width := Cible.width;
  Height := Cible.Height;
  DecalageY := 0;
end;

procedure TSpriteCanard.DeLaDroiteVersLaGaucheFinish(Sender: TObject);
begin
  BougeLeCanardDeGaucheADroite;
end;

procedure TSpriteCanard.DeLaGaucheVersLaDroiteFinish(Sender: TObject);
begin
  BougeLeCanardDeDroiteAGauche;
end;

function TSpriteCanard.getZoneDAffichageHeight: single;
begin
  if assigned(parent) and (parent is tcontrol) then
    result := (parent as tcontrol).Height
  else if assigned(parent) and (parent is tcommoncustomform) then
    result := (parent as tcommoncustomform).Height
  else
    result := Height;
end;

function TSpriteCanard.getZoneDAffichageWidth: single;
begin
  if assigned(parent) and (parent is tcontrol) then
    result := (parent as tcontrol).width
  else if assigned(parent) and (parent is tcommoncustomform) then
    result := (parent as tcommoncustomform).width
  else
    result := width;
end;

procedure TSpriteCanard.InitialiseZoneDeDeplacement;
begin
  PosY := ZoneDAffichageHeight - Cible.Height;
  Position.y := PosY + DecalageY;
  DeLaGaucheVersLaDroite.StartValue := -Cible.width;
  DeLaGaucheVersLaDroite.StopValue := ZoneDAffichageWidth;
  DeLaDroiteVersLaGauche.StartValue := DeLaGaucheVersLaDroite.StopValue;
  DeLaDroiteVersLaGauche.StopValue := DeLaGaucheVersLaDroite.StartValue;
end;

procedure TSpriteCanard.SetonGetDecalageHauteurCanard
  (const Value: tonGetDecalageHauteurCanard);
begin
  FonGetDecalageHauteurCanard := Value;
end;

end.
