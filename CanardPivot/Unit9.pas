unit Unit9;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uSpriteCanard,
  FMX.Objects;

type
  TForm9 = class(TForm)
    SpriteCanard3: TSpriteCanard;
    SpriteCanard1: TSpriteCanard;
    SpriteCanard2: TSpriteCanard;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ClicSurCanard(CanardClique: TSpriteCanard);
    procedure ClicSurBaton(CanardClique: TSpriteCanard);
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

uses
  System.Threading, FMX.ani;

procedure TForm9.ClicSurBaton(CanardClique: TSpriteCanard);
begin
  if CanardClique.isEnMouvement then
    CanardClique.ImmobiliseLeCanard
  else
    CanardClique.BougeLeCanard;
end;

procedure TForm9.ClicSurCanard(CanardClique: TSpriteCanard);
const
  CDureeAnimation = 1; // en secondes
begin
  CanardClique.RotationCenter.x := 0.5;
  CanardClique.RotationCenter.y := 1;
  tanimator.AnimateFloat(CanardClique, 'rotationangle', 90, CDureeAnimation);
  ttask.run(
    procedure
    begin
      sleep(CDureeAnimation * 2000);
      tthread.ForceQueue(nil,
        procedure
        begin
          CanardClique.RotationAngle := 0;
          if random(100) < 50 then
          begin
            // TODO : position.X ne devrait pas avoir à être changée car nouvelle animation du canard
            // CanardClique.position.x := width;
            CanardClique.BougeLeCanardDeDroiteAGauche
          end
          else
          begin
            // CanardClique.position.x := -CanardClique.width;
            CanardClique.BougeLeCanardDeGaucheADroite;
          end;
        end);
    end);
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  SpriteCanard1.InitialiseZoneDeDeplacement;
  SpriteCanard1.Visible := false;
  SpriteCanard1.onClicSurCanard := ClicSurCanard;
  SpriteCanard1.onClicSurBaton := ClicSurBaton;
  SpriteCanard2.InitialiseZoneDeDeplacement;
  SpriteCanard2.Visible := false;
  SpriteCanard2.onClicSurCanard := ClicSurCanard;
  SpriteCanard2.onClicSurBaton := ClicSurBaton;
  SpriteCanard3.InitialiseZoneDeDeplacement;
  SpriteCanard3.Visible := false;
  SpriteCanard3.onClicSurCanard := ClicSurCanard;
  SpriteCanard3.onClicSurBaton := ClicSurBaton;
  ttask.run(
    procedure
    begin
      sleep(random(3000) + 100);
      tthread.Queue(nil,
        procedure
        begin
          SpriteCanard1.Visible := true;
          if random(100) < 50 then
            SpriteCanard1.BougeLeCanardDeDroiteAGauche
          else
            SpriteCanard1.BougeLeCanardDeGaucheADroite;
        end);
    end);
  ttask.run(
    procedure
    begin
      sleep(random(3000) + 100);
      tthread.Queue(nil,
        procedure
        begin
          SpriteCanard2.Visible := true;
          if random(100) < 50 then
            SpriteCanard2.BougeLeCanardDeDroiteAGauche
          else
            SpriteCanard2.BougeLeCanardDeGaucheADroite;
        end);
    end);
  ttask.run(
    procedure
    begin
      sleep(random(3000) + 100);
      tthread.Queue(nil,
        procedure
        begin
          SpriteCanard3.Visible := true;
          if random(100) < 50 then
            SpriteCanard3.BougeLeCanardDeDroiteAGauche
          else
            SpriteCanard3.BougeLeCanardDeGaucheADroite;
        end);
    end);
end;

procedure TForm9.FormResize(Sender: TObject);
begin
  SpriteCanard1.InitialiseZoneDeDeplacement;
  SpriteCanard2.InitialiseZoneDeDeplacement;
  SpriteCanard3.InitialiseZoneDeDeplacement;
end;

initialization

randomize;

end.
