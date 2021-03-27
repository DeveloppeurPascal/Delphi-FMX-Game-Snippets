unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Ani;

type
  TForm1 = class(TForm)
    Baton: TRectangle;
    Canard_VersLaDroite: TRectangle;
    Cible: TLayout;
    DeLaGaucheVersLaDroite: TFloatAnimation;
    DeLaDroiteVersLaGauche: TFloatAnimation;
    Canard_VersLaGauche: TRectangle;
    procedure DeLaDroiteVersLaGaucheFinish(Sender: TObject);
    procedure DeLaGaucheVersLaDroiteFinish(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure BougeLeCanardDeGaucheADroite;
    procedure BougeLeCanardDeDroiteAGauche;
    procedure InitialiseZoneDeDeplacement;
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DeLaDroiteVersLaGaucheFinish(Sender: TObject);
begin
  BougeLeCanardDeGaucheADroite;
end;

procedure TForm1.DeLaGaucheVersLaDroiteFinish(Sender: TObject);
begin
  BougeLeCanardDeDroiteAGauche;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitialiseZoneDeDeplacement;
  BougeLeCanardDeGaucheADroite;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  InitialiseZoneDeDeplacement;
end;

procedure TForm1.BougeLeCanardDeGaucheADroite;
begin
  DeLaDroiteVersLaGauche.Enabled := false;
  DeLaGaucheVersLaDroite.Enabled := true;
  Canard_VersLaGauche.Visible := DeLaDroiteVersLaGauche.Enabled;
  Canard_VersLaDroite.Visible := DeLaGaucheVersLaDroite.Enabled;
end;

procedure TForm1.BougeLeCanardDeDroiteAGauche;
begin
  DeLaGaucheVersLaDroite.Enabled := false;
  DeLaDroiteVersLaGauche.Enabled := true;
  Canard_VersLaGauche.Visible := DeLaDroiteVersLaGauche.Enabled;
  Canard_VersLaDroite.Visible := DeLaGaucheVersLaDroite.Enabled;
end;

procedure TForm1.InitialiseZoneDeDeplacement;
begin
  Cible.Position.y := height - Cible.height;
  DeLaGaucheVersLaDroite.StartValue := -Cible.Width;
  DeLaGaucheVersLaDroite.StopValue := Width;
  DeLaDroiteVersLaGauche.StartValue := DeLaGaucheVersLaDroite.StopValue;
  DeLaDroiteVersLaGauche.StopValue := DeLaGaucheVersLaDroite.StartValue;
end;

end.
