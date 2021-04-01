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
