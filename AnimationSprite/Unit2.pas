unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uSpriteCanard;

type
  TForm2 = class(TForm)
    SpriteCanard1: TSpriteCanard;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  SpriteCanard1.InitialiseZoneDeDeplacement;
  SpriteCanard1.BougeLeCanardDeGaucheADroite;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  SpriteCanard1.InitialiseZoneDeDeplacement;
end;

end.
