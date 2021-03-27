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

end.
