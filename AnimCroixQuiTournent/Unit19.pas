unit Unit19;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, Radiant.Shapes, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm19 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { D�clarations priv�es }
    FirstTime: boolean;
    procedure FinRotation(Sender: TObject);
  public
    { D�clarations publiques }
  end;

const
  CTailleCroix = 100;

var
  Form19: TForm19;

implementation

{$R *.fmx}

procedure TForm19.FinRotation(Sender: TObject);
var
  croix: tradiantplus;
  Ani: tfloatanimation;
begin
  if (Sender is tfloatanimation) then
  begin
    Ani := (Sender as tfloatanimation);
    if (Ani.parent is tradiantplus) then
    begin
      croix := Ani.parent as tradiantplus;
      croix.TagFloat := 0;
      tthread.ForceQueue(nil,
        procedure
        begin
          Ani.free;
        end);
    end
    else
      raise exception.create('couac');
  end
  else
    raise exception.create('couic');
end;

procedure TForm19.FormCreate(Sender: TObject);
begin
  FirstTime := true;
end;

procedure TForm19.FormShow(Sender: TObject);
var
  croix: tradiantplus;
  x, y: single;
  XDeDepart: single;
  CouleurBlanche: boolean;
  CouleurBlancheAuDepart: boolean;
begin
  if FirstTime then
  begin
    FirstTime := false;
    y := -CTailleCroix * 2 / 3;
    XDeDepart := -CTailleCroix;
    CouleurBlancheAuDepart := true;
    while y < clientheight do
    begin
      x := XDeDepart;
      CouleurBlanche := CouleurBlancheAuDepart;
      while x < clientwidth do
      begin
        // background utilis� lors de la rotation
        croix := tradiantplus.create(Self);
        croix.parent := Self;
        croix.Position.Point := pointf(x, y);
        croix.BarSize.ScaleFactor := 0.33;
        croix.Width := CTailleCroix;
        croix.Height := CTailleCroix;
        if not CouleurBlanche then // background donc couleur invers�e
        begin
          croix.Fill.Color := talphacolors.white;
          croix.tag := 1; // couleur blanche / noire
        end
        else
        begin
          croix.Fill.Color := talphacolors.black;
          croix.tag := 0; // couleur blanche / noire
        end;
        croix.stroke.Kind := tbrushkind.None;
        // croix.stroke.Color := croix.Fill.Color;
        croix.TagFloat := -1; // ne doit pas bouger

        // croix qui subit la rotation
        croix := tradiantplus.create(Self);
        croix.parent := Self;
        croix.Position.Point := pointf(x, y);
        croix.BarSize.ScaleFactor := 0.33;
        croix.Width := CTailleCroix;
        croix.Height := CTailleCroix;
        if CouleurBlanche then
        begin
          croix.Fill.Color := talphacolors.white;
          croix.tag := 1; // couleur blanche / noire
        end
        else
        begin
          croix.Fill.Color := talphacolors.black;
          croix.tag := 0; // couleur blanche / noire
        end;
        croix.stroke.Kind := tbrushkind.None;
        // croix.stroke.Color := croix.Fill.Color;
        croix.TagFloat := 0; // animee ou pas

        // passage � la suivante sur la m�me ligne
        x := x + CTailleCroix * 5 / 3;
        CouleurBlanche := not CouleurBlanche;
      end;
      y := y + CTailleCroix * 1 / 3;
      CouleurBlancheAuDepart := not CouleurBlancheAuDepart;
      XDeDepart := XDeDepart + CTailleCroix * 2 / 3;
      while XDeDepart > 0 do
      begin
        XDeDepart := XDeDepart - CTailleCroix * 5 / 3;
        CouleurBlancheAuDepart := not CouleurBlancheAuDepart;
      end;
    end;
  end;
end;

procedure TForm19.Timer1Timer(Sender: TObject);
var
  croix: tradiantplus;
  obj: tfmxobject;
  Ani: tfloatanimation;
begin
  if childrencount > 0 then
  begin
    obj := Children[random(childrencount)];
    if obj is tradiantplus then
    begin
      croix := obj as tradiantplus;
      if croix.TagFloat = 0 then
      begin
        croix.TagFloat := 1;
        croix.BringToFront;
        Ani := tfloatanimation.create(Self);
        Ani.parent := croix;
        Ani.PropertyName := 'RotationAngle';
        Ani.StartFromCurrent := false;
        Ani.Duration := 0.5;
        Ani.StartValue := croix.RotationAngle;
        case croix.tag of
          0:
            Ani.StopValue := croix.RotationAngle + 90;
          1:
            Ani.StopValue := croix.RotationAngle - 90;
        end;
        Ani.OnFinish := FinRotation;
        Ani.Start;
      end;
    end;
  end;
end;

initialization

randomize;
{$IFDEF DEBUG}
ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
