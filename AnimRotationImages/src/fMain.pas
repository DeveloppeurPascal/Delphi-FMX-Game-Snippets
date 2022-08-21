unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts;

type
  TForm21 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Circle1: TCircle;
    Timer1: TTimer;
    Circle2: TCircle;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCircleActive: TCircle;
    procedure OnOffAnimation(Circle: TCircle);
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form21: TForm21;

implementation

{$R *.fmx}

procedure TForm21.Button1Click(Sender: TObject);
begin
  OnOffAnimation(Circle1);
end;

procedure TForm21.Button2Click(Sender: TObject);
begin
  OnOffAnimation(Circle2);
end;

procedure TForm21.FormCreate(Sender: TObject);
begin
  FCircleActive := nil;
  Circle1.Visible := false;
  Circle2.Visible := false;
  Timer1.Enabled := false;
end;

procedure TForm21.OnOffAnimation(Circle: TCircle);
begin
  if Timer1.Enabled and Circle.Visible then
  begin
    Timer1.Enabled := false;
    // FCircleActive.Visible := false;
    Circle.Visible := false;
  end
  else
  begin
    if assigned(FCircleActive) then
      FCircleActive.Visible := false;
    FCircleActive := Circle;
    FCircleActive.Visible := true;
    Timer1.Enabled := true;
  end;
end;

procedure TForm21.Timer1Timer(Sender: TObject);
begin
  FCircleActive.RotationAngle := FCircleActive.RotationAngle + 10;
  while (FCircleActive.RotationAngle > 359) do
    FCircleActive.RotationAngle := FCircleActive.RotationAngle - 360;
end;

end.
