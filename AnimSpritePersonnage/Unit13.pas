unit Unit13;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Ani, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TForm13 = class(TForm)
    Rectangle1: TRectangle;
    Layout1: TLayout;
    Image1: TImage;
    animWalking: TBitmapListAnimation;
    animPositionX: TFloatAnimation;
    animJump: TBitmapListAnimation;
    btnSaute: TButton;
    Memo1: TMemo;
    procedure animPositionXFinish(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure animJumpFinish(Sender: TObject);
    procedure btnSauteClick(Sender: TObject);
    procedure animJumpProcess(Sender: TObject);
  private
    { Déclarations privées }
    procedure NinjaAvance;
  public
    { Déclarations publiques }
  end;

var
  Form13: TForm13;

implementation

{$R *.fmx}

procedure TForm13.animJumpFinish(Sender: TObject);
begin
  animJump.Enabled := false;
  animWalking.Enabled := true;
  Image1.Position.y := Image1.TagFloat;
end;

procedure TForm13.animJumpProcess(Sender: TObject);
var
  pourcent: single;
begin
  pourcent := animJump.CurrentTime * 100 / animJump.Duration;
  Memo1.lines.insert(0, pourcent.tostring + ' ' +
    animJump.AnimationCount.tostring + ' ' + animJump.CurrentTime.tostring + ' '
    + animJump.Duration.tostring);
  if (pourcent < 20) then
    Image1.Position.y := Image1.Position.y - 3
  else if (pourcent < 40) then
    Image1.Position.y := Image1.Position.y - 1.5
  else if (pourcent < 60) then
    Image1.Position.y := Image1.Position.y
  else if (pourcent < 80) then
    Image1.Position.y := Image1.Position.y + 1.5
  else
    Image1.Position.y := Image1.Position.y + 3;
end;

procedure TForm13.animPositionXFinish(Sender: TObject);
begin
  animPositionX.Enabled := false;
  if Image1.Position.x > width then
    Image1.Position.x := -Image1.width;
  NinjaAvance;
end;

procedure TForm13.btnSauteClick(Sender: TObject);
begin
  animWalking.Enabled := false;
  Image1.TagFloat := Image1.Position.y;
  animJump.Enabled := true;
end;

procedure TForm13.FormCreate(Sender: TObject);
begin
  NinjaAvance;
end;

procedure TForm13.NinjaAvance;
begin
  animPositionX.StartValue := Image1.Position.x;
  animPositionX.StopValue := Image1.Position.x + Image1.width / 3;
  animPositionX.Enabled := true;
end;

end.
