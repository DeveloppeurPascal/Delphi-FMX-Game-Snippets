unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Ani;

type
  TForm6 = class(TForm)
    Vagues01: TRectangle;
    Vagues01Anim: TFloatAnimation;
    Vagues02: TRectangle;
    Vagues02Anim: TFloatAnimation;
    Vagues03: TRectangle;
    Vagues03Anim: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
    procedure InitVagues;
  public
    { Déclarations publiques }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.FormCreate(Sender: TObject);
begin
  InitVagues;
  Vagues01Anim.Enabled := true;
  Vagues02Anim.Enabled := true;
  Vagues03Anim.Enabled := true;
end;

procedure TForm6.FormResize(Sender: TObject);
begin
  InitVagues;
end;

procedure TForm6.InitVagues;
begin
  Vagues01.Position.y := height - Vagues01.height;
  Vagues01.Width := Width + 132;
  Vagues01.Position.x := 0;

  Vagues02.Position.y := height - Vagues01.height+20;
  Vagues02.Width := Width + 132;
  Vagues02.Position.x := 0;

  Vagues03.Position.y := height - Vagues01.height+40;
  Vagues03.Width := Width + 132;
  Vagues03.Position.x := 0;
end;

end.
