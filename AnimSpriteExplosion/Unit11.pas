unit Unit11;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm11 = class(TForm)
    btnExploseCanard: TButton;
    Image1: TImage;
    BitmapListAnimation1: TBitmapListAnimation;
    btnAnimationSurBackgroundfiche: TButton;
    procedure btnExploseCanardClick(Sender: TObject);
    procedure BitmapListAnimation1Finish(Sender: TObject);
    procedure btnAnimationSurBackgroundficheClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form11: TForm11;

implementation

{$R *.fmx}

procedure TForm11.BitmapListAnimation1Finish(Sender: TObject);
begin
  BitmapListAnimation1.Enabled := false;
end;

procedure TForm11.btnExploseCanardClick(Sender: TObject);
begin
  BitmapListAnimation1.Enabled := true;
end;

procedure TForm11.btnAnimationSurBackgroundficheClick(Sender: TObject);
begin
  Form11.Fill.Kind := TBrushKind.Bitmap;
  Form11.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  BitmapListAnimation1.parent := Form11;
  BitmapListAnimation1.PropertyName := 'Fill.Bitmap.Bitmap';
  BitmapListAnimation1.Enabled := true;
  Form11.Transparency := true;
end;

end.
