unit Unit8;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Ani, Gamolf.FMX.MusicLoop;

type
  TForm8 = class(TForm)
    MonCanardQuiSouffre: TRectangle;
    animRotation: TFloatAnimation;
    animScale: TFloatAnimation;
    procedure animRotationFinish(Sender: TObject);
    procedure animScaleProcess(Sender: TObject);
    procedure animScaleFinish(Sender: TObject);
    procedure MonCanardQuiSouffreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    audioCouic: TMusicLoop;
  public
    { Déclarations publiques }
  end;

var
  Form8: TForm8;

implementation

{$R *.fmx}

procedure TForm8.animRotationFinish(Sender: TObject);
begin
  animRotation.enabled := false;
end;

procedure TForm8.animScaleFinish(Sender: TObject);
begin
  animScale.enabled := false;
end;

procedure TForm8.animScaleProcess(Sender: TObject);
begin
  MonCanardQuiSouffre.scale.y := MonCanardQuiSouffre.scale.x;
  MonCanardQuiSouffre.opacity :=
    1 - (MonCanardQuiSouffre.scale.x / animScale.StopValue);
  // TODO: déplacer le canard au prorata de sa position initiale
  // MonCanardQuiSouffre.Position.x := MonCanardQuiSouffre.Position.x -
  // (MonCanardQuiSouffre.Width / 2) * MonCanardQuiSouffre.scale.x;
  // MonCanardQuiSouffre.Position.y := MonCanardQuiSouffre.Position.y -
  // (MonCanardQuiSouffre.height / 2) * MonCanardQuiSouffre.scale.y;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  audioCouic := TMusicLoop.Create(Self);
{$IFDEF MSWINDOWS}
  audioCouic.Load('..\..\..\assets\TheGameCreators\SoundMatter\DuckyOuch.wav');
{$ENDIF}
end;

procedure TForm8.MonCanardQuiSouffreClick(Sender: TObject);
begin
  animRotation.enabled := true;
  animScale.enabled := true;
  audioCouic.PlaySound;
end;

end.
