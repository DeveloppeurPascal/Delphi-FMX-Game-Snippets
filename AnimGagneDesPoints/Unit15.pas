unit Unit15;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Ani;

type
  TForm15 = class(TForm)
    Button1: TButton;
    lblScore: TLabel;
    animPointsAjoutes: TFloatAnimation;
    txtPointsAjoutes: TText;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure animPointsAjoutesFinish(Sender: TObject);
  private
    FScore: Integer;
    procedure SetScore(const Value: Integer);
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure AfficheScore;
    property Score: Integer read FScore write SetScore;
  end;

var
  Form15: TForm15;

implementation

{$R *.fmx}

procedure TForm15.AfficheScore;
begin
  lblScore.Text := 'Score : ' + FScore.ToString;
end;

procedure TForm15.Button1Click(Sender: TObject);
begin
  Score := Score + random(96) + 5; // 5 à ((96-1)+5) = 100 à chaque fois
end;

procedure TForm15.animPointsAjoutesFinish(Sender: TObject);
begin
  animPointsAjoutes.Enabled := false;
  txtPointsAjoutes.Visible := false;
  AfficheScore;
  Button1.Enabled := true;
end;

procedure TForm15.FormCreate(Sender: TObject);
begin
  txtPointsAjoutes.Visible := false;
  FScore := 0;
  AfficheScore;
end;

procedure TForm15.SetScore(const Value: Integer);
begin
  Button1.Enabled := false;
  FScore := Value;
  txtPointsAjoutes.Text := '+' + Value.ToString;
  txtPointsAjoutes.Position.Point :=
    pointf(Button1.Position.x + (Button1.Width - txtPointsAjoutes.Width) / 2,
    Button1.Position.y + (Button1.height - txtPointsAjoutes.height) / 2);
  txtPointsAjoutes.Visible := true;
  animPointsAjoutes.Start; // enabled := true
end;

end.
