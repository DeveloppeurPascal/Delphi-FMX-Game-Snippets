unit Unit16;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts;

type
  TForm16 = class(TForm)
    lblScore: TLabel;
    btnAjouteDesPoints: TButton;
    btnEnleveDesPoints: TButton;
    Layout1: TLayout;
    animAffichageScore: TTimer;
    procedure btnAjouteDesPointsClick(Sender: TObject);
    procedure btnEnleveDesPointsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure animAffichageScoreTimer(Sender: TObject);
  private
    FScore: integer;
    FScoreAffiche: integer;
    procedure SetScore(const Value: integer);
    procedure SetScoreAffiche(const Value: integer);
    { Déclarations privées }
  public
    { Déclarations publiques }
    property Score: integer read FScore write SetScore;
    property ScoreAffiche: integer read FScoreAffiche write SetScoreAffiche;
  end;

var
  Form16: TForm16;

implementation

{$R *.fmx}

procedure TForm16.animAffichageScoreTimer(Sender: TObject);
begin
  if FScoreAffiche > FScore then
    ScoreAffiche := ScoreAffiche - 1
  else if FScoreAffiche < FScore then
    ScoreAffiche := ScoreAffiche + 1;
end;

procedure TForm16.btnAjouteDesPointsClick(Sender: TObject);
begin
  Score := Score + random(96) + 5; // ajoute de 5 à 100 points lors d'un clic
end;

procedure TForm16.btnEnleveDesPointsClick(Sender: TObject);
begin
  Score := Score - random(96) + 5; // retire de 5 à 100 points lors d'un clic
end;

procedure TForm16.FormCreate(Sender: TObject);
begin
  ScoreAffiche := 0;
  Score := 0;
end;

procedure TForm16.SetScore(const Value: integer);
begin
  FScore := Value;
end;

procedure TForm16.SetScoreAffiche(const Value: integer);
begin
  FScoreAffiche := Value;
  lblScore.Text := 'Score : ' + FScoreAffiche.ToString;
end;

end.
