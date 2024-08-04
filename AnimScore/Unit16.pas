/// <summary>
/// ***************************************************************************
///
/// Delphi FMX Game Snippets
///
/// Copyright 2021-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Examples of what is done when developing video games: sprite management,
/// background music, sound effects, animations, ...
///
/// Projects are developed under Delphi with its FireMonkey multiplatform
/// framework to run our projects under Windows, macOS, iOS, Android and Linux
/// from the same code base.
///
/// Not all images and musics used in this repository are free of charge.
/// Reuse them only if you have a license. They remain the property of their
/// respective authors and are only present in the programs for demo purposes.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://fmxgamesnippets.developpeur-pascal.fr
///
/// Project site :
///      https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets
///
/// ***************************************************************************
/// File last update : 07/07/2024 08:50:40
/// Signature : 003a14acf5076c8f81e56771b7f5671de41a2d80
/// ***************************************************************************
/// </summary>

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
