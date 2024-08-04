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
/// Signature : 3631f12fc4db12e58ab56ca5a5feb9d6a080c5c1
/// ***************************************************************************
/// </summary>

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
