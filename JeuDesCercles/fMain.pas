unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uSpriteCercle;

const
  NbCerclesMax = 10;

type
  TfrmMain = class(TForm)
    TimerGenerateur: TTimer;
    TimerBoucleDuJeu: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerGenerateurTimer(Sender: TObject);
    procedure TimerBoucleDuJeuTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    CerclesEnJeu: TSpriteCercleList;
    procedure AjouteUnCercle;
    procedure ClicSurCercle(Cercle: TSpriteCercle);
    procedure CollisionSurCercle(Cercle: TSpriteCercle);
    procedure SupprimeLeCercle(Cercle: TSpriteCercle);
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.AjouteUnCercle;
var
  c: TSpriteCercle;
begin
  c := TSpriteCercle.Create(self);
  c.Parent := self;
  c.width := random(5) * 30 + 30;
  c.position.x := random(width);
  c.position.y := random(height);
  c.vx := random(11) - 5; // -5 à +5
  c.vy := random(11) - 5; // -5 à +5
  c.onSpriteClic := ClicSurCercle;
  c.onSpriteCollision := CollisionSurCercle;
  CerclesEnJeu.Add(c);
end;

procedure TfrmMain.TimerBoucleDuJeuTimer(Sender: TObject);
begin
  for var e in Children do
    if (e is TSpriteCercle) then
      (e as TSpriteCercle).Bouge;
end;

procedure TfrmMain.ClicSurCercle(Cercle: TSpriteCercle);
begin
  // TODO : traiter clic sur cercle
  SupprimeLeCercle(Cercle);
end;

procedure TfrmMain.CollisionSurCercle(Cercle: TSpriteCercle);
begin
  // TODO : traiter la collision
  SupprimeLeCercle(Cercle);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CerclesEnJeu := TSpriteCercleList.Create;
  TimerGenerateur.Enabled := true;
  TimerBoucleDuJeu.Enabled := true;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  CerclesEnJeu.Free;
end;

procedure TfrmMain.SupprimeLeCercle(Cercle: TSpriteCercle);
begin
  tthread.ForceQueue(nil,
    procedure
    begin
      CerclesEnJeu.Remove(Cercle);
    end);
end;

procedure TfrmMain.TimerGenerateurTimer(Sender: TObject);
begin
  if (CerclesEnJeu.Count < NbCerclesMax) and (random(100) < 50) then
    AjouteUnCercle;
end;

initialization

randomize;

{$IFDEF DEBUG}
ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
