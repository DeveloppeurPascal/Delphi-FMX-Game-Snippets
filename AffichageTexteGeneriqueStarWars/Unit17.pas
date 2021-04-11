unit Unit17;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Viewport3D,
  System.Math.Vectors, FMX.Ani, FMX.Objects, FMX.Controls3D, FMX.Layers3D,
  FMX.Layouts;

type
  TForm17 = class(TForm)
    Viewport3D1: TViewport3D;
    txtGenerique: TText;
    zoneTexteGenerique2D: TLayout;
    zoneTexteGenerique3D: TLayer3D;
    animTexteGenerique: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure animTexteGeneriqueTimer(Sender: TObject);
  private
    { Déclarations privées }
    procedure AnimationGeneriqueDemarrer;
    procedure AnimationGeneriqueTerminee;
    procedure LanceAudio(FichierMP3: string);
  public
    { Déclarations publiques }
  end;

var
  Form17: TForm17;

implementation

{$R *.fmx}

uses
  System.IOUtils, u_download, uMusicLoop;

procedure TForm17.AnimationGeneriqueDemarrer;
begin
  // chemin Windows en dur, à adapter selon la plateforme
  txtGenerique.Width := zoneTexteGenerique2D.Width;
  txtGenerique.Text := tfile.ReadAllText('..\..\..\README.md', TEncoding.UTF8);
  txtGenerique.AutoSize := true;
  txtGenerique.Position.y := zoneTexteGenerique2D.Height;
  zoneTexteGenerique2D.Visible := true;
  animTexteGenerique.Enabled := true;
end;

procedure TForm17.AnimationGeneriqueTerminee;
begin
  animTexteGenerique.Enabled := false;
  ShowMessage('Générique terminé.');
end;

procedure TForm17.animTexteGeneriqueTimer(Sender: TObject);
begin
  if (txtGenerique.Position.y > -txtGenerique.Height) then
    txtGenerique.Position.y := txtGenerique.Position.y - 5
    // txtGenerique.TextSettings.Font.Size / 5
  else
    AnimationGeneriqueTerminee;
end;

procedure TForm17.FormCreate(Sender: TObject);
var
  fichier: string;
begin
  zoneTexteGenerique2D.Visible := false;
  // https://www.soundboard.com/sb/sound/918028
  fichier := 'soundboard_sound_918028.mp3';
  if not tfile.Exists(fichier) then
    tdownload_file.download
      ('https://www.soundboard.com/handler/DownLoadTrack.ashx?cliptitle=Imperial+March&filename=22/227558-6ea81a03-cbc1-4d18-bdd6-9b031c6752ab.mp3',
      fichier,
      procedure
      begin
        LanceAudio(fichier);
      end)
  else
    LanceAudio(fichier);
end;

procedure TForm17.FormShow(Sender: TObject);
begin
  if not animTexteGenerique.Enabled then
    AnimationGeneriqueDemarrer;
end;

procedure TForm17.LanceAudio(FichierMP3: string);
var
  MusicDeFond: TMusicLoop;
begin
  MusicDeFond := TMusicLoop.Create(self);
  MusicDeFond.Play(FichierMP3);
end;

end.
