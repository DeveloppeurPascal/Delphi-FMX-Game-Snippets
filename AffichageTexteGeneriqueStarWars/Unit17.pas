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
    procedure DeclencheLecture(fichier: string);
  public
    { Déclarations publiques }
  end;

var
  Form17: TForm17;

implementation

{$R *.fmx}

uses
  System.IOUtils, u_download, Gamolf.FMX.MusicLoop;

procedure TForm17.AnimationGeneriqueDemarrer;
var
  fichier: string;
begin
  // chemin Windows en dur, à adapter selon la plateforme
  txtGenerique.Width := zoneTexteGenerique2D.Width;
  // https://raw.githubusercontent.com/DeveloppeurPascal/DelphiFMXGameSnippets/main/README.md
  fichier := tpath.combine(tpath.GetTempPath, 'README.md');
  if not tfile.Exists(fichier) then
    tdownload_file.download
      ('https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets/raw/main/README.md',
      fichier,
      procedure
      begin
        DeclencheLecture(fichier);
      end,
      procedure
      begin
        showmessage(fichier + ' non trouvé');
      end)
  else
    DeclencheLecture(fichier);
end;

procedure TForm17.AnimationGeneriqueTerminee;
begin
  animTexteGenerique.Enabled := false;
  showmessage('Générique terminé.');
end;

procedure TForm17.animTexteGeneriqueTimer(Sender: TObject);
begin
  if (txtGenerique.Position.y > -txtGenerique.Height) then
    txtGenerique.Position.y := txtGenerique.Position.y - 5
    // txtGenerique.TextSettings.Font.Size / 5
  else
    AnimationGeneriqueTerminee;
end;

procedure TForm17.DeclencheLecture(fichier: string);
begin
  txtGenerique.Text := tfile.ReadAllText(fichier, TEncoding.UTF8);
  txtGenerique.AutoSize := true;
  txtGenerique.Position.y := zoneTexteGenerique2D.Height;
  zoneTexteGenerique2D.Visible := true;
  animTexteGenerique.Enabled := true;
end;

procedure TForm17.FormCreate(Sender: TObject);
var
  fichier: string;
begin
  zoneTexteGenerique2D.Visible := false;
  // https://www.soundboard.com/sb/sound/918028
  // => https://www.soundboard.com/track/download/918028
  //
  // http://soundfxcenter.com/download-sound/star-wars-main-theme-song/
  // => http://soundfxcenter.com/movies/star-wars/8d82b5_Star_Wars_Main_Theme_Song.mp3
  fichier := tpath.combine(tpath.GetTempPath, 'soundboard_sound_918028.mp3');
  if not tfile.Exists(fichier) then
    tdownload_file.download
      ('https://www.soundboard.com/track/download/918028', fichier,
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
begin
  musicloop.Play(FichierMP3);
end;

end.
