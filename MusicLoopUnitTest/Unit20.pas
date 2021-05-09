unit Unit20;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uMusicLoop,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm20 = class(TForm)
    swMusic: TSwitch;
    swSon: TSwitch;
    lblMusic: TLabel;
    lblSound: TLabel;
    tbMusic: TTrackBar;
    tbSon: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure swMusicSwitch(Sender: TObject);
    procedure swSonSwitch(Sender: TObject);
    procedure tbMusicTracking(Sender: TObject);
    procedure tbSonTracking(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Déclarations privées }
    MusicMP3: TMusicLoop;
    SonWAV: TMusicLoop;
    function getMusicLoop(FileName: string): TMusicLoop;
  public
    { Déclarations publiques }
    procedure LoadFileFromURL(URL: string; FileName: string;
      OnSuccessProc: TProc < string >= nil);
  end;

var
  Form20: TForm20;

implementation

{$R *.fmx}

uses
  System.IOUtils, u_download;

procedure TForm20.FormClose(Sender: TObject; var Action: TCloseAction);
var
  fichier: string;
begin
  fichier := MusicMP3.audio.FileName;
  if tfile.exists(fichier) then
    tfile.Delete(fichier);
  fichier := SonWAV.audio.FileName;
  if tfile.exists(fichier) then
    tfile.Delete(fichier);
end;

procedure TForm20.FormCreate(Sender: TObject);
begin
  lblSound.enabled := false;
  lblMusic.enabled := false;

  // son : https://opengameart.org/content/win-sound-effect
  // https://opengameart.org/sites/default/files/Win%20sound.wav
  LoadFileFromURL('https://opengameart.org/sites/default/files/Win%20sound.wav',
    'son.wav',
    procedure(FileName: string)
    begin
      SonWAV := getMusicLoop(FileName);
      lblSound.enabled := SonWAV.IsActive;
      tbSon.Value := SonWAV.Volume;
    end);

  // music : https://opengameart.org/content/battle-theme-b-for-rpg
  // https://opengameart.org/sites/default/files/battleThemeB.mp3
  LoadFileFromURL
    ('https://opengameart.org/sites/default/files/battleThemeB.mp3',
    'music.mp3',
    procedure(FileName: string)
    begin
      MusicMP3 := getMusicLoop(FileName);
      lblMusic.enabled := MusicMP3.IsActive;
      tbMusic.Value := MusicMP3.Volume;
    end);
end;

function TForm20.getMusicLoop(FileName: string): TMusicLoop;
begin
  result := TMusicLoop.Create(self);
  result.Load(FileName);
end;

procedure TForm20.LoadFileFromURL(URL, FileName: string;
OnSuccessProc: TProc<string>);
var
  fichier: string;
begin
  fichier := tpath.combine(tpath.GetTempPath, FileName);
  if not tfile.exists(fichier) then
    tdownload_file.download(URL, fichier,
      procedure
      begin
        if assigned(OnSuccessProc) then
          OnSuccessProc(fichier);
      end,
      procedure
      begin
        ShowMessage('Téléchargement non effectué.');
      end)
  else if assigned(OnSuccessProc) then
    OnSuccessProc(fichier);
end;

procedure TForm20.swMusicSwitch(Sender: TObject);
begin
  if assigned(MusicMP3) then
    if swMusic.IsChecked then
      MusicMP3.Play
    else
      MusicMP3.Stop;
end;

procedure TForm20.swSonSwitch(Sender: TObject);
begin
  if assigned(SonWAV) then
    if swSon.IsChecked then
      SonWAV.PlaySound
    else
      SonWAV.Stop;
end;

procedure TForm20.tbMusicTracking(Sender: TObject);
begin
  MusicMP3.Volume := round(tbMusic.Value);
end;

procedure TForm20.tbSonTracking(Sender: TObject);
begin
  SonWAV.Volume := round(tbSon.Value);
end;

end.
