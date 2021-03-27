unit uMusicLoop;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Media;

type
  TMusicLoop = class(TDataModule)
    audio: TMediaPlayer;
    audioCheck: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure audioCheckTimer(Sender: TObject);
  private
    FaudioOn: boolean;
    FaudioActif: boolean;
    procedure SetaudioActif(const Value: boolean);
    procedure SetaudioOn(const Value: boolean);
    { Déclarations privées }
    property audioActif: boolean read FaudioActif write SetaudioActif;
    property audioOn: boolean read FaudioOn write SetaudioOn;
  public
    { Déclarations publiques }
    procedure Play(Filename: string = '');
    procedure Stop;
    function IsPlaying: boolean;
    function IsActive: boolean;
  end;

var
  MusicLoop: TMusicLoop;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

uses System.IOUtils;
{ TMusicLoop }

procedure TMusicLoop.audioCheckTimer(Sender: TObject);
begin
  if (audioActif and audioOn) then
  begin
    if (audio.State = TMediaState.Stopped) then
    begin
      audio.CurrentTime := 0;
      audio.Play;
    end
    else if (audio.State = TMediaState.Playing) and
      (audio.CurrentTime >= audio.Duration) then
      audio.CurrentTime := 0;
  end;
end;

procedure TMusicLoop.DataModuleCreate(Sender: TObject);
begin
  audioCheck.Enabled := false;
  audioCheck.Interval := 100;
  FaudioOn := false;
  FaudioActif := false;
end;

function TMusicLoop.IsActive: boolean;
begin
  result := FaudioActif;
end;

function TMusicLoop.IsPlaying: boolean;
begin
  result := FaudioActif and FaudioOn;
end;

procedure TMusicLoop.Play(Filename: string);
begin
  if (not Filename.IsEmpty) and (tfile.Exists(Filename)) then
  begin
    audio.Filename := Filename;
    audioActif := true;
  end;
  audioOn := true;
end;

procedure TMusicLoop.SetaudioActif(const Value: boolean);
begin
  FaudioActif := Value;
  audioCheck.Enabled := Value;
end;

procedure TMusicLoop.SetaudioOn(const Value: boolean);
begin
  FaudioOn := Value;
  if audioActif then
    if Value then
      audio.Play
    else
      audio.Stop;
end;

procedure TMusicLoop.Stop;
begin
  audioOn := false;
end;

end.
