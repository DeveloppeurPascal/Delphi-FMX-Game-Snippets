program EffetsSonores;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit5 in 'Unit5.pas' {Form5},
  uMusicLoop in '..\MusiqueDAmbiance\uMusicLoop.pas' {MusicLoop: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TMusicLoop, MusicLoop);
  Application.Run;
end.
