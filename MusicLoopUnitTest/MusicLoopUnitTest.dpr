program MusicLoopUnitTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit20 in 'Unit20.pas' {Form20},
  u_download in '..\..\Librairies\u_download.pas',
  uMusicLoop in '..\MusiqueDAmbiance\uMusicLoop.pas' {MusicLoop: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm20, Form20);
  Application.CreateForm(TMusicLoop, MusicLoop);
  Application.Run;
end.
