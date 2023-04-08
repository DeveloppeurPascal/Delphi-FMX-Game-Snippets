program MusicLoopUnitTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit20 in 'Unit20.pas' {Form20},
  Gamolf.FMX.MusicLoop in '..\lib-externes\FMXGameEngine\src\Gamolf.FMX.MusicLoop.pas' {MusicLoop: TDataModule},
  u_download in '..\lib-externes\librairies\u_download.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm20, Form20);
  Application.Run;
end.
