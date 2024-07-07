program MusicLoopUnitTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit20 in 'Unit20.pas' {Form20},
  u_download in '..\lib-externes\librairies\src\u_download.pas',
  Gamolf.FMX.MusicLoop in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.FMX.MusicLoop.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm20, Form20);
  Application.Run;
end.
