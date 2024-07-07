program AffichageTexteGeneriqueStarWars;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit17 in 'Unit17.pas' {Form17},
  u_download in '..\lib-externes\librairies\src\u_download.pas',
  Gamolf.FMX.MusicLoop in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.FMX.MusicLoop.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm17, Form17);
  Application.Run;
end.
