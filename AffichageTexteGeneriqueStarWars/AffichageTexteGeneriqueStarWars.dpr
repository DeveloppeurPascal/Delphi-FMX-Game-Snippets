program AffichageTexteGeneriqueStarWars;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit17 in 'Unit17.pas' {Form17},
  uMusicLoop in '..\MusiqueDAmbiance\uMusicLoop.pas' {MusicLoop: TDataModule},
  u_download in '..\lib-externes\librairies\u_download.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm17, Form17);
  Application.CreateForm(TMusicLoop, MusicLoop);
  Application.Run;
end.
