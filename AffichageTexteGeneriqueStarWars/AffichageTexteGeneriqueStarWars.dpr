program AffichageTexteGeneriqueStarWars;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit17 in 'Unit17.pas' {Form17},
  u_download in '..\..\Librairies\u_download.pas',
  uMusicLoop in '..\MusiqueDAmbiance\uMusicLoop.pas' {MusicLoop: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm17, Form17);
  Application.CreateForm(TMusicLoop, MusicLoop);
  Application.Run;
end.
