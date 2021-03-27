program MusiqueDAmbiance;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  uMusicLoop in 'uMusicLoop.pas' {MusicLoop: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TMusicLoop, MusicLoop);
  Application.Run;
end.
