program MusiqueDAmbiance;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  Gamolf.FMX.MusicLoop in '..\lib-externes\FMXGameEngine\src\Gamolf.FMX.MusicLoop.pas' {MusicLoop: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
