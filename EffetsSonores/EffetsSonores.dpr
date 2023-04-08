program EffetsSonores;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit5 in 'Unit5.pas' {Form5},
  Gamolf.FMX.MusicLoop in '..\lib-externes\FMXGameEngine\src\Gamolf.FMX.MusicLoop.pas' {MusicLoop: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
