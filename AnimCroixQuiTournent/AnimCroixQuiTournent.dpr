program AnimCroixQuiTournent;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit19 in 'Unit19.pas' {Form19};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm19, Form19);
  Application.Run;
end.
