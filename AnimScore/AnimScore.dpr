program AnimScore;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit16 in 'Unit16.pas' {Form16};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm16, Form16);
  Application.Run;
end.
