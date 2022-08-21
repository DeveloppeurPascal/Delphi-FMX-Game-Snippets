program AnimRotationImages;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form21};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm21, Form21);
  Application.Run;
end.

