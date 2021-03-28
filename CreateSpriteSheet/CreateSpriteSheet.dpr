program CreateSpriteSheet;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit10 in 'Unit10.pas' {Form10};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
