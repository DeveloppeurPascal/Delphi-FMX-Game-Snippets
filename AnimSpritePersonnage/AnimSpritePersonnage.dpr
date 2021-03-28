program AnimSpritePersonnage;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit13 in 'Unit13.pas' {Form13};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
