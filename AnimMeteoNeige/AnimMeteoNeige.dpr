program AnimMeteoNeige;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit18 in 'Unit18.pas' {Form18},
  uSpriteCanard in '..\AnimationSprite\uSpriteCanard.pas' {SpriteCanard: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm18, Form18);
  Application.Run;
end.
