program DuckJoke;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit14 in 'Unit14.pas' {frmDuckJoke},
  uSpriteCanard in '..\AnimationSprite\uSpriteCanard.pas' {SpriteCanard: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDuckJoke, frmDuckJoke);
  Application.Run;
end.
