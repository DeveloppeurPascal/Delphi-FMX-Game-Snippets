program AnimationVaguesEtCanards;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit6 in '..\AnimationVagues\Unit6.pas' {Form6},
  Unit7 in 'Unit7.pas' {Form7},
  uSpriteCanard in '..\AnimationSprite\uSpriteCanard.pas' {SpriteCanard: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
