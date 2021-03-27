program CanardPivot;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit9 in 'Unit9.pas' {Form9},
  uSpriteCanard in '..\AnimationSprite\uSpriteCanard.pas' {SpriteCanard: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
