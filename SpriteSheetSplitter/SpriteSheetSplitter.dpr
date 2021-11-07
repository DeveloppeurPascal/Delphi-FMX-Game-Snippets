program SpriteSheetSplitter;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {frmSpriteSheetSplitter};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSpriteSheetSplitter, frmSpriteSheetSplitter);
  Application.Run;
end.
