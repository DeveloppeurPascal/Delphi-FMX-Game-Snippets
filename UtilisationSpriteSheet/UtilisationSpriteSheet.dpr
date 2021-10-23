program UtilisationSpriteSheet;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {frmMain},
  uDMSpriteSheets in 'uDMSpriteSheets.pas' {DMSpriteSheets: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TDMSpriteSheets, DMSpriteSheets);
  Application.Run;
end.
