program MaleCharacterSVGWalkingAnimationSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {frmMain},
  Olf.Skia.SVGToBitmap in '..\..\lib-externes\librairies\src\Olf.Skia.SVGToBitmap.pas',
  USVGKenneyToonsCharaters1MalePersonWalk in '..\Kenney_ToonsCharaters1_MalePersonWalk\USVGKenneyToonsCharaters1MalePersonWalk.pas',
  cKenneyMalePersonWalk in '..\Kenney_ToonsCharaters1_MalePersonWalk\cKenneyMalePersonWalk.pas' {cadKenneyMalePersonWalk: TFrame};

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
