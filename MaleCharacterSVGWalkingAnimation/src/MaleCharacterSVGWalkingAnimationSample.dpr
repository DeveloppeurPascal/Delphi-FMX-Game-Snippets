/// <summary>
/// ***************************************************************************
///
/// Delphi FMX Game Snippets
///
/// Copyright 2021-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Examples of what is done when developing video games: sprite management,
/// background music, sound effects, animations, ...
///
/// Projects are developed under Delphi with its FireMonkey multiplatform
/// framework to run our projects under Windows, macOS, iOS, Android and Linux
/// from the same code base.
///
/// Not all images and musics used in this repository are free of charge.
/// Reuse them only if you have a license. They remain the property of their
/// respective authors and are only present in the programs for demo purposes.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://fmxgamesnippets.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets
///
/// ***************************************************************************
/// File last update : 2024-08-13T12:11:24.000+02:00
/// Signature : e17df109c31c3c06d72433dfaa7c89ab4bb8e2e4
/// ***************************************************************************
/// </summary>

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
