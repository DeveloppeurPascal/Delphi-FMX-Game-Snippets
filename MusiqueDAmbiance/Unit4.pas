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
///      Patrick PREMARTIN
///
/// Site :
///      https://fmxgamesnippets.developpeur-pascal.fr
///
/// Project site :
///      https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets
///
/// ***************************************************************************
/// File last update : 07/07/2024 08:50:40
/// Signature : fb03b939bf951ab443a467ce45034601f1a0fe8e
/// ***************************************************************************
/// </summary>

unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, Gamolf.FMX.MusicLoop;

type
  TForm4 = class(TForm)
    btnChoisirFichierMP3: TButton;
    btnPlay: TButton;
    btnStop: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Layout1: TLayout;
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnChoisirFichierMP3Click(Sender: TObject);
  private
    { Déclarations privées }
  public
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses System.ioutils;

procedure TForm4.btnChoisirFichierMP3Click(Sender: TObject);
begin
  if OpenDialog1.Execute and tfile.exists(OpenDialog1.filename) and
    (tpath.GetExtension(OpenDialog1.filename).ToLower = '.mp3') then
  begin
    MusicLoop.Play(OpenDialog1.filename);
    Memo1.lines.Insert(0, 'Lecture de ' + OpenDialog1.filename);
  end;
end;

procedure TForm4.btnPlayClick(Sender: TObject);
begin
  if not MusicLoop.IsPlaying then
  begin
    MusicLoop.Play;
    Memo1.lines.Insert(0, 'Play');
  end;
end;

procedure TForm4.btnStopClick(Sender: TObject);
begin
  if MusicLoop.IsPlaying then
  begin
    MusicLoop.stop;
    Memo1.lines.Insert(0, 'Stop');
  end;
end;

end.
