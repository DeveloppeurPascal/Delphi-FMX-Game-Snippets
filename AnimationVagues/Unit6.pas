(* C2PP
  ***************************************************************************

  Delphi FMX Game Snippets

  Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  Examples of what is done when developing video games: sprite management,
  background music, sound effects, animations, ...

  Projects are developed under Delphi with its FireMonkey multiplatform
  framework to run our projects under Windows, macOS, iOS, Android and Linux
  from the same code base.

  Not all images and musics used in this repository are free of charge.
  Reuse them only if you have a license. They remain the property of their
  respective authors and are only present in the programs for demo purposes.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://fmxgamesnippets.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets

  ***************************************************************************
  File last update : 2025-02-09T11:12:38.233+01:00
  Signature : 94b2e8b0e6d90dd288bab48fdaea49d03d286dfc
  ***************************************************************************
*)

unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Ani;

type
  TForm6 = class(TForm)
    Vagues01: TRectangle;
    Vagues01Anim: TFloatAnimation;
    Vagues02: TRectangle;
    Vagues02Anim: TFloatAnimation;
    Vagues03: TRectangle;
    Vagues03Anim: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
    procedure InitVagues;
  public
    { Déclarations publiques }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.FormCreate(Sender: TObject);
begin
  InitVagues;
  Vagues01Anim.Enabled := true;
  Vagues02Anim.Enabled := true;
  Vagues03Anim.Enabled := true;
end;

procedure TForm6.FormResize(Sender: TObject);
begin
  InitVagues;
end;

procedure TForm6.InitVagues;
begin
  Vagues01.Position.y := height - Vagues01.height;
  Vagues01.Width := Width + 132;
  Vagues01.Position.x := 0;

  Vagues02.Position.y := height - Vagues01.height+20;
  Vagues02.Width := Width + 132;
  Vagues02.Position.x := 0;

  Vagues03.Position.y := height - Vagues01.height+40;
  Vagues03.Width := Width + 132;
  Vagues03.Position.x := 0;
end;

end.
