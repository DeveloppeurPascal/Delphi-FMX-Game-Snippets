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
  File last update : 2025-02-09T11:12:38.244+01:00
  Signature : 093d1a3760c7e1ba984c83f10cdfe34222ac42aa
  ***************************************************************************
*)

unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ChangeHorizBitmap(FromBitmapFileName, ToBitmapFileName: string);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  System.IOUtils;

procedure TForm2.Button1Click(Sender: TObject);
begin
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00002.png',
    'C:\Users\olfso\Pictures\spritesheet-00008.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00003.png',
    'C:\Users\olfso\Pictures\spritesheet-00009.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00004.png',
    'C:\Users\olfso\Pictures\spritesheet-00010.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00011.png',
    'C:\Users\olfso\Pictures\spritesheet-00013.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00014.png',
    'C:\Users\olfso\Pictures\spritesheet-00012.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00015.png',
    'C:\Users\olfso\Pictures\spritesheet-00021.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00016.png',
    'C:\Users\olfso\Pictures\spritesheet-00022.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00017.png',
    'C:\Users\olfso\Pictures\spritesheet-00023.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00024.png',
    'C:\Users\olfso\Pictures\spritesheet-00026.png');
  ChangeHorizBitmap('C:\Users\olfso\Pictures\spritesheet-00025.png',
    'C:\Users\olfso\Pictures\spritesheet-00027.png');
  showmessage('Done !');
end;

procedure TForm2.ChangeHorizBitmap(FromBitmapFileName, ToBitmapFileName
  : string);
var
  bmpSource, bmpDest: tbitmap;
  i: integer;
begin
  if tfile.exists(FromBitmapFileName) then
  begin
    bmpSource := tbitmap.CreateFromFile(FromBitmapFileName);
    try
      bmpDest := tbitmap.Create(bmpSource.Width, bmpSource.Height);
      try
        for i := 0 to bmpSource.Height - 1 do
          bmpDest.CopyFromBitmap(bmpSource, rect(0, i, bmpSource.Width, i+1), 0,
            bmpDest.Height - i - 1);
        bmpDest.SaveToFile(ToBitmapFileName);
      finally
        bmpDest.Free;
      end;
    finally
      bmpSource.Free;
    end;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
