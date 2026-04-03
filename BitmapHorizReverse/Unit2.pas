(* C2PP
  ***************************************************************************

  Delphi FMX Game Snippets
  Copyright (c) 2021-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  https://codeberg.org/PatrickPremartin/Delphi-FMX-Game-Snippets

  ***************************************************************************
  File last update : 2025-10-25T20:59:46.608+02:00
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
