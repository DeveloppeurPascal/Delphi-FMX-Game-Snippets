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
  File last update : 2025-10-25T20:59:46.755+02:00
  Signature : a3dd8ac7368a9f63c5aa7edfa8a210f158c914d4
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TfrmMain = class(TForm)
    FlowLayout1: TFlowLayout;
    VertScrollBox1: TVertScrollBox;
    Layout1: TLayout;
    Label1: TLabel;
    Image1: TImage;
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ClickImage(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses uDMSpriteSheets;

procedure TfrmMain.ClickImage(Sender: TObject);
var
  bmp: tbitmap;
begin
  if Sender is TImage then
  begin
    Label1.Text := (Sender as TImage).Tag.ToString;
    bmp := DMSpriteSheets.getImageFromSpriteSheet(TSpriteSheetName.platformer,
      (Sender as TImage).Tag);
    try
      Image1.bitmap.setsize(bmp.Width, bmp.Height);
      Image1.bitmap.Assign(bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  SpriteSheet: tbitmap;
  img: TImage;
  ImgNum: integer;
  bmp: tbitmap;
begin
  SpriteSheet := DMSpriteSheets.getSpriteSheetRef(TSpriteSheetName.platformer);
  try
    ImgNum := 0;
    while true do
    begin
      bmp := DMSpriteSheets.getImageFromSpriteSheet(TSpriteSheetName.platformer,
        SpriteSheet, ImgNum);
      if (bmp = nil) then
        break
      else
        try
          img := TImage.Create(self);
          img.Parent := FlowLayout1;
          img.Width := 32;
          img.Height := 32;
          img.bitmap.setsize(bmp.Width, bmp.Height);
          img.bitmap.CopyFromBitmap(bmp, trect.Create(0, 0, bmp.Width,
            bmp.Height), 0, 0);
          img.Tag := ImgNum;
          img.OnClick := ClickImage;
        finally
          bmp.Free;
        end;
      inc(ImgNum);
    end;
  finally
    // SpriteSheet.Free; // Don't free it, it's a reference to the image in the TImageList, not a new bitmap !
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
