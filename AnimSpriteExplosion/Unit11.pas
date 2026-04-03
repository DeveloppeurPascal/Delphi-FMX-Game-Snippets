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
  File last update : 2025-10-25T20:59:46.576+02:00
  Signature : 4675a8eded67a5252b90ff2fa835a4750e41396c
  ***************************************************************************
*)

unit Unit11;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm11 = class(TForm)
    btnExploseCanard: TButton;
    Image1: TImage;
    BitmapListAnimation1: TBitmapListAnimation;
    btnAnimationSurBackgroundfiche: TButton;
    procedure btnExploseCanardClick(Sender: TObject);
    procedure BitmapListAnimation1Finish(Sender: TObject);
    procedure btnAnimationSurBackgroundficheClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form11: TForm11;

implementation

{$R *.fmx}

procedure TForm11.BitmapListAnimation1Finish(Sender: TObject);
begin
  BitmapListAnimation1.Enabled := false;
end;

procedure TForm11.btnExploseCanardClick(Sender: TObject);
begin
  BitmapListAnimation1.Enabled := true;
end;

procedure TForm11.btnAnimationSurBackgroundficheClick(Sender: TObject);
begin
  Form11.Fill.Kind := TBrushKind.Bitmap;
  Form11.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  BitmapListAnimation1.parent := Form11;
  BitmapListAnimation1.PropertyName := 'Fill.Bitmap.Bitmap';
  BitmapListAnimation1.Enabled := true;
  Form11.Transparency := true;
end;

end.
