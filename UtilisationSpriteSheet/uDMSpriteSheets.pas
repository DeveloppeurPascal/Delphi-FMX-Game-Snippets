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
/// File last update : 07/07/2024 08:50:40
/// Signature : 3a07376b0baffc0086aca2962110e662964eaef3
/// ***************************************************************************
/// </summary>

unit uDMSpriteSheets;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList, FMX.Graphics;

type
{$SCOPEDENUMS ON}
  TSpriteSheetName = (base, interior, dungeon, characters, city, platformer);

  TDMSpriteSheets = class(TDataModule)
    SpriteSheetsList: TImageList;
  private
    { Déclarations privées }
  public
    /// <summary>
    /// Retourne une image stockée dans la liste des images, chaque image correspondant à une spritesheet
    /// ATTENTION : il n'y a pas de copie de l'image, c'est directement celle de la liste des images, ne pas supprimer la bitmap reçue !
    /// </summary>
    function getSpriteSheetRef(ASpriteSheetName: TSpriteSheetName): TBitmap;

    /// <summary>
    /// Extrait le bitmap d'un sprite stocké dans une spritesheet
    /// </summary>
    function getImageFromSpriteSheet(ASpriteSheetName: TSpriteSheetName;
      AImageIndex: integer): TBitmap; overload;

    /// <summary>
    /// Extrait le bitmap d'un sprite stocké dans une spritesheet
    /// </summary>
    function getImageFromSpriteSheet(ASpriteSheetName: TSpriteSheetName;
      ASpriteSheetBitmap: TBitmap; AImageIndex: integer): TBitmap; overload;

    /// <summary>
    /// Extrait le bitmap d'un sprite stocké dans une spritesheet
    /// </summary>
    function getImageFromSpriteSheet(ASpriteSheetBitmap: TBitmap;
      AImageIndex, ASpriteWidth, ASpriteHeight, ASpriteMArginRight,
      ASpriteMArginBottom: integer): TBitmap; overload;

    /// <summary>
    /// Retourne le nombre d'éléments (= cases censées être des sprites) de la spritesheet spécifiée
    /// </summary>
    function getNbSprite(ASpriteSheetName: TSpriteSheetName): integer; overload;
    function getNbSprite(ASpriteSheetName: TSpriteSheetName;
      ASpriteSheetRef: TBitmap): integer; overload;

    /// <summary>
    /// Calcule le nombre de colonnes et de lignes disponibles dans la SpriteSheet spécifiée en fonction de la taille d'une sprite
    /// </summary>
    procedure getNbColAndNbRowFromSpriteSheet(ASpriteSheetName
      : TSpriteSheetName; out ColCount, RowCount: integer); overload;

    /// <summary>
    /// Calcule le nombre de colonnes et de lignes disponibles dans la SpriteSheet spécifiée en fonction de la taille d'une sprite
    /// </summary>
    procedure getNbColAndNbRowFromSpriteSheet(ASpriteSheetName
      : TSpriteSheetName; ASpriteSheetBitmap: TBitmap;
      out ColCount, RowCount: integer); overload;
  end;

var
  DMSpriteSheets: TDMSpriteSheets;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

uses
  System.Types, System.TypInfo;

{ TDMSpriteSheets }

function TDMSpriteSheets.getImageFromSpriteSheet(ASpriteSheetName
  : TSpriteSheetName; AImageIndex: integer): TBitmap;
begin
  result := getImageFromSpriteSheet(ASpriteSheetName,
    getSpriteSheetRef(ASpriteSheetName), AImageIndex);
end;

function TDMSpriteSheets.getImageFromSpriteSheet(ASpriteSheetName
  : TSpriteSheetName; ASpriteSheetBitmap: TBitmap;
  AImageIndex: integer): TBitmap;
var
  ssiWidth, ssiHeight, ssiMarginRight, ssiMarginBottom: integer;
begin
  result := nil;
  if (ASpriteSheetBitmap <> nil) then
  begin
    case ASpriteSheetName of
      TSpriteSheetName.base, TSpriteSheetName.interior,
        TSpriteSheetName.dungeon, TSpriteSheetName.characters,
        TSpriteSheetName.city:
        begin
          ssiWidth := 16;
          ssiHeight := 16;
          ssiMarginRight := 1;
          ssiMarginBottom := 1;
        end;
      TSpriteSheetName.platformer:
        begin
          ssiWidth := 18;
          ssiHeight := 18;
          ssiMarginRight := 2;
          ssiMarginBottom := 2;
        end;
    else
      ssiWidth := -1;
      ssiHeight := -1;
      ssiMarginRight := -1;
      ssiMarginBottom := -1;
    end;
    result := getImageFromSpriteSheet(ASpriteSheetBitmap, AImageIndex, ssiWidth,
      ssiHeight, ssiMarginRight, ssiMarginBottom);
  end;
end;

function TDMSpriteSheets.getImageFromSpriteSheet(ASpriteSheetBitmap: TBitmap;
  AImageIndex, ASpriteWidth, ASpriteHeight, ASpriteMArginRight,
  ASpriteMArginBottom: integer): TBitmap;
var
  x, y: integer;
  ColCount: integer;
begin
  result := nil;
  if (ASpriteSheetBitmap <> nil) then
  begin
    if ASpriteWidth > 0 then
    begin
      ColCount := (ASpriteSheetBitmap.Width + ASpriteMArginRight)
        div (ASpriteWidth + ASpriteMArginRight);
      // RowCount := (ASpriteSheetBitmap.height + ASpriteMArginBottom)
      // div (ASpriteHeight + ASpriteMArginBottom);
    end
    else
      ColCount := 0;
    x := (AImageIndex mod ColCount) * (ASpriteWidth + ASpriteMArginRight);
    y := (AImageIndex div ColCount) * (ASpriteHeight + ASpriteMArginBottom);
    if (x < ASpriteSheetBitmap.Width) and (y < ASpriteSheetBitmap.height) then
    begin
      result := TBitmap.Create;
      result.SetSize(ASpriteWidth, ASpriteHeight);
      result.CopyFromBitmap(ASpriteSheetBitmap, rect(x, y, x + ASpriteWidth,
        y + ASpriteHeight), 0, 0);
    end;
  end;
end;

procedure TDMSpriteSheets.getNbColAndNbRowFromSpriteSheet(ASpriteSheetName
  : TSpriteSheetName; out ColCount, RowCount: integer);
begin
  getNbColAndNbRowFromSpriteSheet(ASpriteSheetName,
    getSpriteSheetRef(ASpriteSheetName), ColCount, RowCount);
end;

procedure TDMSpriteSheets.getNbColAndNbRowFromSpriteSheet(ASpriteSheetName
  : TSpriteSheetName; ASpriteSheetBitmap: TBitmap;
  out ColCount, RowCount: integer);
var
  ssiWidth: integer;
  ssiHeight: integer;
  ssiMarginRight: integer;
  ssiMarginBottom: integer;
begin
  ColCount := 0;
  RowCount := 0;
  if (ASpriteSheetBitmap <> nil) then
  begin
    case ASpriteSheetName of
      TSpriteSheetName.base, TSpriteSheetName.interior,
        TSpriteSheetName.dungeon, TSpriteSheetName.characters,
        TSpriteSheetName.city:
        begin
          ssiWidth := 16;
          ssiHeight := 16;
          ssiMarginRight := 1;
          ssiMarginBottom := 1;
        end;
      TSpriteSheetName.platformer:
        begin
          ssiWidth := 18;
          ssiHeight := 18;
          ssiMarginRight := 2;
          ssiMarginBottom := 2;
        end;
    else
      ssiWidth := -1;
      ssiHeight := -1;
      ssiMarginRight := -1;
      ssiMarginBottom := -1;
    end;
    if ssiWidth > 0 then
    begin
      ColCount := (ASpriteSheetBitmap.Width + ssiMarginRight)
        div (ssiWidth + ssiMarginRight);
      RowCount := (ASpriteSheetBitmap.height + ssiMarginBottom)
        div (ssiHeight + ssiMarginBottom);
    end;
  end;
end;

function TDMSpriteSheets.getNbSprite(ASpriteSheetName
  : TSpriteSheetName): integer;
var
  ColCount, RowCount: integer;
begin
  getNbColAndNbRowFromSpriteSheet(ASpriteSheetName, ColCount, RowCount);
  result := ColCount * RowCount;
end;

function TDMSpriteSheets.getNbSprite(ASpriteSheetName: TSpriteSheetName;
  ASpriteSheetRef: TBitmap): integer;
var
  ColCount, RowCount: integer;
begin
  getNbColAndNbRowFromSpriteSheet(ASpriteSheetName, ASpriteSheetRef, ColCount,
    RowCount);
  result := ColCount * RowCount;
end;

function TDMSpriteSheets.getSpriteSheetRef(ASpriteSheetName
  : TSpriteSheetName): TBitmap;
var
  ssName: string;
begin
  case ASpriteSheetName of
    TSpriteSheetName.base, TSpriteSheetName.interior, TSpriteSheetName.dungeon,
      TSpriteSheetName.characters, TSpriteSheetName.city,
      TSpriteSheetName.platformer:
      ssName := GetEnumName(typeinfo(TSpriteSheetName), ord(ASpriteSheetName));
  else
    ssName := '';
  end;
  if ssName.IsEmpty then
    result := nil
  else
  begin
    result := SpriteSheetsList.Source.items
      [SpriteSheetsList.Source.IndexOf(ssName)].MultiResBitmap.Bitmaps[1];
  end;
end;

end.
