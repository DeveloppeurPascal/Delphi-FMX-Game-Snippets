unit uDMSpriteSheets;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList, FMX.Graphics;

type
{$SCOPEDENUMS ON}
  TSpritesheetList = (base, interior, dungeon, characters, city, platformer);

  TDMSpriteSheets = class(TDataModule)
    SpriteSheetsList: TImageList;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function getSpriteSheet(ASpriteSheet: TSpritesheetList): TBitmap;
    function getImageFromSpriteSheet(ASpriteSheet: TSpritesheetList;
      AImageIndex: integer): TBitmap; overload;
    function getImageFromSpriteSheet(ASpriteSheet: TSpritesheetList;
      ASpriteSheetBitmap: TBitmap; AImageIndex: integer): TBitmap; overload;
  end;

var
  DMSpriteSheets: TDMSpriteSheets;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

uses
  System.Types;

function TDMSpriteSheets.getImageFromSpriteSheet(ASpriteSheet: TSpritesheetList;
  AImageIndex: integer): TBitmap;
var
  SpriteSheet: TBitmap;
begin
  SpriteSheet := getSpriteSheet(ASpriteSheet);
  try
    result := getImageFromSpriteSheet(ASpriteSheet, SpriteSheet, AImageIndex);
  finally
    SpriteSheet.Free;
  end;
end;

function TDMSpriteSheets.getImageFromSpriteSheet(ASpriteSheet: TSpritesheetList;
  ASpriteSheetBitmap: TBitmap; AImageIndex: integer): TBitmap;
var
  ssiWidth, ssiHeight, ssiMarginRight, ssiMarginBottom: integer;
  x, y: integer;
  ColCount, RowCount: integer;
begin
  result := nil;
  if (ASpriteSheetBitmap <> nil) then
  begin
    case ASpriteSheet of
      TSpritesheetList.base, TSpritesheetList.interior,
        TSpritesheetList.dungeon, TSpritesheetList.characters,
        TSpritesheetList.city:
        begin
          ssiWidth := 16;
          ssiHeight := 16;
          ssiMarginRight := 1;
          ssiMarginBottom := 1;
        end;
      TSpritesheetList.platformer:
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
      ColCount := (ASpriteSheetBitmap.Width + 1)
        div (ssiWidth + ssiMarginRight);
      RowCount := (ASpriteSheetBitmap.height + 1)
        div (ssiHeight + ssiMarginBottom);
    end;
    x := (AImageIndex mod ColCount) * (ssiWidth + ssiMarginRight);
    y := (AImageIndex div ColCount) * (ssiHeight + ssiMarginBottom);
    if (x < ASpriteSheetBitmap.Width) and (y < ASpriteSheetBitmap.height) then
    begin
      result := TBitmap.Create;
      result.SetSize(ssiWidth, ssiHeight);
      result.CopyFromBitmap(ASpriteSheetBitmap, trect.Create(x, y, x + ssiWidth,
        y + ssiHeight), 0, 0);
    end;
  end;
end;

function TDMSpriteSheets.getSpriteSheet(ASpriteSheet: TSpritesheetList)
  : TBitmap;
var
  ssName: string;
begin
  case ASpriteSheet of
    TSpritesheetList.base: // https://kenney.nl/assets/roguelike-rpg-pack
      ssName := 'base';
    TSpritesheetList.interior: // https://kenney.nl/assets/roguelike-indoors
      ssName := 'interior';
    TSpritesheetList.dungeon:
      // https://kenney.nl/assets/roguelike-caves-dungeons
      ssName := 'dungeon';
    TSpritesheetList.characters:
      // https://kenney.nl/assets/roguelike-characters
      ssName := 'characters';
    TSpritesheetList.city: // https://kenney.nl/assets/roguelike-modern-city
      ssName := 'city';
    TSpritesheetList.platformer: // https://kenney.nl/assets/pixel-platformer
      ssName := 'platformer';
  else
    ssName := '';
  end;
  if ssName.IsEmpty then
    result := nil
  else
  begin
    result := TBitmap.Create;
    result.Assign(SpriteSheetsList.Source.items
      [SpriteSheetsList.Source.IndexOf(ssName)].MultiResBitmap.Bitmaps[1]);
  end;
end;

end.
