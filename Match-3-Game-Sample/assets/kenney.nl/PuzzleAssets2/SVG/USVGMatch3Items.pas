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
  File last update : 2025-05-18T19:38:30.427+02:00
  Signature : c444c7a1563b87bcca3510cbcc9b3cb343515178
  ***************************************************************************
*)

unit USVGMatch3Items;

// ****************************************
// * SVG from folder :
// * /Users/patrickpremartin/Downloads/______Match3/assets/KenneyPuzzleAssets2Match3Items/Match3Items/uSVGMatch3Items.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit v1.0
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 2025-05-18T15:23:00.055Z
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGBubble = 0;
  CSVGDiamond = 1;
  CSVGHeart = 2;
  CSVGOctogon = 3;
  CSVGPentagram = 4;
  CSVGSquare = 5;
  CSVGStar4 = 6;
  CSVGStar5 = 7;
  CSVGTriangle = 8;

type
{$SCOPEDENUMS ON}
  TSVGMatch3ItemsIndex = (
    Bubble = CSVGBubble,
    Diamond = CSVGDiamond,
    Heart = CSVGHeart,
    Octogon = CSVGOctogon,
    Pentagram = CSVGPentagram,
    Square = CSVGSquare,
    Star4 = CSVGStar4,
    Star5 = CSVGStar5,
    Triangle = CSVGTriangle);

  TSVGMatch3Items = class
  private
  class var
    FTag: integer;
    FTagBool: Boolean;
    FTagFloat: Single;
    FTagObject: TObject;
    FTagString: string;
    class procedure SetTag(const Value: integer); static;
    class procedure SetTagBool(const Value: Boolean); static;
    class procedure SetTagFloat(const Value: Single); static;
    class procedure SetTagObject(const Value: TObject); static;
    class procedure SetTagString(const Value: string); static;
  public const
    Bubble = CSVGBubble;
    Diamond = CSVGDiamond;
    Heart = CSVGHeart;
    Octogon = CSVGOctogon;
    Pentagram = CSVGPentagram;
    Square = CSVGSquare;
    Star4 = CSVGStar4;
    Star5 = CSVGStar5;
    Triangle = CSVGTriangle;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGMatch3ItemsIndex) : string; overload;
    class function Count : Integer;
    class constructor Create;
  end;

var
  SVGMatch3Items : array of String;

implementation

uses
  System.SysUtils;

{ TSVGMatch3Items }

class constructor TSVGMatch3Items.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGMatch3Items.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGMatch3Items.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGMatch3Items.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGMatch3Items.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGMatch3Items.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGMatch3Items.SVG(const Index: Integer): string;
begin
  if (index < Count) then
    result := SVGMatch3Items[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGMatch3Items.SVG(const Index : TSVGMatch3ItemsIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGMatch3Items.Count: Integer;
begin
  result := length(SVGMatch3Items);
end;

initialization

SetLength(SVGMatch3Items, 9);

{$TEXTBLOCK NATIVE XML}
SVGMatch3Items[CSVGBubble] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 74 74"><g id="KenneyPuzzleAssets2"><g id="bubble"><path d="M59.6,14.4c-6.2-6.27-13.72-9.4-22.55-9.4h-.05c-8.87,0-16.4,3.13-22.6,9.4-6.27,6.27-9.4,13.8-9.4,22.6s3.12,16.38,9.35,22.65l.05.05c6.2,6.2,13.73,9.3,22.6,9.3h.05c8.83,0,16.35-3.1,22.55-9.3,6.27-6.27,9.4-13.83,9.4-22.7s-3.13-16.33-9.4-22.6" fill="#a34100"/><path d="M10.85,10.9C18.02,3.63,26.73,0,37,0h.05c10.23,0,18.93,3.63,26.1,10.9,7.23,7.23,10.85,15.93,10.85,26.1s-3.62,18.98-10.85,26.25c-7.2,7.17-15.9,10.75-26.1,10.75h-.05c-10.23,0-18.93-3.58-26.1-10.75l-.05-.05h-.05C3.6,55.93,0,47.2,0,37S3.63,18.13,10.9,10.9h-.05" fill="#a34100"/><path d="M20,54c-4.67-4.7-7-10.37-7-17s2.35-12.25,7.05-16.95h.05c4.63-4.7,10.27-7.05,16.9-7.05h.05c6.6,0,12.23,2.35,16.9,7.05,4.7,4.7,7.05,10.35,7.05,16.95s-2.35,12.35-7.05,17.05c-4.63,4.63-10.27,6.95-16.9,6.95h-.05c-6.67,0-12.32-2.32-16.95-6.95l-.05-.05" fill="#e86a17"/><path
d="M59.6,14.4l-5.65,5.65c-4.67-4.7-10.3-7.05-16.9-7.05h-.05c-6.63,0-12.27,2.35-16.9,7.05h-.05c-4.7,4.7-7.05,10.35-7.05,16.95s2.33,12.3,7,17l-5.65,5.65c-6.23-6.27-9.35-13.82-9.35-22.65s3.13-16.33,9.4-22.6c6.2-6.27,13.73-9.4,22.6-9.4h.05c8.83,0,16.35,3.13,22.55,9.4" fill="#f07625"/><path d="M53.95,20.05l5.65-5.65c6.27,6.27,9.4,13.8,9.4,22.6s-3.13,16.43-9.4,22.7c-6.2,6.2-13.72,9.3-22.55,9.3h-.05c-8.87,0-16.4-3.1-22.6-9.3l-.05-.05,5.65-5.65.05.05c4.63,4.63,10.28,6.95,16.95,6.95h.05c6.63,0,12.27-2.32,16.9-6.95,4.7-4.7,7.05-10.38,7.05-17.05s-2.35-12.25-7.05-16.95" fill="#c95b12"/></g></g></svg>
''';
SVGMatch3Items[CSVGDiamond] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 74 62"><g id="KenneyPuzzleAssets2"><g id="diamond"><polyline points="52.9 5 21.2 5 5 22.45 37 57 69 22.45 52.9 5" fill="#14516b"/><path d="M56.6,1.6l16.1,17.45c.87.97,1.3,2.1,1.3,3.4s-.43,2.43-1.3,3.4l-32,34.55c-.67.67-1.45,1.13-2.35,1.4s-1.8.27-2.7,0c-.9-.27-1.67-.73-2.3-1.4L1.35,25.85c-.9-.97-1.35-2.1-1.35-3.4s.45-2.43,1.35-3.4L17.55,1.6c.47-.5,1.02-.88,1.65-1.15.63-.3,1.3-.45,2-.45h31.7c.7,0,1.37.15,2,.45.63.27,1.2.65,1.7,1.15" fill="#14516b"/><polyline points="15.95 22.5 24.7 13 49.4 13 58.1 22.45 37 45.25 15.95 22.5" fill="#1ea7e1"/><polyline points="5 22.45 15.95 22.5 37 45.25 37 57 5 22.45" fill="#2db5ee"/><polyline points="49.4 13 52.9 5 69 22.45 58.1 22.45 49.4 13" fill="#2db5ee"/><polyline points="5 22.45 21.2 5 52.9 5 49.4 13 24.7 13 15.95 22.5 5 22.45" fill="#62d0ff"/><polyline points="69 22.45 37 57 37 45.25 58.1 22.45 69 22.45" fill="#1c82ad"/></g></g></svg>
''';
SVGMatch3Items[CSVGHeart] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 75 69"><g id="KenneyPuzzleAssets2"><g id="heart"><path d="M64.75,10.3c-3.47-3.57-7.6-5.33-12.4-5.3-5-.03-9.2,1.8-12.6,5.5l-.05.05c-.83.87-1.57,1.82-2.2,2.85l-2.25-2.9c-3.5-3.7-7.7-5.53-12.6-5.5-4.83-.03-8.97,1.73-12.4,5.3l-.2.2c-3.33,3.53-5.02,7.8-5.05,12.8,0,4.77,1.53,8.88,4.6,12.35l.2.2.2.2,14.05,14.95,11.35,12.05,1.3.8.8.15.75-.15,1.35-.8,25.4-27,.2-.2.2-.2.65-.75c2.63-3.3,3.95-7.17,3.95-11.6-.03-5-1.72-9.27-5.05-12.8l-.2-.2" fill="#932828"/><path d="M52.4,0c6.13-.03,11.43,2.23,15.9,6.8l.05.05.15.15.1.1c4.2,4.47,6.33,9.87,6.4,16.2-.03,5.57-1.68,10.43-4.95,14.6l-.1.15-1,1.15-.2.2-.1.1-25.4,27-.1.1c-.93.93-2.07,1.62-3.4,2.05h-.1l-1.5.3-.65.05-2.55-.45.25.1c-1.43-.47-2.58-1.18-3.45-2.15L6.45,39.6l-1-1.1C1.82,34.2,0,29.13,0,23.3c.07-6.33,2.22-11.73,6.45-16.2l.1-.1.2-.2-.1.05C11.08,2.25,16.42-.03,22.65,0h.05c5.67-.03,10.6,1.87,14.8,5.7C41.6,1.87,46.55-.03,52.35,0h.05"
fill="#932828"/><path d="M29.7,45.35l-13.85-14.75-.15-.15-.05-.05-.05-.05c-1.73-1.97-2.6-4.3-2.6-7v-.05c0-2.87.97-5.3,2.9-7.3h.05c1.8-2,4.02-3,6.65-3h.1c2.6,0,4.83.98,6.7,2.95l.05.05,1.35,1.75c1,1.57,2.38,2.65,4.15,3.25l2.55.45,2.75-.5c1.73-.67,3.1-1.78,4.1-3.35l1.15-1.5-.15.1.05-.05.25-.2c1.8-1.97,4.02-2.95,6.65-2.95h.1c2.6,0,4.83.98,6.7,2.95l.05.05c1.9,2,2.85,4.43,2.85,7.3v.05c0,2.7-.87,5.03-2.6,7l-.05.05-.15.2-21.7,23.05-7.8-8.3" fill="#c83e3e"/><path
d="M59.1,15.95c-1.87-1.97-4.1-2.95-6.7-2.95h-.1c-2.63,0-4.85.98-6.65,2.95l-.25.2-.05.05.15-.1-1.15,1.5c-1,1.57-2.37,2.68-4.1,3.35l-2.75.5-2.55-.45c-1.77-.6-3.15-1.68-4.15-3.25l-1.35-1.75-.05-.05c-1.87-1.97-4.1-2.95-6.7-2.95h-.1c-2.63,0-4.85,1-6.65,3h-.05c-1.93,2-2.9,4.43-2.9,7.3v.05c0,2.7.87,5.03,2.6,7l.05.05.05.05.15.15,13.85,14.75-5.65,5.65-14.05-14.95-.2-.2-.2-.2c-3.07-3.47-4.6-7.58-4.6-12.35.03-5,1.72-9.27,5.05-12.8l.2-.2c3.43-3.57,7.57-5.33,12.4-5.3,4.9-.03,9.1,1.8,12.6,5.5l2.25,2.9c.63-1.03,1.37-1.98,2.2-2.85l.05-.05c3.4-3.7,7.6-5.53,12.6-5.5,4.8-.03,8.93,1.73,12.4,5.3l-5.65,5.65" fill="#d44c4c"/><path d="M59.1,15.95l5.65-5.65.2.2c3.33,3.53,5.02,7.8,5.05,12.8,0,4.43-1.32,8.3-3.95,11.6l-.65.75-.2.2-.2.2-25.4,27-1.35.8-.75.15-.8-.15-1.3-.8-11.35-12.05,5.65-5.65,7.8,8.3,21.7-23.05.15-.2.05-.05c1.73-1.97,2.6-4.3,2.6-7v-.05c0-2.87-.95-5.3-2.85-7.3l-.05-.05" fill="#aa3030"/></g></g></svg>
''';
SVGMatch3Items[CSVGOctogon] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 74.05 74.05"><g id="KenneyPuzzleAssets2"><g id="octogon"><polyline points="37 69.03 59.6 59.72 69 37.13 59.6 14.42 37 5.03 14.3 14.42 5 37.13 14.3 59.72 37 69.03" fill="#c45a8f"/><path d="M64.25,12.53l9.4,22.7c.53,1.27.53,2.55,0,3.85l-9.4,22.6c-.57,1.27-1.48,2.17-2.75,2.7l-22.6,9.3c-1.27.5-2.53.5-3.8,0l-22.7-9.3c-1.27-.53-2.17-1.45-2.7-2.75L.4,39.03c-.53-1.27-.53-2.53,0-3.8L9.7,12.53c.5-1.27,1.4-2.17,2.7-2.7L35.1.42c1.27-.57,2.55-.57,3.85,0l22.6,9.4c1.27.53,2.17,1.43,2.7,2.7" fill="#c45a8f"/><polyline points="60.35 37.13 53.5 53.57 37 60.38 20.45 53.63 13.65 37.13 20.45 20.57 37 13.67 53.5 20.53 60.35 37.13" fill="#f9c"/><polyline points="37 13.67 20.45 20.57 13.65 37.13 5 37.13 14.3 14.42 37 5.03 37 13.67" fill="#ffd2e8"/><polyline points="37 60.38 37 69.03 14.3 59.72 5 37.13 13.65 37.13 20.45 53.63 37 60.38" fill="#ffaad4"/><polyline points="69 37.13 60.35 37.13 53.5 20.53 37
13.67 37 5.03 59.6 14.42 69 37.13" fill="#ffaad4"/><polyline points="37 60.38 53.5 53.57 60.35 37.13 69 37.13 59.6 59.72 37 69.03 37 60.38" fill="#f080b8"/></g></g></svg>
''';
SVGMatch3Items[CSVGPentagram] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 74.01 74"><g id="KenneyPuzzleAssets2"><g id="pentagram"><path d="M40.1,1.05l31.95,24.4c.8.63,1.37,1.45,1.7,2.45.33.97.35,1.97.05,3l-12.2,39.6c-.33,1.03-.95,1.88-1.85,2.55-.87.63-1.85.95-2.95.95H17.2c-1.1,0-2.08-.32-2.95-.95-.9-.67-1.5-1.52-1.8-2.55L.25,30.9c-.33-1-.33-2,0-3,.33-1,.92-1.82,1.75-2.45L34.05,1.05c.87-.7,1.87-1.05,3-1.05s2.12.35,3.05,1.05" fill="#932828"/><polyline points="37.05 5 5 29.4 17.2 69 56.8 69 69 29.4 37.05 5" fill="#932828"/><polyline points="14.3 32.4 37.05 15.05 59.75 32.4 50.9 61 23.1 61 14.3 32.4" fill="#c83e3e"/><polyline points="37.05 5 37.05 15.05 14.3 32.4 5 29.4 37.05 5" fill="#ee6b6b"/><polyline points="37.05 15.05 37.05 5 69 29.4 56.8 69 50.9 61 59.75 32.4 37.05 15.05" fill="#d44c4c"/><polyline points="5 29.4 14.3 32.4 23.1 61 17.2 69 5 29.4" fill="#d44c4c"/><polyline points="50.9 61 56.8 69 17.2 69 23.1 61 50.9 61" fill="#aa3030"/></g></g></svg>
''';
SVGMatch3Items[CSVGSquare] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 74 74"><g id="KenneyPuzzleAssets2"><g id="square"><path d="M72.55,40.55l-32,32c-1,.97-2.18,1.45-3.55,1.45s-2.53-.48-3.5-1.45L1.5,40.55C.5,39.55,0,38.37,0,37c0-1.37.5-2.53,1.5-3.5L33.5,1.5c.97-1,2.13-1.5,3.5-1.5s2.55.5,3.55,1.5l32,32c.97.97,1.45,2.13,1.45,3.5,0,1.37-.48,2.55-1.45,3.55" fill="#c97900"/><polyline points="5 37 37 69 69 37 37 5 5 37" fill="#c97900"/><polyline points="57.7 37 37 57.7 16.3 37 37 16.3 57.7 37" fill="#fc0"/><polyline points="69 37 57.7 37 37 16.3 37 5 69 37" fill="#ffd52e"/><polyline points="16.3 37 5 37 37 5 37 16.3 16.3 37" fill="#ffe57e"/><polyline points="57.7 37 69 37 37 69 5 37 16.3 37 37 57.7 57.7 37" fill="#ecbd00"/></g></g></svg>
''';
SVGMatch3Items[CSVGStar4] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 74.02 74.03"><g id="KenneyPuzzleAssets2"><g id="star4"><path d="M38.6.3c1.07.33,1.92.98,2.55,1.95l12.25,18.35,18.4,12.25c.93.63,1.58,1.48,1.95,2.55.37,1.07.37,2.13,0,3.2-.37,1.07-1.02,1.92-1.95,2.55l-18.4,12.25-12.25,18.4c-.63.93-1.48,1.58-2.55,1.95-1.07.37-2.13.37-3.2,0-1.07-.37-1.92-1.02-2.55-1.95l-12.25-18.4L2.25,41.15c-.97-.63-1.62-1.48-1.95-2.55-.4-1.07-.4-2.13,0-3.2.33-1.07.98-1.92,1.95-2.55l18.35-12.25L32.85,2.25c.63-.97,1.48-1.62,2.55-1.95,1.07-.4,2.13-.4,3.2,0" fill="#9a9a9a"/><polyline points="69 37 49.8 24.2 37 5 24.2 24.2 5 37 24.2 49.8 37 69 49.8 49.8 69 37" fill="#9a9a9a"/><polyline points="37 19.45 43.15 28.65 44.15 29.85 45.35 30.85 54.6 37 45.35 43.15 44.15 44.15 43.15 45.35 37 54.6 30.85 45.35 29.85 44.15 28.65 43.15 19.45 37 28.65 30.85 29.85 29.85 30.85 28.65 37 19.45" fill="#e5e5e5"/><polyline points="37 5 49.8 24.2 69 37 54.6 37 45.35 30.85 44.15 29.85 43.15
28.65 37 19.45 37 5" fill="#f0f0f0"/><polyline points="37 69 24.2 49.8 5 37 19.45 37 28.65 43.15 29.85 44.15 30.85 45.35 37 54.6 37 69" fill="#f0f0f0"/><polyline points="37 5 37 19.45 30.85 28.65 29.85 29.85 28.65 30.85 19.45 37 5 37 24.2 24.2 37 5" fill="#fff"/><polyline points="54.6 37 69 37 49.8 49.8 37 69 37 54.6 43.15 45.35 44.15 44.15 45.35 43.15 54.6 37" fill="#c9c9c9"/></g></g></svg>
''';
SVGMatch3Items[CSVGStar5] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 74 69.85"><g id="KenneyPuzzleAssets2"><g id="star5"><polyline points="5 27.95 16.3 44.85 17.2 65 37.05 59.65 56.75 65 57.75 44.85 69 27.95 49.75 20.85 37.05 5 24.2 20.85 5 27.95" fill="#44690b"/><path d="M.85,30.75C.28,29.88,0,28.93,0,27.9c0-1.03.32-1.97.95-2.8.57-.87,1.35-1.48,2.35-1.85l17.9-6.6L33.2,1.85c.63-.8,1.43-1.33,2.4-1.6.97-.33,1.95-.33,2.95,0,.97.3,1.77.85,2.4,1.65l11.85,14.75,17.95,6.6c.93.37,1.72.98,2.35,1.85.6.83.9,1.77.9,2.8,0,1-.28,1.95-.85,2.85l-10.45,15.7-.95,18.8c-.03,1-.37,1.92-1,2.75s-1.43,1.42-2.4,1.75l-2.9.1-18.4-5-18.55,5-2.9-.1c-.97-.37-1.77-.95-2.4-1.75-.63-.83-.97-1.75-1-2.75l-.85-18.8L.85,30.75" fill="#44690b"/><path
d="M24.85,54.65l-.1.05-.45-10.2-.35-2.05-1-2.05-5.65-8.45,9.7-3.6c.7-.27,1.35-.62,1.95-1.05l1.5-1.4,6.6-8.1,6.45,8.05,1.5,1.45,2,1.05,9.75,3.6-5.65,8.5c-.43.63-.75,1.3-.95,2-.23.63-.37,1.3-.4,2l-.5,10.25-.15-.05-9.95-2.7-2.1-.3-2.05.3-10.15,2.7" fill="#80be1f"/><path d="M17.3,31.95l-12.3-4,19.2-7.1,12.85-15.85v12.8l-6.6,8.1-1.5,1.4c-.6.43-1.25.78-1.95,1.05l-9.7,3.6" fill="#aadb5c"/><polyline points="17.2 65 16.3 44.85 5 27.95 17.3 31.95 22.95 40.4 23.95 42.45 24.3 44.5 24.75 54.7 24.85 54.65 17.2 65" fill="#8cc92c"/><path d="M37.05,5l12.7,15.85,19.25,7.1-11.25,16.9-1,20.15-7.65-10.35.15.05.5-10.25c.03-.7.17-1.37.4-2,.2-.7.52-1.37.95-2l5.65-8.5-9.75-3.6-2-1.05-1.5-1.45-6.45-8.05V5" fill="#8cc92c"/><polyline points="17.2 65 24.85 54.65 35 51.95 37.05 51.65 39.15 51.95 49.1 54.65 56.75 65 37.05 59.65 17.2 65" fill="#6da615"/></g></g></svg>
''';
SVGMatch3Items[CSVGTriangle] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 73.99 71.25"><g id="KenneyPuzzleAssets2"><g id="triangle"><path d="M41.58.05l31.9,64c.5,1.03.63,2.13.4,3.3-.27,1.13-.85,2.07-1.75,2.8-.93.73-1.98,1.1-3.15,1.1H4.98c-1.17,0-2.2-.37-3.1-1.1-.93-.77-1.52-1.72-1.75-2.85-.27-1.13-.13-2.23.4-3.3L32.63,0" fill="#a34100"/><polyline points="68.98 66.25 37.08 2.25 4.98 66.25 68.98 66.25" fill="#a34100"/><polyline points="37.08 20.15 56.08 58.25 17.93 58.25 37.08 20.15" fill="#e86a17"/><polyline points="37.08 20.15 37.08 2.25 68.98 66.25 56.08 58.25 37.08 20.15" fill="#f07625"/><polyline points="56.08 58.25 68.98 66.25 4.98 66.25 17.93 58.25 56.08 58.25" fill="#c95b12"/><polyline points="37.08 2.25 37.08 20.15 17.93 58.25 4.98 66.25 37.08 2.25" fill="#ff9048"/></g></g></svg>
''';

end.
