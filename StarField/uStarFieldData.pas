/// <summary>
/// ***************************************************************************
///
/// Delphi FMX Game Snippets
///
/// Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.
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
/// File last update : 2025-01-25T12:36:06.000+01:00
/// Signature : 7a68b552daf5b0444d89047db130639d03f75c3c
/// ***************************************************************************
/// </summary>

unit uStarFieldData;

interface

uses
  System.Generics.Collections;

type
  TStar = class(TObject)
  private
    FZ: integer;
    FX: integer;
    FY: integer;
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    procedure SetZ(const Value: integer);
  protected
  public
    property X: integer read FX write SetX;
    property Y: integer read FY write SetY;
    property Z: integer read FZ write SetZ;
    class function GetNewStar(const MaxX, MaxY, MaxZ: integer): TStar;
  end;

  TStarsList = class(TObjectList<TStar>)
  private
    FMaxX, FMaxY, FMaxZ: integer;
  protected
  public
    property MaxX: integer read FMaxX;
    property MaxY: integer read FMaxY;
    property MaxZ: integer read FMaxZ;
    constructor Create(const ANbStars: integer = 100;
      const AMaxX: integer = 500; const AMaxY: integer = 500;
      const AMaxZ: integer = 500); virtual;
    destructor Destroy; override;
    procedure Move(VX, VY, VZ: integer);
  end;

implementation

{ TStar }

class function TStar.GetNewStar(const MaxX, MaxY, MaxZ: integer): TStar;
begin
  result := TStar.Create;
  result.X := random(2 * MaxX) - MaxX;
  result.Y := random(2 * MaxY) - MaxY;
  result.Z := random(2 * MaxZ) - MaxZ;
end;

procedure TStar.SetX(const Value: integer);
begin
  FX := Value;
end;

procedure TStar.SetY(const Value: integer);
begin
  FY := Value;
end;

procedure TStar.SetZ(const Value: integer);
begin
  FZ := Value;
end;

{ TStarsList }

constructor TStarsList.Create(const ANbStars, AMaxX, AMaxY, AMaxZ: integer);
var
  i: integer;
begin
  inherited Create;

  FMaxX := AMaxX;
  FMaxY := AMaxY;
  FMaxZ := AMaxZ;

  for i := 1 to ANbStars do
    self.Add(TStar.GetNewStar(FMaxX, FMaxY, FMaxZ));
end;

destructor TStarsList.Destroy;
begin
  // TODO : à compléter ou supprimer
  inherited;
end;

procedure TStarsList.Move(VX, VY, VZ: integer);
var
  i: integer;
begin
  for i := 0 to self.Count - 1 do
  begin
    self[i].X := self[i].X - VX;
    while (self[i].X > FMaxX) do
      self[i].X := self[i].X - 2 * FMaxX;
    while (self[i].X < -FMaxX) do
      self[i].X := self[i].X + 2 * FMaxX;
    self[i].Y := self[i].Y - VY;
    while (self[i].Y > FMaxY) do
      self[i].Y := self[i].Y - 2 * FMaxY;
    while (self[i].Y < -FMaxY) do
      self[i].Y := self[i].Y + 2 * FMaxY;
    self[i].Z := self[i].Z - VZ;
    while (self[i].Z > FMaxZ) do
      self[i].Z := self[i].Z - 2 * FMaxZ;
    while (self[i].Z < -FMaxZ) do
      self[i].Z := self[i].Z + 2 * FMaxZ;
  end;
end;

end.
