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
  File last update : 2025-05-29T15:23:14.000+02:00
  Signature : f150ebbbdb9395c520ade5d6801e617ab1500ea9
  ***************************************************************************
*)

unit cMatch3Game;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects;

const
  CEmptyItem = 255;

type
{$SCOPEDENUMS ON}
  TMatch3GamePhase = (None, FillFirstLineAndMove, PlayerChoice, CheckMatch3);

  TOnMatch3Event = procedure(const Nb, Item: integer) of object;
  TOnMatch3Proc = reference to procedure(const Nb, Item: integer);
  TOnMoveButNoMatch3Event = procedure of object;
  TOnMoveButNoMatch3Proc = reference to procedure;

  TcadMatch3Game = class(TFrame)
    GameLoop: TTimer;
    GameScene: TImage;
    procedure GameLoopTimer(Sender: TObject);
    procedure FrameResized(Sender: TObject);
    procedure GameSceneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure GameSceneMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
    procedure GameSceneMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure GameSceneMouseLeave(Sender: TObject);
  private
    FNbRow: integer;
    FNbCol: integer;
    FItems: array of string;
    FBackgroundColor: TAlphaColor;
    FSelectedBackgroundColor: TAlphaColor;
    FOnMoveButNoMatch3Event: TOnMoveButNoMatch3Event;
    FOnMatch3Event: TOnMatch3Event;
    FOnMoveButNoMatch3Proc: TOnMoveButNoMatch3Proc;
    FOnMatch3Proc: TOnMatch3Proc;
    function GetSVGItems(Index: integer): string;
    procedure SetItems(Index: integer; const Value: string);
    procedure SetNbCol(const Value: integer);
    procedure SetNbRow(const Value: integer);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetSelectedBackgroundColor(const Value: TAlphaColor);
    procedure SetOnMatch3Event(const Value: TOnMatch3Event);
    procedure SetOnMatch3Proc(const Value: TOnMatch3Proc);
    procedure SetOnMoveButNoMatch3Event(const Value: TOnMoveButNoMatch3Event);
    procedure SetOnMoveButNoMatch3Proc(const Value: TOnMoveButNoMatch3Proc);
  protected
    FIsInitialized: boolean;
    FGrid: array of array of integer;
    FStatus: TMatch3GamePhase;
    FNeedARepaint: boolean;
    FSelectedCol, FSelectedRow: integer;
    FPaintedBlocSize: integer;
    FIsMouseDown: boolean;
    FCheckMatch3AfterUserMove: boolean;
    procedure Repaint(const Force: boolean = false);
    function MoveItems: boolean;
    function FillFirstLine: boolean;
    function HadAMatch3: boolean;
  public
    property NbCol: integer read FNbCol write SetNbCol;
    property NbRow: integer read FNbRow write SetNbRow;
    property SVGItems[Index: integer]: string read GetSVGItems write SetItems;
    property BackgroundColor: TAlphaColor read FBackgroundColor
      write SetBackgroundColor;
    property SelectedBackgroundColor: TAlphaColor read FSelectedBackgroundColor
      write SetSelectedBackgroundColor;
    property OnMatch3Event: TOnMatch3Event read FOnMatch3Event
      write SetOnMatch3Event;
    property OnMatch3Proc: TOnMatch3Proc read FOnMatch3Proc
      write SetOnMatch3Proc;
    property OnMoveButNoMatch3Event: TOnMoveButNoMatch3Event
      read FOnMoveButNoMatch3Event write SetOnMoveButNoMatch3Event;
    property OnMoveButNoMatch3Proc: TOnMoveButNoMatch3Proc
      read FOnMoveButNoMatch3Proc write SetOnMoveButNoMatch3Proc;
    procedure Clear;
    procedure Initialize;
    procedure StartGame;
    procedure StopGame;
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure FitInParent;
  end;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap,
  USVGMatch3Items;

{ TcadMatch3Game }

procedure TcadMatch3Game.AfterConstruction;
begin
  inherited;
  GameLoop.Enabled := True;
end;

procedure TcadMatch3Game.Clear;
begin
  FIsInitialized := false;
  FNbCol := 7;
  FNbRow := 5;
  SetLength(FItems, 0);
  SetLength(FGrid, 0);
  FStatus := TMatch3GamePhase.None;
  FNeedARepaint := false;
  FBackgroundColor := TAlphaColors.Darkslategrey;
  FSelectedBackgroundColor := TAlphaColors.Lightslategrey;
  FSelectedCol := 0;
  FSelectedRow := 0;
  FPaintedBlocSize := 0;
  FIsMouseDown := false;
  FCheckMatch3AfterUserMove := false;
end;

constructor TcadMatch3Game.Create(AOwner: TComponent);
begin
  inherited;
  FOnMoveButNoMatch3Event := nil;
  FOnMoveButNoMatch3Proc := nil;
  FOnMatch3Event := nil;
  FOnMatch3Proc := nil;
  Clear;
end;

function TcadMatch3Game.FillFirstLine: boolean;
var
  Col: integer;
begin
  result := false;
  for Col := 1 to FNbCol do
    if FGrid[Col][1] = CEmptyItem then
    begin
      FGrid[Col][1] := random(length(FItems));
      result := True;
    end;
end;

procedure TcadMatch3Game.FitInParent;
var
  W, H: Single;
  BlocSize: integer;
begin
  if (parent is TControl) then
  begin
    W := (parent as TControl).Width / FNbCol;
    H := (parent as TControl).Height / FNbRow;
  end
  else if (parent is TCustomForm) then
  begin
    W := (parent as TCustomForm).Width / FNbCol;
    H := (parent as TCustomForm).Height / FNbRow;
  end
  else
    exit;

  if (W < H) then
    BlocSize := trunc(W)
  else
    BlocSize := trunc(H);

  BeginUpdate;
  try
    Width := BlocSize * FNbCol;
    Height := BlocSize * FNbRow;
  finally
    EndUpdate;
  end;
  FNeedARepaint := True;
end;

procedure TcadMatch3Game.FrameResized(Sender: TObject);
begin
  Repaint(True);
end;

procedure TcadMatch3Game.GameLoopTimer(Sender: TObject);
var
  HasMoved, HasFilled: boolean;
begin
  case FStatus of
    TMatch3GamePhase.FillFirstLineAndMove:
      begin
        HasMoved := MoveItems;
        HasFilled := FillFirstLine;
        if HasMoved or HasFilled then
          FNeedARepaint := True
        else
        begin
          FCheckMatch3AfterUserMove := false;
          FStatus := TMatch3GamePhase.CheckMatch3;
          // TODO : add a "move" animation state
        end;
      end;
    TMatch3GamePhase.PlayerChoice:
      ;
    TMatch3GamePhase.CheckMatch3:
      if HadAMatch3 then
        FStatus := TMatch3GamePhase.FillFirstLineAndMove
        // TODO : add a "match" animation state
      else
      begin
        if FCheckMatch3AfterUserMove then
        begin
          if assigned(FOnMoveButNoMatch3Event) then
            FOnMoveButNoMatch3Event;
          if assigned(FOnMoveButNoMatch3Proc) then
            FOnMoveButNoMatch3Proc;
        end;
        FStatus := TMatch3GamePhase.PlayerChoice;
      end;
  end;
  Repaint;
end;

procedure TcadMatch3Game.GameSceneMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Col, Row: integer;
  SwapItem: integer;
begin
  if FStatus <> TMatch3GamePhase.PlayerChoice then
    exit;

  FIsMouseDown := True;

  Col := trunc(X / FPaintedBlocSize) + 1;
  Row := trunc(Y / FPaintedBlocSize) + 1;

  if (Col = FSelectedCol) and (Row = FSelectedRow) then
  begin // Unselect current selected item
    FSelectedCol := 0;
    FSelectedRow := 0;
    FNeedARepaint := True;
  end
  else if (FSelectedCol > 0) and (FSelectedRow > 0) and
    (((Col in [FSelectedCol - 1, FSelectedCol + 1]) and (Row = FSelectedRow)) or
    ((Row in [FSelectedRow - 1, FSelectedRow + 1]) and (Col = FSelectedCol)))
  then
  begin // Clicked on an adjacent item, try to swap them

    // TODO : test in the movement is allowed to have a classic behaviour

    // Move even if no match-3 is available
    SwapItem := FGrid[Col][Row];
    FGrid[Col][Row] := FGrid[FSelectedCol][FSelectedRow];
    FGrid[FSelectedCol][FSelectedRow] := SwapItem;
    FSelectedCol := 0;
    FSelectedRow := 0;
    FNeedARepaint := True;
    FCheckMatch3AfterUserMove := True;
    FStatus := TMatch3GamePhase.CheckMatch3;
  end
  else
  begin // Select a new item
    FSelectedCol := Col;
    FSelectedRow := Row;
    FNeedARepaint := True;
  end;
end;

procedure TcadMatch3Game.GameSceneMouseLeave(Sender: TObject);
begin
  FIsMouseDown := false;
end;

procedure TcadMatch3Game.GameSceneMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  Col, Row: integer;
  SwapItem: integer;
begin
  if not FIsMouseDown then
    exit;

  if FStatus <> TMatch3GamePhase.PlayerChoice then
    exit;

  Col := trunc(X / FPaintedBlocSize) + 1;
  Row := trunc(Y / FPaintedBlocSize) + 1;

  if (FSelectedCol > 0) and (FSelectedRow > 0) and
    (((Col in [FSelectedCol - 1, FSelectedCol + 1]) and (Row = FSelectedRow)) or
    ((Row in [FSelectedRow - 1, FSelectedRow + 1]) and (Col = FSelectedCol)))
  then
  begin // Clicked on an adjacent item, try to swap them

    // TODO : test in the movement is allowed to have a classic behaviour

    // Move even if no match-3 is available (manage a lives number)
    SwapItem := FGrid[Col][Row];
    FGrid[Col][Row] := FGrid[FSelectedCol][FSelectedRow];
    FGrid[FSelectedCol][FSelectedRow] := SwapItem;
    FSelectedCol := 0;
    FSelectedRow := 0;
    FNeedARepaint := True;
    FCheckMatch3AfterUserMove := True;
    FStatus := TMatch3GamePhase.CheckMatch3;
    FIsMouseDown := false;
  end;
end;

procedure TcadMatch3Game.GameSceneMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := false;
end;

function TcadMatch3Game.GetSVGItems(Index: integer): string;
begin
  if (length(FItems) < index + 1) then
    result := FItems[index]
  else
    result := '';
end;

function TcadMatch3Game.HadAMatch3: boolean;
  function NbItems(const Col, Row: integer; const Item: integer): integer;
  begin
    if (FGrid[Col][Row] = Item) then
    begin
      FGrid[Col][Row] := FGrid[Col][Row] + length(FItems);
      result := 1 + NbItems(Col - 1, Row, Item) + NbItems(Col + 1, Row, Item) +
        NbItems(Col, Row - 1, Item) + NbItems(Col, Row + 1, Item);
    end
    else
      result := 0;
  end;
  procedure ResetItems(const Col, Row: integer);
  begin
    if (FGrid[Col][Row] <> CEmptyItem) and (FGrid[Col][Row] >= length(FItems))
    then
    begin
      FGrid[Col][Row] := FGrid[Col][Row] - length(FItems);
      ResetItems(Col - 1, Row);
      ResetItems(Col + 1, Row);
      ResetItems(Col, Row - 1);
      ResetItems(Col, Row + 1);
    end;
  end;
  procedure DestroyItems(const Col, Row: integer);
  begin
    if (FGrid[Col][Row] <> CEmptyItem) and (FGrid[Col][Row] >= length(FItems))
    then
    begin
      FGrid[Col][Row] := CEmptyItem;
      // TODO : add a "destroy" animation somewhere
      DestroyItems(Col - 1, Row);
      DestroyItems(Col + 1, Row);
      DestroyItems(Col, Row - 1);
      DestroyItems(Col, Row + 1);
    end;
  end;

var
  Col, Row: integer;
  Item: integer;
  Nb: integer;
begin
  result := false;
  for Col := 1 to NbCol do
    for Row := 1 to NbRow do
      if (FGrid[Col][Row] < length(FItems)) and
        (((FGrid[Col + 1][Row] = FGrid[Col][Row]) and
        (FGrid[Col + 2][Row] = FGrid[Col][Row])) or
        ((FGrid[Col][Row + 1] = FGrid[Col][Row]) and
        (FGrid[Col][Row + 2] = FGrid[Col][Row]))) then
      begin
        Item := FGrid[Col][Row];
        Nb := NbItems(Col, Row, Item);
        if Nb < 3 then
          ResetItems(Col, Row)
        else
        begin
          result := True;
          FNeedARepaint := True;
          if assigned(FOnMatch3Event) then
            FOnMatch3Event(Nb, Item);
          if assigned(FOnMatch3Proc) then
            FOnMatch3Proc(Nb, Item);
          DestroyItems(Col, Row);
        end;
      end;
end;

procedure TcadMatch3Game.Initialize;
var
  Col, Row: integer;
begin
  FIsInitialized := True;

  if FNbCol < 5 then
    raise Exception.Create('Need at least 5 columns.');

  if FNbRow < 5 then
    raise Exception.Create('Need at least 5 rows.');

  if (length(FItems) < 5) then
    raise Exception.Create('Need at least 5 items.');

  SetLength(FGrid, FNbCol + 2);
  for Col := 0 to FNbCol + 1 do
  begin
    SetLength(FGrid[Col], FNbRow + 2);
    for Row := 0 to FNbRow + 1 do
      // TODO : use a different item value for borders and empty cell if needed
      FGrid[Col][Row] := CEmptyItem;
  end;

  FSelectedCol := 0;
  FSelectedRow := 0;

  FNeedARepaint := True;
  FStatus := TMatch3GamePhase.None;
  Repaint;
end;

function TcadMatch3Game.MoveItems: boolean;
var
  Col, Row: integer;
begin
  result := false;
  // TODO : add a "move by pixels" animation
  for Col := 1 to FNbCol do
    for Row := FNbRow - 1 downto 1 do
      if (FGrid[Col][Row + 1] = CEmptyItem) then
      begin
        FGrid[Col][Row + 1] := FGrid[Col][Row];
        FGrid[Col][Row] := CEmptyItem;
        result := True;
      end;
end;

procedure TcadMatch3Game.Repaint(const Force: boolean);
var
  Col, Row: integer;
  X, Y: integer;
  W, H: Single;
  BMPCanvas: TCanvas;
  BackgroundBrush, SelectedBackgroundBrush: TBrush;
  BMP: TBitmap;
  Dest: TRectF;
begin
  if not FIsInitialized then
    exit;

  if FNeedARepaint or Force then
  begin
    FNeedARepaint := false;

    BackgroundBrush := TBrush.Create(TBrushKind.Solid, FBackgroundColor);
    try
      SelectedBackgroundBrush := TBrush.Create(TBrushKind.Solid,
        FSelectedBackgroundColor);
      try
        GameScene.BeginUpdate;
        try
          // Calculate bitmap real size in pixels
          // W := GameScene.Width * GameScene.Bitmap.BitmapScale / FNbCol;
          // H := GameScene.Height * GameScene.Bitmap.BitmapScale / FNbRow;
          W := GameScene.Width / FNbCol;
          H := GameScene.Height / FNbRow;
          if (W < H) then
            FPaintedBlocSize := trunc(W)
          else
            FPaintedBlocSize := trunc(H);
          GameScene.Bitmap.SetSize
            (trunc(FPaintedBlocSize * FNbCol * GameScene.Bitmap.BitmapScale),
            trunc(FPaintedBlocSize * FNbRow * GameScene.Bitmap.BitmapScale));

          // Draw the items on the bitmap
          BMPCanvas := GameScene.Bitmap.Canvas;
          BMPCanvas.BeginScene;
          try
            for Col := 1 to FNbCol do
            begin
              X := Col - 1;
              for Row := 1 to FNbRow do
              begin
                Y := Row - 1;
                Dest := TRectF.Create(X * FPaintedBlocSize,
                  Y * FPaintedBlocSize, X * FPaintedBlocSize + FPaintedBlocSize,
                  Y * FPaintedBlocSize + FPaintedBlocSize);
                if (Col = FSelectedCol) and (Row = FSelectedRow) then
                  BMPCanvas.FillRect(Dest, 1, SelectedBackgroundBrush)
                else
                  BMPCanvas.FillRect(Dest, 1, BackgroundBrush);
                if FGrid[Col][Row] < length(FItems) then
                begin
                  // TODO : use the Bitmap() method from TOlfSVGBitmapList to have a bitmap cache
                  BMP := SVGToBitmap(FPaintedBlocSize, FPaintedBlocSize,
                    FItems[FGrid[Col][Row]], GameScene.Bitmap.BitmapScale,
                    3, 3, 3, 3);
                  try
                    BMPCanvas.DrawBitmap(BMP, BMP.BoundsF, Dest, 1);
                  finally
                    BMP.Free;
                  end;
                end;
              end;
            end;
          finally
            BMPCanvas.EndScene;
          end;
        finally
          GameScene.EndUpdate;
        end;
      finally
        SelectedBackgroundBrush.Free;
      end;
    finally
      BackgroundBrush.Free;
    end;
  end;
end;

procedure TcadMatch3Game.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
end;

procedure TcadMatch3Game.SetItems(Index: integer; const Value: string);
begin
  if (length(FItems) < index + 1) then
    SetLength(FItems, index + 1);
  FItems[index] := Value;
end;

procedure TcadMatch3Game.SetNbCol(const Value: integer);
begin
  FNbCol := Value;
end;

procedure TcadMatch3Game.SetNbRow(const Value: integer);
begin
  FNbRow := Value;
end;

procedure TcadMatch3Game.SetOnMatch3Event(const Value: TOnMatch3Event);
begin
  FOnMatch3Event := Value;
end;

procedure TcadMatch3Game.SetOnMatch3Proc(const Value: TOnMatch3Proc);
begin
  FOnMatch3Proc := Value;
end;

procedure TcadMatch3Game.SetOnMoveButNoMatch3Event
  (const Value: TOnMoveButNoMatch3Event);
begin
  FOnMoveButNoMatch3Event := Value;
end;

procedure TcadMatch3Game.SetOnMoveButNoMatch3Proc
  (const Value: TOnMoveButNoMatch3Proc);
begin
  FOnMoveButNoMatch3Proc := Value;
end;

procedure TcadMatch3Game.SetSelectedBackgroundColor(const Value: TAlphaColor);
begin
  FSelectedBackgroundColor := Value;
end;

procedure TcadMatch3Game.StartGame;
begin
  FStatus := TMatch3GamePhase.FillFirstLineAndMove;
  GameLoop.Enabled := True;
end;

procedure TcadMatch3Game.StopGame;
begin
  FStatus := TMatch3GamePhase.None;
  GameLoop.Enabled := false;
end;

end.
