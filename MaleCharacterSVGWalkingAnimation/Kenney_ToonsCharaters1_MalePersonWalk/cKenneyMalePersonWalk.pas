unit cKenneyMalePersonWalk;

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

type
  TcadKenneyMalePersonWalk = class(TFrame)
    Image1: TImage;
    Timer1: TTimer;
    procedure Image1Resized(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCoinID: Integer;
    FLoop: boolean;
    FInverse: boolean;
    FDuration: single;
    FAutoReverse: boolean;
    FAutoReverseAndInverDone: boolean;
    FIsStarted: boolean;
    procedure SetCoinID(const Value: Integer);
    procedure SetAutoReverse(const Value: boolean);
    procedure SetDuration(const Value: single);
    procedure SetInverse(const Value: boolean);
    procedure SetLoop(const Value: boolean);
    procedure SetIsStarted(const Value: boolean);
  protected
  public
    property CoinID: Integer read FCoinID write SetCoinID;
    property AutoReverse: boolean read FAutoReverse write SetAutoReverse;
    property Duration: single read FDuration write SetDuration;
    property Inverse: boolean read FInverse write SetInverse;
    property Loop: boolean read FLoop write SetLoop;
    property IsStarted: boolean read FIsStarted write SetIsStarted;
    procedure RepaintCoin; virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap,
  USVGKenneyToonsCharaters1MalePersonWalk;

procedure TcadKenneyMalePersonWalk.AfterConstruction;
begin
  inherited;
  tthread.forcequeue(nil,
    procedure
    begin
      if FIsStarted then
      begin
        FIsStarted := false;
        Start;
      end
      else
      begin
        FIsStarted := true;
        Stop;
      end;
    end);
end;

constructor TcadKenneyMalePersonWalk.Create(AOwner: TComponent);
begin
  inherited;
  FCoinID := 0;
  FLoop := true;
  FInverse := false;
  FDuration := 0.2;
  FAutoReverse := false;
  FAutoReverseAndInverDone := false;
  FIsStarted := true;
  Timer1.enabled := false;
end;

procedure TcadKenneyMalePersonWalk.Image1Resized(Sender: TObject);
begin
  if not(csLoading in componentstate) then
    RepaintCoin;
end;

procedure TcadKenneyMalePersonWalk.RepaintCoin;
var
  ID: Integer;
begin
  ID := FCoinID + TSVGKenneyToonsCharaters1MalePersonWalk.tag;
  Image1.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(ID, round(Image1.width),
    round(Image1.height), Image1.Bitmap.BitmapScale));
end;

procedure TcadKenneyMalePersonWalk.SetAutoReverse(const Value: boolean);
begin
  FAutoReverse := Value;
end;

procedure TcadKenneyMalePersonWalk.SetCoinID(const Value: Integer);
begin
  FCoinID := Value;

  if (FCoinID < 0) then
    FCoinID := -FCoinID;
  if (FCoinID >= TSVGKenneyToonsCharaters1MalePersonWalk.Count) then
    FCoinID := FCoinID mod TSVGKenneyToonsCharaters1MalePersonWalk.Count;

  RepaintCoin;
end;

procedure TcadKenneyMalePersonWalk.SetDuration(const Value: single);
begin
  FDuration := Value;
  Timer1.Interval :=
    round(FDuration * 1000 / TSVGKenneyToonsCharaters1MalePersonWalk.Count);
end;

procedure TcadKenneyMalePersonWalk.SetInverse(const Value: boolean);
begin
  FInverse := Value;
end;

procedure TcadKenneyMalePersonWalk.SetIsStarted(const Value: boolean);
begin
  if (FIsStarted <> Value) then
  begin
    FIsStarted := Value;
    if FIsStarted then
      Start
    else
      Stop
  end;
end;

procedure TcadKenneyMalePersonWalk.SetLoop(const Value: boolean);
begin
  FLoop := Value;
end;

procedure TcadKenneyMalePersonWalk.Start;
begin
  if IsStarted then
    exit;

  IsStarted := true;
  if Inverse then
    FCoinID := TSVGKenneyToonsCharaters1MalePersonWalk.Count
  else
    FCoinID := -1;
  Timer1.Interval :=
    round(FDuration * 1000 / TSVGKenneyToonsCharaters1MalePersonWalk.Count);
  Timer1.enabled := true;
end;

procedure TcadKenneyMalePersonWalk.Stop;
begin
  if not IsStarted then
    exit;

  IsStarted := false;
  Timer1.enabled := false;
end;

procedure TcadKenneyMalePersonWalk.Timer1Timer(Sender: TObject);
var
  NewCoinID: Integer;
begin
  if (not IsStarted) or (not visible) then
    exit;

  case Inverse of
    false:
      begin
        NewCoinID := FCoinID + 1;
        if (NewCoinID >= TSVGKenneyToonsCharaters1MalePersonWalk.Count) then
          if Loop then
          begin
            if AutoReverse then
              if FAutoReverseAndInverDone then
                FAutoReverseAndInverDone := false
              else
              begin
                Inverse := not Inverse;
                FAutoReverseAndInverDone := true;
              end;
            NewCoinID := 0;
          end
          else
          begin
            CoinID := 0;
            IsStarted := false;
            exit;
          end;
      end;
    true:
      begin
        NewCoinID := FCoinID - 1;
        if (NewCoinID < 0) then
          if Loop then
          begin
            if AutoReverse then
              if FAutoReverseAndInverDone then
                FAutoReverseAndInverDone := false
              else
              begin
                Inverse := not Inverse;
                FAutoReverseAndInverDone := true;
              end;
            NewCoinID := TSVGKenneyToonsCharaters1MalePersonWalk.Count - 1;
          end
          else
          begin
            IsStarted := false;
            exit;
          end;
      end;
  end;

  CoinID := NewCoinID;
end;

initialization

TSVGKenneyToonsCharaters1MalePersonWalk.tag :=
  TOlfSVGBitmapList.AddItem(SVGKenneyToonsCharaters1MalePersonWalk);

end.