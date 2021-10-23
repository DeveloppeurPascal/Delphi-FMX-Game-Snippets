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
    bmp := DMSpriteSheets.getImageFromSpriteSheet(TSpritesheetList.platformer,
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
  SpriteSheet := DMSpriteSheets.getSpritesheet(TSpritesheetList.platformer);
  try
    ImgNum := 0;
    while true do
    begin
      bmp := DMSpriteSheets.getImageFromSpriteSheet(TSpritesheetList.platformer,
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
    SpriteSheet.Free;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
