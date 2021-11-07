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
