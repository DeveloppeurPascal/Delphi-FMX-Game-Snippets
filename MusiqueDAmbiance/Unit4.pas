unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, Gamolf.FMX.MusicLoop;

type
  TForm4 = class(TForm)
    btnChoisirFichierMP3: TButton;
    btnPlay: TButton;
    btnStop: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Layout1: TLayout;
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnChoisirFichierMP3Click(Sender: TObject);
  private
    { Déclarations privées }
    MusicLoop: TMusicLoop;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses System.ioutils;

procedure TForm4.btnChoisirFichierMP3Click(Sender: TObject);
begin
  if OpenDialog1.Execute and tfile.exists(OpenDialog1.filename) and
    (tpath.GetExtension(OpenDialog1.filename).ToLower = '.mp3') then
  begin
    MusicLoop.Play(OpenDialog1.filename);
    Memo1.lines.Insert(0, 'Lecture de ' + OpenDialog1.filename);
  end;
end;

procedure TForm4.btnPlayClick(Sender: TObject);
begin
  if not MusicLoop.IsPlaying then
  begin
    MusicLoop.Play;
    Memo1.lines.Insert(0, 'Play');
  end;
end;

procedure TForm4.btnStopClick(Sender: TObject);
begin
  if MusicLoop.IsPlaying then
  begin
    MusicLoop.stop;
    Memo1.lines.Insert(0, 'Stop');
  end;
end;

constructor TForm4.Create(AOwner: TComponent);
begin
  inherited;
  MusicLoop := TMusicLoop.Create(self);
end;

end.
