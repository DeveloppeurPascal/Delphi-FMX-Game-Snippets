unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts;

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
  public
    { Déclarations publiques }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses uMusicLoop, System.ioutils;

procedure TForm4.btnChoisirFichierMP3Click(Sender: TObject);
begin
  if OpenDialog1.Execute and tfile.exists(OpenDialog1.filename) and
    (tpath.GetExtension(OpenDialog1.filename).ToLower = '.mp3') then
  begin
    musicloop.Play(OpenDialog1.filename);
    Memo1.lines.Insert(0, 'Lecture de ' + OpenDialog1.filename);
  end;
end;

procedure TForm4.btnPlayClick(Sender: TObject);
begin
  if not musicloop.IsPlaying then
  begin
    musicloop.Play;
    Memo1.lines.Insert(0, 'Play');
  end;
end;

procedure TForm4.btnStopClick(Sender: TObject);
begin
  if musicloop.IsPlaying then
  begin
    musicloop.stop;
    Memo1.lines.Insert(0, 'Stop');
  end;
end;

end.
