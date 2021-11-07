unit Unit10;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm10 = class(TForm)
    ListBox1: TListBox;
    btnCreeSpriteSheet: TButton;
    Layout1: TLayout;
    Splitter1: TSplitter;
    VertScrollBox1: TVertScrollBox;
    FlowLayout1: TFlowLayout;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    cbSurUneLigne: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure btnCreeSpriteSheetClick(Sender: TObject);
  private
    { Déclarations privées }
    DossierActuel: string;
    NbPNGDansDossier: integer;
    lstFichiers: TStringDynArray;
    procedure AfficheSousDossiers(DossierParent: string);
  public
    { Déclarations publiques }
  end;

var
  Form10: TForm10;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.Math, System.Generics.Collections,
  System.Generics.Defaults;

procedure TForm10.AfficheSousDossiers(DossierParent: string);
var
  lstDossiers: TStringDynArray;
  i: integer;
  img: timage;
begin
  NbPNGDansDossier := 0;
  if tdirectory.Exists(DossierParent) then
  begin
    // met à jour la liste des sous-dossiers du dossier actuel
    DossierActuel := DossierParent;
    ListBox1.BeginUpdate;
    try
      ListBox1.Items.Clear;
      ListBox1.ListItems[ListBox1.Items.Add('..')].tagstring :=
        tpath.GetDirectoryName(DossierActuel);
      lstDossiers := tdirectory.GetDirectories(DossierActuel);
      for i := 0 to length(lstDossiers) - 1 do
        if tdirectory.Exists(lstDossiers[i]) then
          ListBox1.ListItems
            [ListBox1.Items.Add(tpath.GetFileName(lstDossiers[i]))].tagstring :=
            lstDossiers[i];
    finally
      ListBox1.EndUpdate;
    end;
    // efface images existantes
    for i := FlowLayout1.ChildrenCount - 1 downto 0 do
      if (FlowLayout1.Children[i] is timage) then
        (FlowLayout1.Children[i] as timage).free;
    FlowLayout1.height := 100;
    // affiche les PNG du dossier
    lstFichiers := tdirectory.GetFiles(DossierActuel);
    TArray.Sort<String>(lstFichiers, TStringComparer.Ordinal);
    for i := 0 to length(lstFichiers) - 1 do
      if (tpath.GetExtension(lstFichiers[i]).tolower = '.png') then
      begin
        inc(NbPNGDansDossier);
        img := timage.Create(Self);
        img.parent := FlowLayout1;
        img.Margins.Left := 5;
        img.Margins.right := 5;
        img.Margins.top := 5;
        img.Margins.bottom := 5;
        img.Width := 150;
        img.height := 150;
        img.Bitmap.LoadThumbnailFromFile(lstFichiers[i], img.Width, img.height);
        if FlowLayout1.height < img.height + img.Position.y then
          FlowLayout1.height := img.height + img.Position.y;
      end;
  end
  else
    raise exception.Create('Dossier "' + DossierParent + '" n''existe pas.');
end;

procedure TForm10.btnCreeSpriteSheetClick(Sender: TObject);
var
  NbCol, NbRow: integer;
  row, col: integer;
  spritesheet, img: tbitmap;
  w, h: integer;
  i: integer;
begin
  if (NbPNGDansDossier > 0) then
  begin
    row := 0;
    col := 0;
    w := 0;
    h := 0;
    spritesheet := nil;
    if cbSurUneLigne.IsChecked then
      // Sprites les uns à côté des autres sur une seule ligne
      NbRow := 1
    else
      // Sprites stockés sous forme de grille
      NbRow := trunc(sqrt(NbPNGDansDossier));
    NbCol := ceil(NbPNGDansDossier / NbRow);
    img := tbitmap.Create;
    try
      for i := 0 to length(lstFichiers) - 1 do
        if (tpath.GetExtension(lstFichiers[i]).tolower = '.png') and
          (not lstFichiers[i].tolower.EndsWith('-spritesheet.png')) then
        begin
          img.LoadFromFile(lstFichiers[i]);
          if (not assigned(spritesheet)) then
          begin
            row := 0;
            col := 0;
            w := img.Width;
            h := img.height;
            spritesheet := tbitmap.Create(w * NbCol, h * NbRow);
          end;
          // ne pas mettre taille-1 car Bitmap a un pixel de trop à cause du BMP de Windows mal conçu !
          // spritesheet.CopyFromBitmap(img, Rect(0, 0, img.Width - 1,
          // img.height - 1), col * w, row * h);
          spritesheet.CopyFromBitmap(img, Rect(0, 0, img.Width, img.height),
            col * w, row * h);
          inc(col);
          if (col >= NbCol) then
          begin
            col := 0;
            inc(row);
          end;
        end;
      // TODO : proposer de choisir le nom de la spritesheet ou vérifier son existence avant écrasement
      if assigned(spritesheet) then
      begin
        spritesheet.SaveToFile(tpath.combine(DossierActuel,
          tpath.GetFileNameWithoutExtension(DossierActuel) +
          '-spritesheet.png'));
        spritesheet.free;
      end;
      showmessage('Fichier créé');
    finally
      img.free;
    end;
  end
  else
    raise exception.Create('Pas d''image dans le dossier');
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  AfficheSousDossiers(tpath.GetDocumentsPath);
end;

procedure TForm10.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  AfficheSousDossiers(Item.tagstring);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

// TODO : à déboguer sur macOS car des violations d'accès apparaissent parfois
end.
