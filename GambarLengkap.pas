unit GambarLengkap;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  HtmlParser, System.Actions, FMX.ActnList, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Edit,
  FMX.Objects, FMX.Layouts, System.ImageList, FMX.ImgList, FMX.Gestures;

type
  TFrmLengkap = class(TForm)
    Client: TNetHTTPClient;
    Req: TNetHTTPRequest;
    tlb1: TToolBar;
    EdChapter: TEdit;
    img1: TImage;
    il1: TImageList;
    SbInfo: TStatusBar;
    LblInfo: TLabel;
    btnnext: TSpeedButton;
    btnPrev: TSpeedButton;
    btnRefresh: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure EdChapterChange(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure btnPrevClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
  private
    FDirectory: string;
    FLastDistance: Integer;
    FPosisi: Integer;
    FList: TStrings;
    procedure ParseImage(const AHtml: WideString; AStrings: TStrings);
    procedure ShowImage(ANext: Boolean = True);
  public
    { Public declarations }
  end;

var
  FrmLengkap: TFrmLengkap;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.NmXhdpiPh.fmx ANDROID}

uses
  System.IOUtils;

const
  sHTML = 'http://www.mangacanblog.com/baca-komik-one_piece-' +
    '%s-%d-bahasa-indonesia-one_piece-%s-terbaru.html';

procedure TFrmLengkap.BtnNextClick(Sender: TObject);
begin
  ShowImage;
end;

procedure TFrmLengkap.btnPrevClick(Sender: TObject);
begin
  ShowImage(False);
end;

procedure TFrmLengkap.btnRefreshClick(Sender: TObject);
var
  LUrl: string;
begin
  if (EdChapter.Text = EmptyStr) then
    Exit;

  LUrl := Format(sHTML, [EdChapter.Text, StrToInt(EdChapter.Text) + 1,
    EdChapter.Text]);

  Req.URL := LUrl;
  FList.Clear;
  ParseImage(Req.Execute().ContentAsString(), FList);
  FPosisi := -1;
  if (FList.Count = 0) then
  begin
    ShowMessage(Format('komik One Piece Chapter %s Belum Rilis', [EdChapter.Text]));
    Exit;
  end;
  ShowImage;
end;

procedure TFrmLengkap.EdChapterChange(Sender: TObject);
begin
  btnRefresh.Enabled := True;
end;

procedure TFrmLengkap.FormCreate(Sender: TObject);
begin
  FList := TStringList.Create;
end;

procedure TFrmLengkap.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

procedure TFrmLengkap.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  LObj: IControl;
  LImage: TImage;
  LImageCenter: TPointF;
begin
  if EventInfo.GestureID = igiZoom then
  begin
    LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
    if LObj is TImage then
    begin
      if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and
        (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then
      begin
        { zoom the image }
        LImage := TImage(LObj.GetObject);
        LImageCenter := LImage.Position.Point + PointF(LImage.Width / 2,
          LImage.Height / 2);
        LImage.Width := LImage.Width + (EventInfo.Distance - FLastDistance);
        LImage.Height := LImage.Height + (EventInfo.Distance - FLastDistance);
        LImage.Position.X := LImageCenter.X - LImage.Width / 2;
        LImage.Position.Y := LImageCenter.Y - LImage.Height / 2;
      end;
      FLastDistance := EventInfo.Distance;
    end;
  end;

end;

procedure TFrmLengkap.ParseImage(const AHtml: WideString; AStrings: TStrings);
var
  I: Integer;
  LNodes: IHtmlElement;
  LlistNodes: IHtmlElementList;
  LElement: IHtmlElement;
  LUrlGambar: WideString;
  LFile: TFileName;
begin
  FDirectory := Format('%s/OnePiece/%s',[TPath.GetSharedPicturesPath,
    EdChapter.Text]);

  if not (TDirectory.Exists(FDirectory)) then
  begin
    TDirectory.CreateDirectory(FDirectory);
  end;

  LFile:= TPath.Combine(FDirectory, Format('data-%s.txt', [EdChapter.Text]));

  if TFile.Exists(LFile) then
  begin
    AStrings.LoadFromFile(LFile);
    Exit;
  end;

  LNodes := ParserHTML(AHtml);
  LlistNodes := LNodes.SimpleCSSSelector('div[id="manga"] img');
  for I := 0 to LlistNodes.Count - 1 do
  begin
    LElement := LlistNodes.Items[I];
    LUrlGambar := LElement.Attributes['src'];
    AStrings.Add(LUrlGambar);
  end;
  if (AStrings.Count > 0) then
    AStrings.SaveToFile(LFile);
end;

procedure TFrmLengkap.ShowImage(ANext: Boolean);
var
  LUrl: string;
  Lstream: TStream;
  LFile: TFileName;
begin
  if ANext then
    Inc(FPosisi)
  else
    Dec(FPosisi);

  LFile:= TPath.Combine(FDirectory, Format('%d.jpg', [FPosisi + 1 ]));

  if TFile.Exists(LFile) then
  begin
    img1.Bitmap.LoadFromFile(LFile);
  end else
  begin
    LUrl := FList[FPosisi];
    Req.URL := LUrl;
    Lstream := Req.Execute().ContentStream;
    img1.Bitmap.LoadFromStream(Lstream);
    img1.Bitmap.SaveToFile(LFile);
  end;

  btnRefresh.Enabled := False;
  btnPrev.Enabled := FPosisi > 0;
  btnnext.Enabled := FPosisi < FList.Count - 1;
  LblInfo.Text := Format('%d Dari %d Halaman', [FPosisi + 1, FList.Count]);
end;

end.
