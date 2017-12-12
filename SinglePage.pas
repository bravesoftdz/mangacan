unit SinglePage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.WebBrowser, FMX.Edit, HtmlParser, System.IOUtils,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TFrmSingle = class(TForm)
    ToolBar1: TToolBar;
    BtnRefresh: TButton;
    LblJudul: TLabel;
    Browser: TWebBrowser;
    EdChapter: TEdit;
    Client: TNetHTTPClient;
    Req: TNetHTTPRequest;
    procedure BtnRefreshClick(Sender: TObject);
  private
    FChapter: string;
    FTitle: string;
    function ParseImageList(AChapter: string): string;
  public
  published
    property Chapter: string read FChapter write FChapter;
    property Title: string read FTitle write FTitle;
  end;

var
  FrmSingle: TFrmSingle;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.NmXhdpiPh.fmx ANDROID}

const
  sSource = 'http://www.mangacanblog.com/baca-komik-one_piece-' +
    '%s-%d-bahasa-indonesia-one_piece-%s-terbaru.html';
  sIndex = '<!DOCTYPE html><html lang="id"><head><meta charset="UTF-8">' +
    '<meta name="viewport" content="width=device-width, initial-scale=1">' +
    '<style>#responsive-image{width:100%%;height:auto;margin:auto}</style>' +
    '</head><body>%s</body></html>';
  sImage = '<img src="%s" id="responsive-image">';

procedure TFrmSingle.BtnRefreshClick(Sender: TObject);
var
  LFileHtml : string;
begin
  Chapter := EdChapter.Text;
  Title := '';
  LblJudul.Text := Title;
  if (Chapter = EmptyStr) then
    Exit;

  LFileHtml := ParseImageList(Chapter);
  if (LFileHtml.IsEmpty) then
  begin
    ShowMessage(Format('Komik One Piece Chapter %s Tidak ditemukan', [Chapter]));
    Exit;
  end;
  Browser.Navigate('file://' + LFileHtml);
end;

function TFrmSingle.ParseImageList(AChapter: string): string;
var
  I: Integer;
  LUrl, LDirectory: string;
  LHtml : WideString;
  LNodes: IHtmlElement;
  LlistNodes: IHtmlElementList;
  LElement: IHtmlElement;
  LUrlGambar: WideString;
  LFile: TFileName;
  LImageSrc: string;
  LIndex: string;
begin
  Result := EmptyStr;
  LDirectory := Format('%s/OnePiece',[TPath.GetCachePath]);

  if not (TDirectory.Exists(LDirectory)) then
  begin
    TDirectory.CreateDirectory(LDirectory);
  end;

  LFile:= TPath.Combine(LDirectory, Format('%s.html', [AChapter]));

  if TFile.Exists(LFile) then
  begin
    Result := LFile;
    Exit;
  end;

  LUrl := Format(sSource, [Chapter, StrToInt(Chapter) + 1, Chapter]);

  Req.URL := LUrl;
  LHtml := Req.Execute().ContentAsString();

  LNodes := ParserHTML(LHtml);
  LlistNodes := LNodes.SimpleCSSSelector('div[id="manga"] img');
  if (LListNodes.Count = 0) then
    exit;

  for I := 0 to LlistNodes.Count - 1 do
  begin
    LElement := LlistNodes.Items[I];
    LUrlGambar := LElement.Attributes['src'];
    LImageSrc := LImageSrc + Format(sImage, [LUrlGambar]);
  end;
  LIndex := Format(sIndex, [LImageSrc]);
  TFile.WriteAllText(LFile, LIndex);
  Result := LFile;
end;

end.
