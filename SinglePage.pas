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
    LblJudul: TLabel;
    Browser: TWebBrowser;
    Client: TNetHTTPClient;
    Req: TNetHTTPRequest;
    BtnBack: TButton;
  private
    FChapter: string;
    FTitle: string;
    function ParseImageList(AChapter: string): string;
  public
    procedure Refresh(AChapter: string; ATitle: string);
  end;

var
  FrmSingle: TFrmSingle;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.NmXhdpiPh.fmx ANDROID}

uses
  Commons;

const
  sSource = 'http://www.mangacanblog.com/baca-komik-one_piece-' +
    '%s-%d-bahasa-indonesia-one_piece-%s-terbaru.html';
  sIndex = '<!DOCTYPE html><html lang="id"><head><meta charset="UTF-8">' +
    '<meta name="viewport" content="width=device-width, initial-scale=1">' +
    '<style>#responsive-image{width:100%%;height:auto;margin:auto}</style>' +
    '</head><body>%s</body></html>';
  sImage = '<img src="%s" id="responsive-image">';

procedure TFrmSingle.Refresh(AChapter, ATitle: string);
var
  LFileHtml : string;
begin
  FChapter := AChapter;
  FTitle := ATitle;
  LblJudul.Text := Format('%s : %s', [FChapter, FTitle]);
  if (FChapter = EmptyStr) then
    Exit;

  LFileHtml := ParseImageList(FChapter);
  if (LFileHtml.IsEmpty) then
  begin
    ShowMessage(Format('Komik One Piece Chapter %s Tidak ditemukan', [FChapter]));
    Exit;
  end;
  Browser.Navigate('file://' + LFileHtml);
end;

function TFrmSingle.ParseImageList(AChapter: string): string;
var
  I: Integer;
  LUrl: string;
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
  LFile:= TPath.Combine(CachePath, Format('%s.html', [AChapter]));

  if TFile.Exists(LFile) then
  begin
    Result := LFile;
    Exit;
  end;

  LUrl := Format(sSource, [FChapter, StrToInt(AChapter) + 1, AChapter]);

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
