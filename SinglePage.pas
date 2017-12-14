unit SinglePage;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.WebBrowser;

type
  TLoadThread = class(TThread)
  private
    FChapter: string;
    FBrowser: TWebBrowser;
    FHtmlFile: TFileName;
    procedure SetHtmlFile;
    procedure ParseImageList(AHtmlDoc: string);
    procedure LoadBrowser;
  protected
    procedure Execute; override;
  public
    constructor Create(ABrowser: TWebBrowser; AChapter: string);
  end;

  TFrmSingle = class(TForm)
    ToolBar1: TToolBar;
    LblJudul: TLabel;
    Browser: TWebBrowser;
    BtnBack: TButton;
    procedure BtnBackClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FChapter: string;
    FTitle: string;
    procedure Refresh;
  public
    constructor Create(AOwner: TComponent; AChapter:
      string; ATitle: string); reintroduce;
  end;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.NmXhdpiPh.fmx ANDROID}

uses
  Commons, System.Net.HttpClient, HtmlParser, System.IOUtils;

const
  sSource = 'http://www.mangacanblog.com/baca-komik-one_piece-' +
    '%s-%d-bahasa-indonesia-one_piece-%s-terbaru.html';
  sIndex = '<!DOCTYPE html><html lang="id"><head><meta charset="UTF-8">' +
    '<meta name="viewport" content="width=device-width, initial-scale=1">' +
    '<style>#responsive-image{width:100%%;height:auto;margin:auto}</style>' +
    '</head><body>%s</body></html>';
  sImage = '<img src="%s" id="responsive-image">';

constructor TFrmSingle.Create(AOwner: TComponent; AChapter, ATitle: string);
begin
  inherited Create(AOwner);
  FChapter := AChapter;
  FTitle := ATitle;
  LblJudul.Text := Format('%s : %s', [FChapter, FTitle]);
end;

procedure TFrmSingle.FormShow(Sender: TObject);
begin
  Refresh;
end;

procedure TFrmSingle.BtnBackClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmSingle.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFrmSingle.Refresh;
var
  FLoadThread : TLoadThread;
begin
  FLoadThread := TLoadThread.Create(Browser, FChapter);
  FLoadThread.Start;
end;

{ TLoadThread }

constructor TLoadThread.Create(ABrowser: TWebBrowser; AChapter: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FBrowser := ABrowser;
  FChapter := AChapter;
  FHtmlFile := TPath.Combine(CachePath, Format('%s.html', [AChapter]));
end;

procedure TLoadThread.SetHtmlFile;
var
  LClient : THTTPClient;
  LResponse : IHTTPResponse;
  LUrl: string;
begin
  if TFile.Exists(FHtmlFile) then
  begin
    Exit;
  end;

  LClient := THTTPClient.Create;
  try
    LUrl := Format(sSource, [FChapter, StrToInt(FChapter) + 1, FChapter]);
    LResponse := LClient.Get(LUrl);
    ParseImageList(LResponse.ContentAsString);
  finally
    LClient.Free;
  end;
end;

procedure TLoadThread.ParseImageList(AHtmlDoc: string);
var
  I: Integer;
  LNodes: IHtmlElement;
  LlistNodes: IHtmlElementList;
  LElement: IHtmlElement;
  LUrlGambar: WideString;
  LImageSrc: string;
  LIndex: string;
begin
  LNodes := ParserHTML(AHtmlDoc);
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
  TFile.WriteAllText(FHtmlFile, LIndex);
end;

procedure TLoadThread.LoadBrowser;
begin
  if (TFile.Exists(FHtmlFile)) then
    FBrowser.Navigate('file://' + FHtmlFile);
end;

procedure TLoadThread.Execute;
begin
  SetHtmlFile;
  Synchronize(LoadBrowser);
end;

end.
