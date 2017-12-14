unit TitlePage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TLoadThread = class(TThread)
  private
    FHtml: WideString;
    FHtmlFile: TFileName;
    FTitles: TStrings;
    FLIstView: TListView;
    FIndicator: TAniIndicator;
    procedure GetHtml;
    procedure ParseTitleList;
    procedure TitlesChange;
  protected
    procedure Execute; override;
  public
    constructor Create(AListView: TListView; AIndicator: TAniIndicator);
    destructor Destroy; override;
  end;

  TFrmTitle = class(TForm)
    LvTitle: TListView;
    ToolBar1: TToolBar;
    BtnRefresh: TButton;
    Label1: TLabel;
    Indicator: TAniIndicator;
    procedure LvTitlePullRefresh(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LvTitleItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure BtnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Refresh;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmTitle: TFrmTitle;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiPh.fmx ANDROID}

uses
  SinglePage, HtmlParser, System.IOUtils, System.Net.HttpClient, Commons;

const
  sUrl = 'http://www.mangacanblog.com/baca-komik-one_piece-bahasa-indonesia-online-terbaru.html';

procedure TFrmTitle.BtnRefreshClick(Sender: TObject);
begin
  Refresh;
end;

procedure TFrmTitle.FormCreate(Sender: TObject);
begin
  if not (TDirectory.Exists(CachePath)) then
  begin
    TDirectory.CreateDirectory(CachePath);
  end;
end;

procedure TFrmTitle.FormShow(Sender: TObject);
begin
  Refresh;
end;

procedure TFrmTitle.LvTitleItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  FrmSingle.Refresh(AItem.Text, AITem.Detail);
  FrmSingle.ShowModal;
end;

procedure TFrmTitle.LvTitlePullRefresh(Sender: TObject);
begin
  Refresh;
end;

procedure TFrmTitle.Refresh;
var
  FLoadThread : TLoadThread;
begin
  Indicator.Visible := True;
  FLoadThread := TLoadThread.Create(LvTitle, Indicator);
  FLoadThread.Start;
end;

{ TLoadThread }

constructor TLoadThread.Create(AListView: TListView; AIndicator: TAniIndicator);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTitles := TStringList.Create;
  FIndicator := AIndicator;
  FLIstView := ALIstView;
  FHtmlFile := TPath.Combine(CachePath, 'index.html');
end;

destructor TLoadThread.Destroy;
begin
  FTitles.Free;
  inherited;
end;

procedure TLoadThread.GetHtml;
var
  LClient : THTTPClient;
  LResponse : IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    try
      LResponse := LClient.Get(sUrl);
      FHtml := LResponse.ContentAsString;
      TFile.WriteAllText(FHtmlFile, FHtml);
    except on E: Exception do
      if TFile.Exists(FHtmlFile) then
        FHtml := TFile.ReadAllText(FHtmlFile);
    end;
  finally
    LClient.Free;
  end;
end;

procedure TLoadThread.TitlesChange;
var
  I: Integer;
  LItem: TListViewItem;
begin
  FLIstView.Items.Clear;
  for I := 0 to Pred(FTitles.Count) do
  begin
    LItem := FLIstView.Items.Add;
    LItem.Text := FTitles.KeyNames[I];
    LItem.Detail := FTitles.ValueFromIndex[I];
  end;
end;

procedure TLoadThread.ParseTitleList;
var
  I : integer;
  LNodes: IHtmlElement;
  LlistNodes: IHtmlElementList;
  LElement: IHtmlElement;
  LLengkap, LChapter, LJudul : string;
begin
  LNodes := ParserHTML(FHtml);
  LlistNodes := LNodes.SimpleCSSSelector('a[class="chaptersrec"]');
  if (LListNodes.Count = 0) then
    exit;

  for I := 0 to LlistNodes.Count - 1 do
  begin
    LElement := LlistNodes.Items[I];
    LLengkap := LElement.InnerText;
    LChapter := Copy(LLengkap, 11, LLengkap.IndexOf(':') - 11);
    LJudul := Copy(LLengkap, LLengkap.IndexOf(':') + 3, LLengkap.Length);
    FTitles.AddPair(LChapter, LJudul);
  end;
end;

procedure TLoadThread.Execute;
begin
  FIndicator.Visible := True;
  GetHtml;
  ParseTitleList;
  Synchronize(TitlesChange);
  FIndicator.Visible := False;
end;

end.
