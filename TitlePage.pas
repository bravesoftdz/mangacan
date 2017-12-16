unit TitlePage;

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
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView,
  FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TLoadThread = class(TThread)
  private
    FJson: string;
    FListFile: TFileName;
    FTitles: TStrings;
    FLIstView: TListView;
    FIndicator: TAniIndicator;
    FOffline: Boolean;
    FAllChapter: Boolean;
    procedure GetJson;
    procedure ParseTitleList;
    procedure TitlesChange;
  protected
    procedure Execute; override;
  public
    constructor Create(AListView: TListView; AIndicator: TAniIndicator;
      AOffline: Boolean; AAllChapter: Boolean);
    destructor Destroy; override;
  end;

  TFrmTitle = class(TForm)
    LvTitle: TListView;
    ToolBar1: TToolBar;
    BtnRefresh: TButton;
    Label1: TLabel;
    Indicator: TAniIndicator;
    PnlBottom: TPanel;
    SwShow: TSwitch;
    LblShow: TLabel;
    procedure FormShow(Sender: TObject);
    procedure LvTitleItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure BtnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SwShowSwitch(Sender: TObject);
  private
    procedure Refresh(AOffline: Boolean; AAllChapter: Boolean);
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
{$R *.XLgXhdpiTb.fmx ANDROID}
{$R *.LgXhdpiTb.fmx ANDROID}

uses
  SinglePage, System.JSON, System.IOUtils, System.Net.HttpClient, Commons;

const
  sUrlAll = 'https://www.cbsanjaya.com/onepiece/all.json';
  sUrlLast5 = 'https://www.cbsanjaya.com/onepiece/last5.json';

resourcestring
  sShowAllChapter = 'Menampilkan Seluruh Chapter';
  sShowOnly5Chapter = 'Hanya Menampilkan Lima Chapter Terahir';

procedure TFrmTitle.FormCreate(Sender: TObject);
begin
  if not (TDirectory.Exists(CachePath)) then
  begin
    TDirectory.CreateDirectory(CachePath);
  end;
end;

procedure TFrmTitle.FormShow(Sender: TObject);
begin
  Refresh(True, SwShow.IsChecked);
end;

procedure TFrmTitle.BtnRefreshClick(Sender: TObject);
begin
  Refresh(False, SwShow.IsChecked);
end;

procedure TFrmTitle.LvTitleItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  LFrmSingle : TFrmSingle;
begin
  LFrmSingle := TFrmSingle.Create(Self, AItem.Text, AItem.Detail);
  LFrmSingle.Show;
end;

procedure TFrmTitle.Refresh(AOffline: Boolean; AAllChapter: Boolean);
var
  FLoadThread : TLoadThread;
begin
  FLoadThread := TLoadThread.Create(LvTitle, Indicator, AOffline, AAllChapter);
  FLoadThread.Start;
end;

procedure TFrmTitle.SwShowSwitch(Sender: TObject);
begin
  if TSwitch(Sender).IsChecked then
  begin
    LblShow.Text := sShowAllChapter;
  end else
  begin
    LblShow.Text := sShowOnly5Chapter;
  end;
  Refresh(False, SwShow.IsChecked);
end;

{ TLoadThread }

constructor TLoadThread.Create(AListView: TListView; AIndicator: TAniIndicator;
  AOffline: Boolean; AAllChapter: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOffline := AOffline;
  FTitles := TStringList.Create;
  FIndicator := AIndicator;
  FLIstView := ALIstView;
  FAllChapter := AAllChapter;
  if FAllChapter then
    FListFile := TPath.Combine(CachePath, 'all.json') else
    FListFile := TPath.Combine(CachePath, 'last5.json');
end;

destructor TLoadThread.Destroy;
begin
  FTitles.Free;
  inherited;
end;

procedure TLoadThread.GetJson;
var
  LClient : THTTPClient;
  LResponse : IHTTPResponse;
  LUrs: string;
begin
  LClient := THTTPClient.Create;
  try
    try
      if FAllChapter then
        LUrs := sUrlAll else
        LUrs := sUrlLast5;

      LResponse := LClient.Get(LUrs);
      FJson := LResponse.ContentAsString;
    except
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
  LChapter, LJudul : string;
  LValues, LObject : TJSONValue;
begin
  if (FJson.IsEmpty) then
  begin
    if (TFile.Exists(FListFile)) then
      FTitles.LoadFromFile(FListFile);
    exit;
  end;

  LValues := TJSONObject.ParseJSONValue(FJson);

  FTitles.Clear;
  for LObject in LValues as TJSONArray do
  begin
    LChapter := LObject.GetValue<string>('chapter');
    LJudul := LObject.GetValue<string>('title');
    FTitles.AddPair(LChapter, LJudul);
  end;
  FTitles.SaveToFile(FListFile);
end;

procedure TLoadThread.Execute;
begin
  FIndicator.Visible := True;

  if (FOffline) then
  begin
    if (TFile.Exists(FListFile)) then
      FTitles.LoadFromFile(FListFile);
  end else
  begin
    GetJson;
    ParseTitleList;
  end;

  Synchronize(TitlesChange);
  FIndicator.Visible := False;
end;

end.
