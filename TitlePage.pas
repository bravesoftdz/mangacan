unit TitlePage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView;

type
  TFrmTitle = class(TForm)
    LvTitle: TListView;
    procedure LvTitlePullRefresh(Sender: TObject);
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

procedure TFrmTitle.LvTitlePullRefresh(Sender: TObject);
begin
  Refresh;
end;

procedure TFrmTitle.Refresh;
var
  LItem: TListViewItem;
begin
  LItem := LvTitle.Items.Add;
  LItem.Text := '888';
  LItem.Detail := 'Singa';
end;

end.
