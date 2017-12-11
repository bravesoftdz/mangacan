unit DaftarJudul;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  System.ImageList, FMX.ImgList, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ListView, System.IOUtils;

type
  TFrmDaftar = class(TForm)
    lv1: TListView;
    tlbDaftar: TToolBar;
    ilDaftar: TImageList;
    BtnRefresh: TButton;
    procedure BtnRefreshClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmDaftar: TFrmDaftar;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

procedure TFrmDaftar.BtnRefreshClick(Sender: TObject);
begin
  ShowMessage(TPath.GetPicturesPath);
end;

end.
