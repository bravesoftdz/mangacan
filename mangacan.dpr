program mangacan;

uses
  System.StartUpCopy,
  FMX.Forms,
  GambarLengkap in 'GambarLengkap.pas' {FrmLengkap},
  HtmlParser in 'HtmlParser.pas',
  DaftarJudul in 'DaftarJudul.pas' {FrmDaftar},
  SinglePage in 'SinglePage.pas' {FrmSingle};

{$R *.res}

begin
  Application.Initialize;
//  Application.CreateForm(TFrmLengkap, FrmLengkap);
  Application.CreateForm(TFrmSingle, FrmSingle);
  Application.Run;
end.
