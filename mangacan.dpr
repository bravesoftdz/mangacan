program mangacan;

uses
  System.StartUpCopy,
  FMX.Forms,
  HtmlParser in 'HtmlParser.pas',
  SinglePage in 'SinglePage.pas' {FrmSingle},
  TitlePage in 'TitlePage.pas' {FrmTitle},
  Commons in 'Commons.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmTitle, FrmTitle);
  Application.CreateForm(TFrmSingle, FrmSingle);
  Application.Run;
end.
