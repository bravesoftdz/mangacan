program mangacan;

uses
  System.StartUpCopy,
  FMX.Forms,
  HtmlParser in 'HtmlParser.pas',
  SinglePage in 'SinglePage.pas' {FrmSingle};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmSingle, FrmSingle);
  Application.Run;
end.
