unit Commons;

interface
uses
  System.SysUtils, System.IOUtils;

function CachePath : string;

implementation

function CachePath : string;
begin
  Result := Format('%s/OnePiece',[TPath.GetCachePath]);
end;

end.
