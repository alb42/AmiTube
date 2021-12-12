unit Filedownloadunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOnProgress = procedure(Sender: TObject; Percent: integer; Speed: Integer) of object;

procedure DonwloadFile(OnProgress: TOnProgress; cURL: string; cFile: string);

procedure KillDownload;

var
  ShortVer: string = 'AmiTube';

implementation

uses
  fphttpclient;
var
  vHTTP: TFPHTTPClient = nil;

procedure KillDownload;
begin
  if Assigned(vHTTP) then
    vHTTP.Terminate;
end;

{ TStreamAdapter }
type
  TStreamAdapter = class(TStream)
  strict private
    fOnProgress: TOnProgress;
    fPercent: integer;
    fStream: TStream;
    FSize: LongInt;
    FPos: LongInt;
    FBuffer: Pointer;
    FBufPos: PByte;
    FBytes: LongInt;
    StartTime: LongWord;
  public
    constructor Create(AStream: TStream; ASize: int64);
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    procedure DoProgress(Writing: boolean); virtual;
  published
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
  end;

procedure DonwloadFile(OnProgress: TOnProgress; cURL: string; cFile: string);
//const
//  cUrl = 'http://www.imagemagick.org/download/binaries/ImageMagick-6.8.6-8-Q8-x86-static.exe';
//  cFile = 'ImageMagick-6.8.6-8-Q8-x86-static.exe';
var
  vStream: TStreamAdapter;
  VSize: int64 = 0;

  I: integer;
  S: string;
begin
  vStream := nil;
  vHTTP := nil;
  vSize := 1000000;
  try
    vHTTP := TFPHTTPClient.Create(nil);
    vHTTP.AddHeader('User-Agent', ShortVer + ' ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%});
    vHTTP.HTTPMethod('HEAD', cUrl, nil, [200]);
    for I := 0 to pred(vHTTP.ResponseHeaders.Count) do
    begin
      S := UpperCase(vHTTP.ResponseHeaders[I]);
      if Pos('CONTENT-LENGTH:', S) > 0 then
      begin
        VSize := StrToIntDef(Copy(S, Pos(':', S) + 1, Length(S)), 0);
        Break;
      end;
    end;

    vStream := TStreamAdapter.Create(TFileStream.Create(cFile, fmCreate), VSize);
    vStream.OnProgress := OnProgress;

    vHTTP.HTTPMethod('GET', cUrl, vStream, [200]);
    // vHTTP.Get(cUrl, vStream);
  finally
    vHTTP.Free;
    vHTTP := nil;
    vStream.Free;
  end;
end;

{ TStreamAdapter }

const BUFFSIZE = 1024000;

constructor TStreamAdapter.Create(AStream: TStream; ASize: int64);
begin
  inherited Create;
  StartTime := GetTickCount;
  FStream := AStream;
  fStream.Size := ASize;
  fStream.Position := 0;
  FSize := ASize;
  FPos := 0;
  FBuffer := AllocMem(BUFFSIZE);
  FBufPos := FBuffer;
  FBytes := 0;
end;

destructor TStreamAdapter.Destroy;
begin
  if FBytes > 0 then
    FStream.Write(FBuffer, FBytes);
  FStream.Free;
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TStreamAdapter.Read(var Buffer; Count: longint): longint;
begin
  Result := FStream.Read(Buffer, Count);
  DoProgress(False);
end;

function TStreamAdapter.Write(const Buffer; Count: longint): longint;
begin
  if FBytes + Count > BUFFSIZE then
  begin
    FStream.Write(FBuffer, FBytes);
    FBufPos := FBuffer;
    FBytes := 0;
  end;
  //
  Move(Buffer, FBufPos^, Count);
  Result := Count;
  Inc(FBufPos, Count);
  Inc(FBytes, Count);
  //
  //Result := FStream.Write(Buffer, Count);
  Inc(FPos, Result);
  DoProgress(False);
end;

function TStreamAdapter.Seek(Offset: longint; Origin: word): longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TStreamAdapter.DoProgress(Writing: boolean);
var
  t1: LongWord;
  Speed: Integer;
begin
  //fPercent := Trunc((FStream.Position) / (FStream.Size) * 100);
  fPercent := Trunc((FPos) / (FSize) * 100);
  t1 := GetTickCount;
  Speed := 0;
  t1 := t1 - StartTime;
  if t1 > 0 then
    Speed := Round(FPos / (t1 / 1000));
  if Assigned(OnProgress) then
  begin
    OnProgress(self, FPercent, Speed);
  end;
end;

end.
