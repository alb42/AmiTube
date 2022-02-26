unit Filedownloadunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MUIClass.Dialog;

type
    // event type for pregress informations, mainly for download files
  TProgressEvent = procedure(Sender: TObject; Percent: Integer; Text: string) of object;

  TOnProgress = procedure(Sender: TObject; Percent, Speed: integer; FullSize: Int64) of object;

{ Download a file with progress from URL to File}
procedure DownloadFile(OnProgress: TOnProgress; AURL: string; AFileName: string);

procedure KillDownload;

// short version kept here because need for download as well, will be set at
// start to the actual Version number
var
  ShortVer: string = 'AmiTube';
  SearchURL, SearchURLID, ConvertURL, ShareURL,
  SharedURL, IconURL, DownloadURL: string;

implementation

uses
  fphttpclient;

// must kept global for the kill function, means only one allows to run at a time
var
  DownClient: TFPHTTPClient = nil;

{ try to kill the download, not sure when it will be killed}
procedure KillDownload;
begin
  if Assigned(DownClient) then
    DownClient.Terminate;
end;

{ TStreamAdapter }
type
  TStreamAdapter = class(TStream)
  strict private
    // event
    FOnProgress: TOnProgress;
    // filestream
    FStream: TStream;
    // Full size of the resulting file
    FSize: LongInt;
    // position in the filestream -> number of bytes recieved
    FPos: LongInt;
    // Buffer for faster writing on classic Amiga
    FBuffer: Pointer;
    // position in the Buffer
    FBufPos: PByte;
    // bytes in the Buffer still to write
    FBytes: LongInt;
    // time we started to calculate the speed
    StartTime: LongWord;
    // Buffersize
    BuffSize: LongInt;
    // last time the OnProgress was started
    LastCall: LongWord;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    procedure DoProgress(Writing: Boolean); virtual;
    procedure HeaderEvent(Sender: TObject);
  published
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
  end;

procedure DownloadFile(OnProgress: TOnProgress; AURL: string; AFileName: string);
var
  ProgStream: TStreamAdapter;
begin
  ProgStream := nil;
  DownClient := nil;
  try
    try
      DownClient := TFPHTTPClient.Create(nil);
      DownClient.AllowRedirect := True;
      DownClient.AddHeader('User-Agent', ShortVer + ' ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%});
      //
      ProgStream := TStreamAdapter.Create(TFileStream.Create(AFileName, fmCreate));
      ProgStream.OnProgress := OnProgress;
      //
      DownClient.OnHeaders := @(ProgStream.HeaderEvent);
      DownClient.HTTPMethod('GET', AURL, ProgStream, [200]);
    except
      on E: Exception do
      begin
        writeln('Error downloading file: ' + E.Message);
        FreeAndNil(ProgStream);
        DeleteFile(AFileName);
      end;
    end;
    // DownClient.Get(AURL, ProgStream);
  finally
    DownClient.Free;
    DownClient := nil;
    ProgStream.Free;
  end;
end;

{ TStreamAdapter }

const
  DefBUFFSIZE = 1024000;
  DefSmallBUFFSIZE = 10240;

constructor TStreamAdapter.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FStream.Size := 0;
  FStream.Position := 0;
  FSize := 0;
  FPos := 0;
  // Create the Buffer, much faster saving to HD
  BuffSize := DefBUFFSIZE;
  try
    FBuffer := AllocMem(BUFFSIZE);
  except
    FBuffer := nil;
  end;
  // cannot create the Buffer, try with a much smaller Buffer
  if FBuffer = nil then
  begin
    BuffSize := DefSmallBUFFSIZE;
    FBuffer := AllocMem(BUFFSIZE);
  end;
  FBufPos := FBuffer;
  FBytes := 0;
  StartTime := GetTickCount;
end;

destructor TStreamAdapter.Destroy;
begin
  if FBytes > 0 then
    FStream.Write(FBuffer, FBytes);
  FStream.Free;
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TStreamAdapter.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Read(Buffer, Count);
  DoProgress(False);
end;

function TStreamAdapter.Write(const Buffer; Count: LongInt): LongInt;
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
  Inc(FPos, Result);
  DoProgress(False);
end;

function TStreamAdapter.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  Result := FStream.Seek(Offset, Origin);
end;

{ Header received, if we have a size in there we can show a progressbar }
procedure TStreamAdapter.HeaderEvent(Sender: TObject);
var
  HGet: TFPHTTPClient;
  i: Integer;
  s: String;
begin
  if Sender is TFPHTTPClient then
  begin
    HGet := TFPHTTPClient(Sender);
    for I := 0 to Pred(HGet.ResponseHeaders.Count) do
    begin
      S := UpperCase(HGet.ResponseHeaders[I]);
      if Pos('CONTENT-LENGTH:', S) > 0 then
      begin
        //writeln('got size: ', StrToIntDef(Copy(S, Pos(':', S) + 1, Length(S)), 0), ' should be: ', FSize);
        FSize := StrToIntDef(Copy(S, Pos(':', S) + 1, Length(S)), 0);
        Break;
      end;
    end;
    StartTime := GetTickCount;
  end;
end;

{ do progress change }
procedure TStreamAdapter.DoProgress(Writing: boolean);
var
  t1: LongWord;
  Speed: Integer;
  Percent: integer;
begin
  //
  t1 := GetTickCount;
  if t1 - LastCall > 400 then  // do not call too often
  begin
    // calc size
    if FSize > 0 then
      Percent := Trunc(FPos / FSize * 100)
    else
      Percent := 0;
    // Calc Speed
    Speed := 0;
    t1 := t1 - StartTime;
    if t1 > 0 then
      Speed := Round(FPos / (t1 / 1000));
    // send to GUI
    if Assigned(OnProgress) then
      OnProgress(self, Percent, Speed, FPos);
    //
    LastCall := t1;
  end;
end;

end.
