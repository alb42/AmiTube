program AmiTube;
// search and download Youtube videos to CDXL
{$mode objfpc}{$H+}
uses
  AThreads,
  Classes, SysUtils, fphttpclient, jsonparser, fpjson,
  MUIClass.Base, MUIClass.Window, MUIClass.Group, MUIClass.Area, MUIClass.Gadget,
  MUIClass.StringGrid, MUIClass.Dialog, MUIClass.List, filedownloadunit;

const
  BaseURL = 'http://build.alb42.de/ytsearch.php?num=10&q=';
  //BaseURL = 'http://build.alb42.de/yttest.php?num=10&q=';
  ConvertURL = 'http://build.alb42.de/ytcdxl.php?id=';

  MovieFolder = 'movies';

type

  TProgressEvent = procedure(Sender: TObject; Percent: Integer; Text: string) of object;

  { TSearchThread }

  TSearchThread = class(TThread)
  protected
    FPer: Integer;
    FTxt: string;
    procedure DoOnEnd;
    procedure DoSyncProgress;
    procedure DoProgress(APercent: integer; AText: string);
    procedure Execute; override;
  public
    Results: array of record
      Name: string;
      ID: string;
      Duration: string;
      Desc: string;
    end;
    ErrMsg: string;
    IsError: Boolean;
    Search: string;
    OnEnd: TNotifyEvent;
    OnProgress: TProgressEvent;
  end;

  { TStartConvertThread }

  TStartConvertThread = class(TThread)
  private
    FPer: Integer;
    FTxt: string;
    LastTime: Cardinal;
    procedure DoSyncProgress;
    procedure DoProgress(APercent: integer; AText: string);
    procedure ProgressUpdate(Sender: TObject; Percent: integer);
  protected
    procedure Execute; override;
    procedure DoOnEnd;
  public
    ID: String;
    Movies: string;
    Desc: string;
    OnEnd: TNotifyEvent;
    OnProgress: TProgressEvent;

  end;


  { TMainWindow }

  TMainWindow = class(TMUIWindow)
    SearchField: TMUIString;
    List: TMUIStringGrid;
    TextOut: TMUIFloatText;
    Button, Button2: TMUIButton;
    StatusLabel: TMUIText;
    Progress: TMUIGauge;
    procedure SearchEntry(Sender: TObject);
    procedure EndThread(Sender: TObject);
    procedure EndCThread(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ProgressEvent(Sender: TObject; Percent: Integer; Text: string);
  private
    Movies: string;
    Results: array of record
      ID: string;
      Desc: string;
    end;
    SearchThread: TSearchThread;
    ConvertThread: TStartConvertThread;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

function GetFile(address: string; AStream: TStream): Boolean;
var
  hp: TFPHTTPClient;
begin
  Result := False;
  //if not IsOnline then
  //  Exit;
  hp := TFPHTTPClient.Create(nil);
  try
    hp.AddHeader('User-Agent', 'AmiTube ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%});
    hp.Get(address, AStream);
    Result := True;
  finally
    hp.Free;
  end;
end;

procedure TStartConvertThread.ProgressUpdate(Sender: TObject; Percent: integer);
var
  t1: Cardinal;
begin
  t1 := GetTickCount;
  if t1-LastTime > 500 then
  begin
    DoProgress(Percent, 'Downloading ...');
    LastTime := GetTickCount;
  end;
  //writeln('Status: ', Percent, '%');
end;

procedure TStartConvertThread.DoSyncProgress;
begin
  if Assigned(OnProgress) then
    OnProgress(Self, FPer, FTxt);
end;

procedure TStartConvertThread.DoProgress(APercent: integer; AText: string);
begin
  FPer := APercent;
  FTxt := AText;
  Synchronize(@DoSyncProgress);
end;

procedure TStartConvertThread.Execute;
var
  URL: string;
  Mem: TMemoryStream;
  SL: TStringList;
begin
  try
  DoProgress(0, 'Start converting movie');
  Url := ConvertURL + ID;
  Mem := TMemoryStream.Create;
  if GetFile(Url, Mem) then
  begin
    SL := TStringList.Create;
    Mem.Position := 0;
    SL.LoadFromStream(Mem);
    //
    LastTime := GetTickCount;
    DoProgress(0, 'Downloading ...');
    if Pos('http', sl[0]) >= 1 then
    begin
      //writeln('download file');
      DonwloadFile(@ProgressUpdate, sl[0], IncludeTrailingPathDelimiter(Movies) + ID + '.cdxl');
      if FileExists(IncludeTrailingPathDelimiter(Movies) + ID + '.cdxl') then
      begin
        with TStringList.Create do
        begin
          Text := Desc;
          SaveToFile(IncludeTrailingPathDelimiter(Movies) + ID + '.txt');
          Free;
        end;
      end;
    end
    else
      DoProgress(0, 'Error on convert');//writeln('no download');
  end;
  Mem.Free;
  SL.Free;
  except
    on e:Exception do
      writeln('Exception ' + E.MEssage);
  end;
  Synchronize(@DoOnEnd);
  Terminate;
end;

procedure TStartConvertThread.DoOnEnd;
begin
  if Assigned(OnEnd) then
    OnEnd(Self)
end;

procedure TSearchThread.DoOnEnd;
begin
  if Assigned(OnEnd) then
    OnEnd(Self);
end;

procedure TSearchThread.DoSyncProgress;
begin
  if Assigned(OnProgress) then
    OnProgress(Self, FPer, FTxt);
end;

procedure TSearchThread.DoProgress(APercent: integer; AText: string);
begin
  FPer := APercent;
  FTxt := AText;
  Synchronize(@DoSyncProgress);
end;

procedure TSearchThread.Execute;
var
  Url, SearchTerm, EncStr, na, id: string;
  i, Idx: Integer;
  Mem: TMemoryStream;
  jData, jData1: TJSONData;
  jObject: TJSONObject;
  SL: TStringList;
  Line: string;
begin
  DoProgress(0, 'Prepare Search');
  SearchTerm := StringReplace(Search, '/', ' ', [rfReplaceAll]);
  SearchTerm := StringReplace(SearchTerm, '?', ' ', [rfReplaceAll]);
  SearchTerm := StringReplace(SearchTerm, '#', ' ', [rfReplaceAll]);
  SearchTerm := StringReplace(SearchTerm, ':', ' ', [rfReplaceAll]);
  SearchTerm := StringReplace(SearchTerm, ';', ' ', [rfReplaceAll]);
  SearchTerm := AnsiToUTF8(SearchTerm);
  EncStr := '';
  for i := 1 to Length(SearchTerm) do
    EncStr := EncStr + '%' + IntToHex(Ord(SearchTerm[i]),2);
  Url := BaseURL + EncStr;
  Mem := TMemoryStream.Create;
  ErrMsg := '';
  IsError := True;
  try
    DoProgress(0, 'Send Search');
    if GetFile(Url, Mem) then
    begin
      DoProgress(0, 'Parse Search Result');
      Mem.Position := 0;
      SL := TStringList.create;
      SL.LoadFromStream(Mem);
      for i := 0 to SL.Count - 1 do
      begin
        DoProgress(Round(((i + 1) / SL.Count) * 100), 'Parse Search');
        Line := SL[i];
        jData := GetJSON(Line);
        jObject := TJSONObject(jData);
        Idx := Length(Results);
        SetLength(Results, Idx + 1);
        Results[Idx].Name := jObject.Get('fulltitle');
        Results[Idx].Id := jObject.Get('id');
        Results[Idx].Duration := jObject.Get('duration');
        Results[Idx].Desc := Results[Idx].Name + #10#10;
        try
        Results[Idx].Desc := Results[Idx].Desc + 'Uploader: ' + jObject.Get('uploader')+#10;
        except
        end;
        try
        Results[Idx].Desc := Results[Idx].Desc + 'Likes: ' + IntToStr(jObject.Get('like_count'))+#10;
        except
        end;
        try
        Results[Idx].Desc := Results[Idx].Desc + 'Views: ' + IntToStr(jObject.Get('view_count'))+#10;
        except
        end;
        try
        Results[Idx].Desc := Results[Idx].Desc + 'Licence: ' + jObject.Get('license')+#10;
        except
        end;
        try
        Results[Idx].Desc := Results[Idx].Desc + #10 + jObject.Get('description');
        except
        end;
        jData.Free;
      end;
      DoProgress(100, 'Search Done');
      IsError := False;
    end
    else
    begin
      ErrMsg := 'Error get URL';
    end;
  except
    on E:Exception do
      ErrMsg := E.Message;
  end;
  Mem.Free;



  Terminate;
  Synchronize(@DoOnEnd);
end;

{ TMainWindow }

procedure TMainWindow.SearchEntry(Sender: TObject);
begin
  SearchField.Disabled := True;
  List.NumRows := 0;
  List.NumColumns := 0;
  if Assigned(SearchThread) then
  begin
    SearchThread.Terminate;
    SearchThread.WaitFor;
    SearchThread.Free;
  end;
  SearchThread := TSearchThread.Create(True);
  SearchThread.Search := SearchField.Contents;
  SearchThread.OnProgress := @ProgressEvent;
  SearchThread.OnEnd := @EndThread;
  SearchThread.Start;
end;

//##### Endew Thread
procedure TMainWindow.EndThread(Sender: TObject);
var
  i: Integer;
  t, s: Integer;
begin
  ProgressEvent(Self, 0, 'Idle');
  if SearchThread.IsError then
  begin
    ShowMessage('SearchThread Error: ' + SearchThread.ErrMsg);
  end
  else
  begin
    // get lists
    List.Quiet := True;
    List.NumColumns := 4;
    List.NumRows := Length(SearchThread.Results);
    SetLength(Results, Length(SearchThread.Results));
    for i := 0 to List.NumRows - 1 do
    begin
      Results[i].id := SearchThread.Results[i].id;
      Results[i].Desc := SearchThread.Results[i].Desc;
      List.Cells[0, i] := IntToStr(i + 1);
      List.Cells[1, i] := UTF8ToAnsi(SearchThread.Results[i].Name);
      t := StrToIntDef(SearchThread.Results[i].Duration, 0);
      s := t mod 60;
      List.Cells[2, i] := IntToStr(t div 60) + ':' + Format('%2.2d',[s]);
      t := t * 150;
      if t > 1000 then
        List.Cells[3, i] := FloatToStrF(t/1024, ffFixed, 8,1) + ' MByte'
      else
        List.Cells[3, i] := IntToStr(t) + ' MByte'
    end;
    List.Quiet := False;
    SearchField.Contents := '';
  end;
  SearchField.Disabled := False;
end;

procedure TMainWindow.EndCThread(Sender: TObject);
begin
  ProgressEvent(Self, 0, 'Idle');
  if (List.Row >= 0) and (List.Row <= High(Results)) then
  begin
    Button.Disabled := FileExists(IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.cdxl');
    Button2.Disabled := not Button.Disabled;
  end
  else
  begin
    Button.Disabled := True;
    Button2.Disabled := True;
  end;
end;

procedure TMainWindow.ListClick(Sender: TObject);
begin
  if (List.Row >= 0) and (List.Row <= High(Results)) then
  begin
    TextOut.Text := Results[List.Row].Desc;
    Button.Disabled := FileExists(IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.cdxl');
    Button2.Disabled := not Button.Disabled;
  end
  else
  begin
    Button.Disabled := True;
    Button2.Disabled := True;
  end;
end;

procedure TMainWindow.ButtonClick(Sender: TObject);
var
  CT: TStartConvertThread;
begin
  Button.Disabled := True;
  if Assigned(ConvertThread) then
  begin
    ConvertThread.Terminate;
    ConvertThread.WaitFor;
    ConvertThread.Free;
  end;
  if (List.Row >= 0) and (List.Row <= High(Results)) then
  begin
    CT := TStartConvertThread.Create(True);
    CT.Desc := Results[List.Row].Desc;
    CT.OnProgress := @ProgressEvent;
    CT.Movies := Movies;
    CT.OnEnd := @EndCThread;
    CT.Id :=  Results[List.Row].ID;
    CT.Start;
    ConvertThread := CT;
  end;
end;

procedure TMainWindow.Button2Click(Sender: TObject);
var
  MyID: string;
  MovieName: string;
begin
  if (List.Row >= 0) and (List.Row <= High(Results)) then
  begin
    MyID := Results[List.Row].ID;
    MovieName := IncludeTrailingPathDelimiter(Movies) + MyID + '.cdxl';
    if FileExists(MovieName) then
    begin
      ExecuteProcess('SYS:Utilities/MultiView', MovieName, []);
    end;
  end;
end;

procedure TMainWindow.ProgressEvent(Sender: TObject; Percent: Integer; Text: string);
begin
  StatusLabel.Contents := Text;
  Progress.Current := Percent;
end;

constructor TMainWindow.Create;
var
  Grp1, Grp2: TMUIGroup;
begin
  inherited Create;
  ConvertThread := nil;
  SearchThread := nil;

  Movies := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + MovieFolder;

  if not DirectoryExists(Movies) then
    CreateDir(Movies);

  Horizontal := False;

  // Top Group;
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Horiz := False;
    Parent := Self;
  end;

  SearchField := TMUIString.Create;
  with SearchField do
  begin
    OnAcknowledge := @SearchEntry;
    Parent := Grp1;
  end;

  StatusLabel := TMUIText.Create('Idle');
  StatusLabel.Parent := Grp1;

  Progress := TMUIGauge.Create;
  with Progress do
  begin
    Horiz := True;
    Max := 100;
    FixHeight := 20;
    Current := 0;
    Parent := Grp1;
  end;

  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Horiz := True;
    Parent := Self;
  end;

  List := TMUIStringGrid.Create;
  with List do
  begin
    OnClick := @ListClick;
    Parent := Grp1;
  end;

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Horiz := False;
    Parent := Grp1;
  end;

  TextOut := TMUIFloatText.Create;
  with TextOut do
  begin
    Parent := Grp2;
  end;

  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Horiz := True;
    Parent := Grp2;
  end;

  Button := TMUIButton.Create('Download as CDXL');
  Button.OnClick := @ButtonClick;
  Button.Disabled := True;
  Button.Parent := Grp1;

  Button2 := TMUIButton.Create('Start Movie');
  Button2.OnClick := @Button2Click;
  Button2.Disabled := True;
  Button2.Parent := Grp1;
end;

destructor TMainWindow.Destroy;
begin
  if Assigned(SearchThread) then
  begin
    SearchThread.Terminate;
    SearchThread.WaitFor;
    SearchThread.Free;
  end;
  if Assigned(ConvertThread) then
  begin
    ConvertThread.Terminate;
    ConvertThread.WaitFor;
    ConvertThread.Free;
  end;
  inherited Destroy;
end;

begin
  TMainWindow.Create;


  MUIApp.Run;


end.

