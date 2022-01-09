program AmiTube;
// search and download Youtube videos to CDXL

{
TODO:
- ordering of Buttons -> cdxl ocs, cdxl aga, cdxl aga+ mpeg1
- settings to choose that he askes everytime for destination for the cdxl/mpeg as well
- Check moviedir on bootup
-> Laden des vorschaubilds während download -> absturz?
-> neue convert währen download- friert ein --> besser ablehnen
-> clipboard device wird geöffnet auch ohne das setting
}

{$mode objfpc}{$H+}
uses
  AThreads, clipboard, iffparse, AGraphics, Intuition, AmigaDos, Exec,
  Datatypes, Utility, workbench, icon, fgl,
  Classes, SysUtils, fphttpclient, mui, muihelper, SyncObjs,
  MUIClass.Base, MUIClass.Window, MUIClass.Group, MUIClass.Area, MUIClass.Gadget,
  MUIClass.Menu, MUIClass.DrawPanel,
  MUIClass.StringGrid, MUIClass.Dialog, MUIClass.List, filedownloadunit, prefsunit,
  XMLRead, DOM, AmiTubelocale, resolutionselunit;

const
  BaseURL = 'http://amitube.alb42.de/yt3/';

  SearchBase = 'ytsearch.php?q=';
  SearchBaseID = 'ytsearch.php?id=';
  ConvertBase = 'ytconvert.php?id=';
  ShareBase = 'ytshare.php?id=';
  SharedFile = 'ytshares.xml';
  IconBase = 'yticon.php?id=';
  DownloadBase = 'ytdownload.php?id=';

  UpdateURL = 'http://amitube.alb42.de/amitubeversion';

  MovieTemplateFolder = 'movies';

const
  VERSION = '$VER: AmiTube 0.8 beta1 (09.01.2022)';
  DownName: array[0..3] of string = ('CDXL OCS', 'CDXL AGA', 'MPEG1', 'CDXL AGA Large');
  DownSizes: array[0..3] of Integer = (150, 300, 170, 900);


type
  TMyVersion = record
    Major: Integer;
    Minor: Integer;
    isBeta: Boolean;
  end;

  TProgressEvent = procedure(Sender: TObject; Percent: Integer; Text: string) of object;

  { TSearchThread }

  TSearchThread = class(TThread)
  protected
    procedure DoOnEnd;
    procedure DoProgress(APercent: integer; AText: string);
    procedure Execute; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    SearchRes: TResultEntries;
    GetSharedList: Boolean;
    ErrMsg: string;
    IsError: Boolean;
    Search: string;
    OnEnd: TNotifyEvent;
    OnProgress: TProgressEvent;
  end;

  { TStartConvertThread }

  TStartConvertThread = class(TThread)
  private
    LastTime: Cardinal;
    procedure DoProgress(APercent: integer; AText: string);
    procedure ProgressUpdate(Sender: TObject; Percent, Speed: Integer; ASize: Int64);
  protected
    procedure Execute; override;
    procedure DoOnEnd;
  public
    //
    FormatID: string;
    Filename: string;
    //
    ID: string;
    Movies: string;
    Desc: string;
    Format: Integer;
    OnEnd: TNotifyEvent;
    OnProgress: TProgressEvent;
  end;


  { TMainWindow }

  TMainWindow = class(TMUIWindow)
  public
    SearchField: TMUIString;
    List: TMUIStringGrid;
    TextOut: TMUIFloatText;
    DownloadBtn: array[0..3] of TMUIButton;
    DownloadOriginal, PlayBtn, DeleteBtn, ShareBtn, StopButton: TMUIButton;
    SharedMenu: TMUIMenuItem;
    StatusLabel: TMUIText;
    Progress: TMUIGauge;
    Icon: TMUIDrawPanel;
    IconGrp: TMUIGroup;
    LoadIconBtn: TMUIButton;
    BtnGroup: TMUIGroup;
    StatText: TMUIText;
    procedure SearchEntry(Sender: TObject);
    procedure EndThread(Sender: TObject);
    procedure EndCThread(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure DownloadClick(Sender: TObject);
    procedure PlayClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure ProgressEvent(Sender: TObject; Percent: Integer; Text: string);
    procedure LoadLocalFiles(Sender: TObject);
    procedure QuitEvent(Sender: TObject);
    procedure PrefsStart(Sender: TObject);
    procedure StopAll(Sender: TObject);
    procedure TimerEvent(Sender: TObject);
    procedure FormatChangeEvent(Sender: TObject);
    procedure ShareClick(Sender: TObject);
    procedure LoadSharedList(Sender: TObject);
    procedure AboutMUI(Sender: TObject);
    procedure AboutAmiTube(Sender: TObject);
    procedure MUISettingsStart(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ClipTimerEvent(Sender: TObject);
    procedure ClipChanged(Sender: TObject);
    procedure CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
    procedure LoadIcon(Sender: TObject);
    procedure DrawIcon(Sender: TObject; RP: PRastPort; ARect: TRect);
    procedure GetOriginal(Sender: TObject);
    procedure CloseRes(Sender: TObject; var CloseAction: TCloseAction);
    procedure CheckForUpdate(Sender: TObject);
  private
    PlayFormat: Integer;
    DTObj: PObject_;
    IconName: string;
    DrawHandle: Pointer;
    ValLock: TCriticalSection;
    NewVal: Boolean;
    FPerc: Integer;
    FTxt: string;
    Timer: TMUITimer;
    ClipTimer: TMUITimer;
    Movies: string;
    MovieLock: BPTR;
    OldFreeAmount: Int64;
    ResultEntries: TResultEntries;
    SearchThread: TSearchThread;
    ConvertThread: TStartConvertThread;
    ImgSize: TPoint;
    BaseServer: string;
    procedure SetStatusText(AText: string; APos: LongInt = -1);
    procedure DestroyDTObj;

    procedure EnableDownloads(Enabled: Boolean; PlayButtons: Boolean);
    procedure UpdateDownloadBtns(Duration: Integer);
    procedure UpdateFreeMem;
    procedure StartDownload(AID, AFormatID, AFilename: string);
    procedure StartDownloadURL(URL, AFilename: string);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  //hp: TFPHTTPClient;
  HPsLock: TCriticalSection;
  HPs: TList;
  MyVersion: TMyVersion;

  SearchURL, SearchURLID, ConvertURL, ShareURL,
  SharedURL, IconURL, DownloadURL: string;

procedure MakeURLs(NewBaseURL: string);
begin
  SearchURL := NewBaseURL + SearchBase;
  SearchURLID := NewBaseURL + SearchBaseID;
  ConvertURL := NewBaseURL + ConvertBase;
  ShareURL := NewBaseURL + ShareBase;
  SharedURL := NewBaseURL + SharedFile;
  IconURL := NewBaseURL + IconBase;
  DownloadURL := NewBaseURL + DownloadBase;
end;
{
const
  AFF_68080 = 1 shl 10;

function CheckMe: Boolean; inline;
begin
  Result := (PExecBase(AOS_ExecBase)^.AttnFlags and AFF_68080) <> 0;
end;}

procedure KillSearch;
var
  i: Integer;
begin
  if Assigned(HPs) then
  begin
    HPsLock.Enter;
    try
      for i := 0 to HPs.Count - 1 do
        TFPHTTPClient(HPs[i]).Terminate;
    finally
      HPsLock.Leave;
    end;
  end;
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
    HPsLock.Enter;
    HPs.Add(hp);
    HPsLock.Leave;
    hp.AllowRedirect := True;
    hp.AddHeader('User-Agent', ShortVer + ' ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%});
    hp.Get(address, AStream);
    Result := True;
  finally
    HPsLock.Enter;
    HPs.Remove(HP);
    HPsLock.Leave;
    hp.Free;
    hp := nil;
  end;
end;

function MakeVersionNumber(s: string): TMyVersion;
begin
  s := Trim(s);
  Result.IsBeta := Pos('beta', s) > 0;
  if Pos(' ', s) > 0 then
    Delete(s, Pos(' ', s), Length(s));
  Result.Major := StrToIntDef(Copy(s, 1, Pos('.', s) - 1), -1);
  Result.Minor := StrToIntDef(Copy(s, Pos('.', s) + 1, Length(s)), -1);
end;

function PrintVersionNumber(Mv: TMyVersion): string;
begin
  Result := '';
  if (Mv.Major >= 0) and (MV.Minor >= 0) then
  begin
    Result := IntToStr(Mv.Major) + '.' + IntToStr(Mv.Minor);
    if Mv.IsBeta then
      Result := Result + ' beta';
  end;
end;

procedure TStartConvertThread.ProgressUpdate(Sender: TObject; Percent: integer; Speed: Integer; ASize: Int64);
var
  t1: Cardinal;
  s: string;
begin
  t1 := GetTickCount;
  if t1 - LastTime > 500 then
  begin
    s := GetLocString(MSG_STATUS_DOWNLOADING) + '...' + FloatToStrF(ASize/1000/1000, ffFixed, 8, 3) + ' MB';
    if Speed > 0 then
      s := s + ' @' + FloatToStrF(Speed/1000, ffFixed, 8,2) + ' kb/s';
    //
    if Percent = 0 then
      Percent := -1;
    DoProgress(Percent, s);
    //
    LastTime := GetTickCount;
  end;
end;

procedure TStartConvertThread.DoProgress(APercent: integer; AText: string);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, APercent, AText);
end;

procedure TStartConvertThread.Execute;
var
  Url, Ext: string;
begin
  try
    //writeln('start Thread ', FormatID);
    if FormatID = '' then
    begin
      DoProgress(0, GetLocString(MSG_STATUS_CONVERT));
      Url := ConvertURL + ID + '&format=' + IntToStr(Format);
      try
        if Format = 2 then
          Ext := '.mpeg'
        else
          Ext := '.cdxl';
        DonwloadFile(@ProgressUpdate, URL, IncludeTrailingPathDelimiter(Movies) + ID + Ext, True);
        //
        if FileExists(IncludeTrailingPathDelimiter(Movies) + ID + Ext) then
        begin
          if Terminated then
          begin
            DeleteFile(IncludeTrailingPathDelimiter(Movies) + ID + Ext);
            Exit;
          end;
          with TStringList.Create do
          begin
            Text := Desc;
            SaveToFile(IncludeTrailingPathDelimiter(Movies) + ID + '.txt');
            Free;
          end;
        end;
            //DoProgress(0, GetLocString(MSG_ERROR_CONVERT));//writeln('no download');
      except
        on e:Exception do
        begin
          writeln('Convert Thread Exception ' + E.Message);
        end;
      end;
    end
    else
    begin
      //writeln('start with FormatID');
      if Pos('http', FormatID) = 1 then
        Url := FormatID
      else
        Url := DownloadURL + ID + '&format=' + FormatID;
      //writeln('URL ', URL);
      DonwloadFile(@ProgressUpdate, Url, Filename, False);
      //writeln('done?');
    end;
  finally
    Synchronize(@DoOnEnd);
    Terminate;
  end;

end;

procedure TStartConvertThread.DoOnEnd;
begin
  if Assigned(OnEnd) then
    OnEnd(Self)
end;

procedure Tsearchthread.Doonend;
begin
  if Assigned(OnEnd) then
    OnEnd(Self);
end;


procedure Tsearchthread.Doprogress(Apercent: Integer; Atext: String);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, APercent, AText);
end;

function GetStringAttribute(ANode: TDOMNode; AttributeName: string; default: string = ''): string;
var
  Node: TDOMNode;
begin
  Result := Default;
  Node := ANode.Attributes.GetNamedItem(UniCodeString(AttributeName));
  if Assigned(Node) then
    Result := string(Node.NodeValue);
end;

procedure Tsearchthread.Execute;
var
  Url, SearchTerm, EncStr: string;
  i, Idx: Integer;
  Mem: TMemoryStream;
  Doc: TXMLDocument;
  Child, Node, FNode: TDOMNode;
  s: String;
  p: Integer;
  Count: Integer;
  AsID: Boolean;
  SRes: TResultEntry;
begin
  DoProgress(0, GetLocString(MSG_STATUS_PREPSEARCH));
  Doc := nil;
  Mem := Nil;
  try
    if GetSharedList then
    begin
      Url := SharedURL;
    end
    else
    begin
      AsID := False;
      Search := Trim(Search);
      p := Pos('v=', Search);
      if (Pos('https://', LowerCase(Trim(Search))) = 1) and (p > 0) then
      begin
        s := Copy(Search, P + 2, Length(Search));
        p := Pos('&', s);
        if p > 1 then
          Delete(s, p, Length(s));
        Search := s;
        AsId := True;
      end
      else
      if Pos('https://youtu.be/', LowerCase(Search)) = 1 then
      begin
        Delete(Search, 1, 17);
        AsId := True;
      end;
      SearchTerm := StringReplace(Search, '/', ' ', [rfReplaceAll]);
      SearchTerm := StringReplace(SearchTerm, '?', ' ', [rfReplaceAll]);
      SearchTerm := StringReplace(SearchTerm, '#', ' ', [rfReplaceAll]);
      SearchTerm := StringReplace(SearchTerm, ':', ' ', [rfReplaceAll]);
      SearchTerm := StringReplace(SearchTerm, ';', ' ', [rfReplaceAll]);
      SearchTerm := AnsiToUTF8(SearchTerm);
      EncStr := '';
      for i := 1 to Length(SearchTerm) do
        EncStr := EncStr + '%' + IntToHex(Ord(SearchTerm[i]),2);
      if AsId then
        Url := SearchURLID + EncStr
      else
        Url := SearchURL + EncStr + '&num=' + IntToStr(Prefs.NumSearch);
    end;
    Mem := TMemoryStream.Create;
    ErrMsg := '';
    IsError := True;
    try
      DoProgress(0, GetLocString(MSG_STATUS_SEARCH));
      if GetFile(Url, Mem) then
      begin
        if Terminated then
          Exit;
        DoProgress(0, GetLocString(MSG_STATUS_PARSESEARCH));
        {Mem.Position := 0;
        With TStringList.Create do
        begin
          LoadFromStream(Mem);
          Writeln(Text);
          Free;
        end;}
        try
          Mem.Position := 0;
          ReadXMLFile(Doc, Mem);
        except
          on E: Exception do
          begin
            writeln('Exception in ReadXMLFile ', E.Message);
            Mem.Position := 0;
            With TStringList.Create do
            begin
              LoadFromStream(Mem);
              Writeln(Text);
              Exit;
              Free;
            end;
          end;
        end;
        Child := Doc.DocumentElement.FirstChild;
        Count := Doc.DocumentElement.ChildNodes.Count;
        if Count = 0 then
          Count := 1;
        i := 0;
        while Assigned(Child) do
        begin
          DoProgress(Round(((i + 1) / Count) * 100), GetLocString(MSG_STATUS_PARSESEARCH));
          Inc(i);
          if Child.NodeName <> 'result' then
            Continue;
          SRes := TResultEntry.Create;
          SearchRes.Add(SRes);
          SRes.Name := GetStringAttribute(Child, 'fulltitle');
          SRes.Id := GetStringAttribute(Child, 'id');
          SRes.Icon := GetStringAttribute(Child, 'icon');
          SRes.Duration := StrToIntDef(GetStringAttribute(Child, 'duration'), 0);
          SRes.Desc := SRes.Name + #10#10;
          s := GetStringAttribute(Child, 'uploader');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Uploader: ' + s + #10;
          s := GetStringAttribute(Child, 'like_count');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Likes: ' + s + #10;
          s := GetStringAttribute(Child, 'view_count');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Views: ' + s + #10;
          s := GetStringAttribute(Child, 'license');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'License: ' + s + #10;
          Node := Child.FirstChild;
          while Assigned(Node) do
          begin
            if Node.NodeName = 'description' then
              SRes.Desc := SRes.Desc + #10 + string(Node.TextContent);
            if Node.NodeName = 'formats' then
            begin
              FNode := Node.FirstChild;
              while Assigned(FNode) do
              begin
                if FNode.NodeName = 'format' then
                begin
                  Idx := Length(SRes.Formats);
                  SetLength(SRes.Formats, Idx + 1);
                  SRes.Formats[idx].Title := GetStringAttribute(FNode, 'title');
                  SRes.Formats[idx].ACodec := GetStringAttribute(FNode, 'acodec');
                  SRes.Formats[idx].VCodec := GetStringAttribute(FNode, 'vcodec');
                  SRes.Formats[idx].URL := GetStringAttribute(FNode, 'url');
                  SRes.Formats[idx].Ext := GetStringAttribute(FNode, 'ext');
                  SRes.Formats[idx].FormatID := GetStringAttribute(FNode, 'format_id');
                end;
                FNode := FNode.NextSibling;
              end;
            end;
            Node := Node.NextSibling;
          // get formats
          end;



          Child := Child.NextSibling;
        end;
        DoProgress(100, GetLocString(MSG_STATUS_SEARCHDONE));
        IsError := False;
      end
      else
      begin
        ErrMsg := GetLocString(MSG_ERROR_GETURL);
      end;
    except
      on E:Exception do
        ErrMsg := E.Message;
    end;
  finally
    Doc.Free;
    Mem.Free;
    Terminate;
    Synchronize(@DoOnEnd);
  end;

end;

constructor Tsearchthread.Create;
begin
  inherited Create(True);
  SearchRes := TResultEntries.Create(False);
end;

destructor Tsearchthread.Destroy;
begin
  SearchRes.Free;
  inherited Destroy;
end;

{ TMainWindow }

procedure Tmainwindow.Searchentry(Sender: Tobject);
begin
  SearchField.Disabled := True;
  SharedMenu.Enabled := False;
  List.NumRows := 0;
  List.NumColumns := 0;
  if Assigned(SearchThread) then
  begin
    SearchThread.Terminate;
    SearchThread.WaitFor;
    SearchThread.Free;
  end;
  SearchThread := TSearchThread.Create;
  SearchThread.GetSharedList := False;
  SearchThread.Search := SearchField.Contents;
  SearchThread.OnProgress := @ProgressEvent;
  SearchThread.OnEnd := @EndThread;
  SearchThread.Start;
  StopButton.Disabled := False;
  Timer.Enabled := True;
end;

//##### Endew Thread
procedure Tmainwindow.Endthread(Sender: Tobject);
var
  i: Integer;
  t, s: Integer;
  st: string;
  SRes: TResultEntry;
begin
  ProgressEvent(Self, 0, GetLocString(MSG_STATUS_IDLE));
  if SearchThread.IsError then
  begin
    ShowMessage('SearchThread Error: ' + SearchThread.ErrMsg);
  end
  else
  begin
    // get lists
    List.Quiet := True;
    List.NumColumns := 4;
    List.NumRows := SearchThread.SearchRes.Count;
    ResultEntries.Clear;
    for i := 0 to List.NumRows - 1 do
    begin
      SRes := SearchThread.SearchRes[i];
      ResultEntries.Add(Sres);
      List.Cells[0, i] := IntToStr(i + 1);
      st := SRes.Name;
      if Length(st) > Prefs.MaxTitleLen + 3 then
        st := Copy(st, 1, Prefs.MaxTitleLen) + '...';
      List.Cells[1, i] := UTF8ToAnsi(st);
      t := SRes.Duration;
      s := t mod 60;
      List.Cells[2, i] := IntToStr(t div 60) + ':' + Format('%2.2d',[s]) + ' ';
      t := t * DownSizes[Prefs.Format];
      if t > 1000 then
        List.Cells[3, i] := FloatToStrF(t/1024, ffFixed, 8,1) + ' MB '
      else
        List.Cells[3, i] := IntToStr(t) + ' kB '
    end;
    List.Quiet := False;
    SearchField.Contents := '';
  end;
  SearchField.Disabled := False;
  SharedMenu.Enabled := True;
  StopButton.Disabled := True;
  TimerEvent(Timer);
  Timer.Enabled := False;
end;



procedure Tmainwindow.Endcthread(Sender: Tobject);
var
  i: Integer;
  SRes: TResultEntry;
begin
  ProgressEvent(Self, 0, GetLocString(MSG_STATUS_IDLE));

  for i := 0 to ResultEntries.Count do
  begin
    SRes := ResultEntries[i];
    if ConvertThread.Id = SRes.Id then
    begin
      List.Row := i;
      Break;
    end;
  end;
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    PlayFormat := 0;
    if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.mpeg') then
      PlayFormat := 2
    else
      if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.cdxl') then
        PlayFormat := 1;
    EnableDownloads(PlayFormat = 0, PlayFormat > 0);
  end
  else
  begin
    EnableDownloads(False, False);
  end;
  StopButton.Disabled := True;
  TimerEvent(Timer);
  Timer.Enabled := False;
  ClipChanged(nil);
  if Prefs.AutoStart and (not PlayBtn.Disabled) and (ConvertThread.FormatID = '') then
    PlayClick(PlayBtn);
  //if ConvertThread.FormatID <> '' then
  //  ShowMessage('Download Done');
end;

procedure Tmainwindow.Listclick(Sender: Tobject);
begin
  Destroydtobj;
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    TextOut.Text := UTF8ToAnsi(ResultEntries[List.Row].Desc);
    PlayFormat := 0;
    if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.mpeg') then
      PlayFormat := 2
    else
      if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.cdxl') then
        PlayFormat := 1;
    EnableDownloads(PlayFormat = 0, PlayFormat > 0);
    //
    if Prefs.AutoIcon then
      LoadIcon(Sender);
  end
  else
  begin
    EnableDownloads(False, False);
  end;
end;

procedure Tmainwindow.Downloadclick(Sender: Tobject);
var
  CT: TStartConvertThread;
  Format, i: Integer;
  FileSize: Int64;
begin
  if Assigned(ConvertThread) then
  begin
    if not ConvertThread.Terminated then
    begin
      ShowMessage(GetLocString(MSG_ERROR_ALREADYRUN));
      Exit;
    end;
    ConvertThread.WaitFor;
    ConvertThread.Free;
  end;
  Format := 0;
  if Sender is TMUIButton then
    Format := TMUIButton(Sender).Tag;
  for i := 0 to High(DownloadBtn) do
    DownloadBtn[i].Disabled := True;
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    FileSize := Int64(ResultEntries[List.Row].Duration) * DownSizes[Format] * 1024;
    UpdateFreeMem;
    if FileSize > OldFreeAmount then
    begin
      if MessageBox('Error', GetLocString(MSG_ERROR_NO_SPACE), [GetLocString(MSG_GUI_YES), GetLocString(MSG_GUI_NO)]) <> 1 then
      begin
        EnableDownloads(True, False);
        Exit;
      end;
    end;
    CT := TStartConvertThread.Create(True);
    CT.Desc := ResultEntries[List.Row].Desc;
    CT.Format := Format;
    CT.OnProgress := @ProgressEvent;
    CT.Movies := Movies;
    CT.OnEnd := @EndCThread;
    CT.Id :=  ResultEntries[List.Row].ID;
    CT.Start;
    ConvertThread := CT;
    StopButton.Disabled := False;
  end;
  Timer.Enabled := True;
end;

var
  LastStart: Cardinal = 0;

procedure Tmainwindow.Playclick(Sender: Tobject);
var
  MyID: string;
  MovieName: string;
  Param: string;
begin
  if GetTickCount - LastStart < 200 then
    Exit;
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    MyID := ResultEntries[List.Row].ID;
    if PlayFormat = 2 then
    begin
      MovieName := IncludeTrailingPathDelimiter(Movies) + MyID + '.mpeg';
      if FileExists(MovieName) then
      begin
        Param := Prefs.MPEGPlayerParam;
        Param := StringReplace(Param, '%f', '"' + MovieName + '"', [rfReplaceAll]);
        ExecuteProcess(Prefs.MPEGPlayerPath, Param, []);
        LastStart := GetTickCount;
      end;
    end
    else
    begin
      MovieName := IncludeTrailingPathDelimiter(Movies) + MyID + '.cdxl';
      if FileExists(MovieName) then
      begin
        Param := Prefs.PlayerParam;
        Param := StringReplace(Param, '%f', '"' + MovieName + '"', [rfReplaceAll]);
        ExecuteProcess(Prefs.PlayerPath, Param, []);
        LastStart := GetTickCount;
      end;
    end;

  end;
end;

procedure Tmainwindow.Deleteclick(Sender: Tobject);
var
  MyID: string;
  MovieName, ReadMeName, MPEGName: string;
begin
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    MyID := ResultEntries[List.Row].ID;
    MovieName := IncludeTrailingPathDelimiter(Movies) + MyID + '.cdxl';
    MPEGName := IncludeTrailingPathDelimiter(Movies) + MyID + '.mpeg';
    ReadmeName := IncludeTrailingPathDelimiter(Movies) + MyID + '.txt';

    if MessageBox(GetLocString(MSG_GUI_DELETE), GetLocString(MSG_GUI_DELETE)+ #10' "' + List.Cells[1, List.Row] + '"?' , [GetLocString(MSG_GUI_YES), GetLocString(MSG_GUI_NO)]) = 1 then
    begin
      if FileExists(MovieName) then
        DeleteFile(MovieName);
      if FileExists(MPEGName) then
        DeleteFile(MPEGName);
      if FileExists(ReadMeName) then
        DeleteFile(ReadMeName);
      EnableDownloads(True, False);
    end;
  end;
end;

procedure Tmainwindow.Progressevent(Sender: Tobject; Percent: Integer; Text: String);
begin
  ValLock.Enter;
  try
    NewVal := True;
    FTxt := Text;
    FPerc := Percent;
  finally
    ValLock.Leave;
  end;
end;

procedure Tmainwindow.Loadlocalfiles(Sender: Tobject);
var
  Info: TSearchRec;
  FileName, TXTFilename: string;
  SL: TStringList;
  MyRes: array of record
    Name: string;
    id: string;
    Desc: string;
    Size: string;
    Format: Integer;
  end;
  Idx,i : Integer;
  st: string;
  Format: Integer;
  SRes: TResultEntry;
begin
  // make a list
  MyRes := [];
  SL := TStringList.Create;
  if FindFirst (IncludeTrailingPathDelimiter(Movies) + '*.*', faAnyFile, Info) = 0 then
  begin
    repeat
      Format := 0;
      if ExtractFileExt(Info.Name) = '.mpeg' then
        Format := 2
      else
        if ExtractFileExt(Info.Name) = '.cdxl' then
          Format := 1;
      if Format = 0 then
        Continue;
      Filename := IncludeTrailingPathDelimiter(Movies) + Info.Name;
      TxtFilename := ChangeFileExt(FileName, '.txt');
      if FileExists(TXTFilename) then
      begin
        Idx := Length(MyRes);
        SetLength(MyRes, Idx + 1);
        MyRes[Idx].Id := ExtractFilename(ChangeFileExt(TXTFilename, ''));
        if (Info.size / 1024) > 1024 then
          MyRes[Idx].Size := FloatToStrF(Info.Size / 1024 / 1024, ffFixed, 8, 1) + ' MByte'
        else
          MyRes[Idx].Size := IntToStr(Round(Info.Size / 1024)) + ' kByte';
        MyRes[Idx].Format := Format;
        SL.Clear;
        SL.LoadFromFile(TxtFileName);
        if SL.Count > 0 then
          MyRes[Idx].Name := SL[0];
        MyRes[Idx].Desc := SL.Text;
      end;
    Until FindNext(info)<>0;
    FindClose(Info);
  end;

  if Length(MyRes) > 0 then
  begin
    List.NumRows := 0;
    ResultEntries.Clear;

    List.Quiet := True;
    List.NumColumns := 3;
    List.NumRows := Length(MyRes);
    //SetLength(Results, Length(MyRes));
    for i := 0 to List.NumRows - 1 do
    begin
      SRes := TResultEntry.Create;
      ResultEntries.Add(SRes);
      SRes.id := MyRes[i].id;
      SRes.Desc := MyRes[i].Desc;
      SRes.Duration := 0;
      List.Cells[0, i] := IntToStr(i + 1);
      st := MyRes[i].Name;
      if Length(st) > Prefs.MaxTitleLen + 3 then
        st := Copy(st, 1, Prefs.MaxTitleLen) + '...';
      List.Cells[1, i] := UTF8ToAnsi(st);
      List.Cells[2, i] := MyRes[i].Size;
    end;
    List.Quiet := False;
    SearchField.Contents := '';
  end
  else
    ShowMessage(GetLocString(MSG_ERROR_LOCAL));
  SL.Free;
end;

procedure Tmainwindow.Quitevent(Sender: Tobject);
begin
  Close;
end;

procedure Tmainwindow.Prefsstart(Sender: Tobject);
begin
  Prefs.UpdateSettings;
  Prefs.Open := True;
end;

procedure Tmainwindow.Stopall(Sender: Tobject);
begin
  if Assigned(SearchThread) then
    SearchThread.Terminate;
  if Assigned(ConvertThread) then
    ConvertThread.Terminate;
  KillSearch;
  KillDownload;
  StopButton.Disabled := True;
end;

procedure Tmainwindow.Timerevent(Sender: Tobject);
begin
  ValLock.Enter;
  try
    if NewVal then
    begin
      StatusLabel.Contents := FTxt;
      Progress.Current := FPerc;
      NewVal := False;
    end;
  finally
    ValLock.Leave;
  end;

end;

procedure Tmainwindow.Formatchangeevent(Sender: Tobject);
var
  t,i : Integer;
begin
  {if Prefs.Format = 2 then
    DownloadBtn.Contents := GetLocString(MSG_GUI_DOWNLOAD_MPEG)
  else
    DownloadBtn.Contents := GetLocString(MSG_GUI_DOWNLOAD_CDXL);}
  if List.NumColumns > 3 then
  begin
    for i := 0 to ResultEntries.Count - 1 do
    begin
      t := ResultEntries[i].Duration;
      t := t * DownSizes[Prefs.Format];
      if t > 1000 then
        List.Cells[3, i] := FloatToStrF(t/1024, ffFixed, 8,1) + ' MB'
      else
        List.Cells[3, i] := IntToStr(t) + ' kB'
    end;
  end;
end;

procedure Tmainwindow.Shareclick(Sender: Tobject);
var
  s, Url, MyID, EncStr: string;
  hp: TFPHTTPClient;
  i: Integer;
begin
  //
  ShareBtn.Disabled := True;
  hp := nil;
  try
    if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
    begin
      MyID := ResultEntries[List.Row].ID;
      MyID := StringReplace(MyID, '/', ' ', [rfReplaceAll]);
      MyID := StringReplace(MyID, '?', ' ', [rfReplaceAll]);
      MyID := StringReplace(MyID, '#', ' ', [rfReplaceAll]);
      MyID := StringReplace(MyID, ':', ' ', [rfReplaceAll]);
      MyID := StringReplace(MyID, ';', ' ', [rfReplaceAll]);
      EncStr := '';
      for i := 1 to Length(MyID) do
        EncStr := EncStr + '%' + IntToHex(Ord(MyID[i]),2);
      Url := ShareURL + EncStr;
      try
        hp := TFPHTTPClient.Create(nil);
        hp.AllowRedirect := True;
        hp.AddHeader('User-Agent', ShortVer + ' ' +  {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%});
        s := hp.Get(URL);
        ShowMessage(GetLocString(MSG_STATUS_SHARED) + #10 + s);
      except
        On E:Exception do
          ShowMessage(GetLocString(MSG_ERROR_SHARE)+ ' ' + E.Message);
      end;
    end;
  finally
    hp.Free;
    ShareBtn.Disabled := False;
  end;

end;

procedure Tmainwindow.Loadsharedlist(Sender: Tobject);
begin
  begin
  SearchField.Disabled := True;
  SharedMenu.Enabled := False;
  List.NumRows := 0;
  List.NumColumns := 0;
  if Assigned(SearchThread) then
  begin
    SearchThread.Terminate;
    SearchThread.WaitFor;
    SearchThread.Free;
  end;
  SearchThread := TSearchThread.Create;
  SearchThread.GetSharedList := True;
  SearchThread.OnProgress := @ProgressEvent;
  SearchThread.OnEnd := @EndThread;
  SearchThread.Start;
  StopButton.Disabled := False;
  Timer.Enabled := True;
end;

end;

procedure Tmainwindow.Aboutmui(Sender: Tobject);
begin
  MUIApp.AboutMUI;
end;

procedure Tmainwindow.Aboutamitube(Sender: Tobject);
var
  s: string;
begin
  s := (MUIX_C + #10 + MUIX_B + '---   ' + ShortVer + '   ---' + MUIX_N+ #10#10 +
       'made with Free Pascal for Amiga by ALB42'#10 +
       'special thanks to Michal Bergseth for idea and encouragement.'#10#10 +
       'Check ' + MUIX_U + 'https://blog.alb42.de' + MUIX_N + ' for updates.'#10);
  if BaseServer <> '' then
    s := s + 'Used Server: ' + BaseServer + #10;
  ShowMessage(s);
end;

procedure Tmainwindow.Muisettingsstart(Sender: Tobject);
begin
  MUIApp.OpenConfigWindow;
end;

procedure Tmainwindow.Formshow(Sender: Tobject);
begin
  case Prefs.Startup of
    1: LoadLocalFiles(nil);
    2: LoadSharedList(nil);
  end;
  Prefs.OnClipChanged := @ClipChanged;
  ClipChanged(nil);
end;

const
  ID_FTXT = 1179932756;
  ID_CHRS = 1128813139;

function GetTextFromClip(ClipUnit: Byte): AnsiString;
var
  Iff: PIffHandle;
  Error: LongInt;
  Cn: PContextNode;
  Buf: PChar;
  Len: Integer;
  Cu: LongInt;
begin
  GetTextFromClip := '';
  Cu := ClipUnit;
  Iff := AllocIff;
  if Assigned(Iff) then
  begin
    Iff^.iff_Stream := NativeUInt(OpenClipboard(Cu));
    if Iff^.iff_Stream<>0 then
    begin
      InitIffAsClip(iff);
      if OpenIff(Iff, IFFF_READ) = 0 then
      begin
        if StopChunk(iff, ID_FTXT, ID_CHRS) = 0 then
        begin
          while True do
          begin
            Error := ParseIff(iff, IFFPARSE_SCAN);
            if (Error <> 0) and (Error <> IFFERR_EOC) then
              Break;
            Cn := CurrentChunk(Iff);
            if not Assigned(Cn) then
            begin
              Continue;
            end;
            Len := Cn^.cn_Size;
            if (Cn^.cn_Type = ID_FTXT) and (Cn^.cn_ID = ID_CHRS) and (Len > 0) then
            begin
              GetMem(Buf, Len + 1);
              FillChar(Buf^, Len + 1, #0);
              ReadChunkBytes(Iff, Buf, Len);
              GetTextFromClip := GetTextFromClip + AnsiString(Buf);
              FreeMem(Buf);
            end;
          end;
        end;
        CloseIff(Iff);
      end;
      CloseClipboard(PClipBoardHandle(iff^.iff_Stream));
    end;
    FreeIFF(Iff);
  end;
end;

var
  LastClip: string;

procedure Tmainwindow.Cliptimerevent(Sender: Tobject);
var
  s: string;
begin
  ClipTimer.Enabled := False;
  try
    UpdateFreeMem;
    //
    if Assigned(SearchThread) and (not SearchThread.Terminated) or SearchField.Disabled then
      Exit;
    if Prefs.ObserveClip then
    begin
      s := GetTextFromClip(PRIMARY_CLIP);
      if s = LastClip then
        Exit;
      LastClip := s;
      s := Trim(s);
      if ((Pos('https://', lowercase(s)) = 1) and (Pos('youtube', lowercase(s)) > 0)) or (Pos('https://youtu.be/', lowercase(s)) = 1) then
      begin
        if MessageBox(GetLocString(MSG_GUI_GOTURL), StringReplace(GetLocString(MSG_GUI_GOTURLTEXT), '%s', s, [rfReplaceAll]), [GetLocString(MSG_GUI_YES), GetLocString(MSG_GUI_NO)]) = 1 then
        begin
          SearchField.Contents := s;
          SearchEntry(SearchField);
        end;
      end;
    end;
  finally
    ClipTimer.Enabled := True;
  end;
end;

procedure Tmainwindow.Clipchanged(Sender: Tobject);
begin
  ClipTimer.Enabled := True;
end;

procedure Tmainwindow.Closewindow(Sender: Tobject; var Closeaction: Tcloseaction);
begin
  Prefs.SaveSettings;
end;

procedure Tmainwindow.Loadicon(Sender: Tobject);
var
  URL: string;
  FS: TFileStream;
  bmhd: PBitMapHeader;
  bm: PBitMap;
  Filename: string;
begin
  Destroydtobj;
  if (List.Row>=0) and (List.Row < ResultEntries.Count) then
  begin
    Setstatustext('Load Icon');
    URL := IconURL + ResultEntries[List.Row].ID;

    if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.jpg') then
    begin
      Filename := IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.jpg';
      IconName := '';
    end
    else
    begin
      if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.txt') then
      begin
        Filename := IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.jpg';
        IconName := '';
      end
      else
      begin
        IconName := 'T:' + ResultEntries[List.Row].ID + '.jpg';
        FileName := IconName;
      end;

      if FileExists(Filename) then
        DeleteFile(Filename);
      FS := TFileStream.Create(Filename, fmCreate);
      try
      GetFile(URL, FS);
      except
        on E:Exception do
        begin
          SetStatusText(GetLocString(MSG_ERROR_LOAD_ICON) + '(' + E.Message + ')');
          FS.Free;
          DeleteFile(Filename);
          Exit;
        end;
      end;
      if FS.Size = 0 then
      begin
        SetStatusText(GetLocString(MSG_ERROR_LOAD_ICON) + '(1)');
        FS.Free;
        DeleteFile(Filename);
      end
      else
        FS.Free;
    end;
    DTObj := NewDTObject(PChar(FileName), [
        DTA_GroupID, GID_PICTURE,
        PDTA_Remap, AsTag(TRUE),
        PDTA_DestMode,PMODE_V43,
        PDTA_Screen, AsTag(Self.Screen),
        OBP_Precision, Precision_Image,
        TAG_END, TAG_END]);
    if not Assigned(DTObj) then
    begin
      SetStatusText(GetLocString(MSG_ERROR_LOAD_ICON) + '(2)');
      if IconName <> '' then
        DeleteFile(IconName);
      Exit;
    end;
    DoMethod(DTObj, [DTM_PROCLAYOUT, 0 , 1]);
    GetDTAttrs(DTObj,
      [
      PDTA_DestBitMap, AsTag(@bm),
      PDTA_BitMapHeader,AsTag(@bmhd),
      TAG_END]);
    if not Assigned(bm) or not Assigned(bmhd) then
    begin
      SetStatusText(GetLocString(MSG_ERROR_LOAD_ICON) + '(3)');
      Exit;
    end;
    ImgSize.x := bmhd^.bmh_Width;
    ImgSize.Y := bmhd^.bmh_Height;
    DrawHandle := ObtainDTDrawInfoA(DTObj, nil);

    Icon.ShowMe := True;
    LoadIconBtn.ShowMe := False;
    IconGrp.InitChange;
    Icon.MinHeight := ImgSize.Y;
    Icon.MaxHeight := ImgSize.Y;
    Icon.MinWidth := ImgSize.X;
    Icon.MaxWidth := ImgSize.X;
    IconGrp.ExitChange;
    Setstatustext(GetLocString(MSG_STATUS_IDLE));
  end;
end;

procedure Tmainwindow.Drawicon(Sender: Tobject; Rp: Prastport; Arect: Trect);
var
  s: string;
begin
  //
  //sysdebugln('draw ' + IntToStr(ARect.Left) + '; ' + IntToStr(AREct.Top) + ' size = ' + IntToStr(ImgSize.X) + '; ' + IntToStr(ImgSize.Y));
  if Assigned(DTObj) then
    DrawDTObjectA(RP, DTObj, ARect.Left, ARect.Top, ImgSize.x, ImgSize.y, 0, 0, nil)
  else
  begin
    SetRast(RP, 0);
    GFXMove(RP, ARect.Left + 2, ARect.Top + Icon.Height div 2);
    s := GetLocString(MSG_GUI_LOAD_ICON);
    SetDrmd(RP, JAM1);
    SetAPen(RP, 1);
    GfxText(RP, PChar(S), Length(s));
  end;
  //
end;

procedure Tmainwindow.Getoriginal(Sender: Tobject);
var
  i, Count: Integer;
  EncStr, URL: string;
  SRes: TResultEntry;
  Mem: TMemoryStream;
  Doc: TXMLDocument;
  Idx: Integer;
  Child, Node, FNode: TDOMNode;
  s: string;
begin
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    Self.Sleep := True;
    SRes := ResultEntries[List.Row];
    // no formats until now, so go and get them
    if Length(SRes.Formats) = 0 then
    begin
      EncStr := '';
      for i := 1 to Length(SRes.ID) do
        EncStr := EncStr + '%' + IntToHex(Ord(SRes.ID[i]),2);
      Url := SearchURLID + EncStr;
      Mem := TMemoryStream.Create;
      try
        if GetFile(Url, Mem) then
        begin
          try
            Mem.Position := 0;
            ReadXMLFile(Doc, Mem);
          except
            on E: Exception do
            begin
              writeln('Exception in ReadXMLFile ', E.Message);
              Mem.Position := 0;
              With TStringList.Create do
              begin
                LoadFromStream(Mem);
                Writeln(Text);
                Exit;
                Free;
              end;
            end;
          end;
          Child := Doc.DocumentElement.FirstChild;
          Count := Doc.DocumentElement.ChildNodes.Count;
        if Count = 0 then
          Count := 1;
        i := 0;
        if Assigned(Child) then
        begin
          SRes.Name := GetStringAttribute(Child, 'fulltitle');
          SRes.Id := GetStringAttribute(Child, 'id');
          SRes.Icon := GetStringAttribute(Child, 'icon');
          SRes.Duration := StrToIntDef(GetStringAttribute(Child, 'duration'), 0);
          SRes.Desc := SRes.Name + #10#10;
          s := GetStringAttribute(Child, 'uploader');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Uploader: ' + s + #10;
          s := GetStringAttribute(Child, 'like_count');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Likes: ' + s + #10;
          s := GetStringAttribute(Child, 'view_count');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Views: ' + s + #10;
          s := GetStringAttribute(Child, 'license');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'License: ' + s + #10;
          Node := Child.FirstChild;
          while Assigned(Node) do
          begin
            if Node.NodeName = 'description' then
              SRes.Desc := SRes.Desc + #10 + string(Node.TextContent);
            if Node.NodeName = 'formats' then
            begin
              FNode := Node.FirstChild;
              while Assigned(FNode) do
              begin
                if FNode.NodeName = 'format' then
                begin
                  Idx := Length(SRes.Formats);
                  SetLength(SRes.Formats, Idx + 1);
                  SRes.Formats[idx].Title := GetStringAttribute(FNode, 'title');
                  SRes.Formats[idx].ACodec := GetStringAttribute(FNode, 'acodec');
                  SRes.Formats[idx].VCodec := GetStringAttribute(FNode, 'vcodec');
                  SRes.Formats[idx].URL := GetStringAttribute(FNode, 'url');
                  SRes.Formats[idx].Ext := GetStringAttribute(FNode, 'ext');
                  SRes.Formats[idx].FormatID := GetStringAttribute(FNode, 'format_id');
                end;
                FNode := FNode.NextSibling;
              end;
            end;
            Node := Node.NextSibling;
          // get formats
          end;
          Child := Child.NextSibling;
        end;
        end;
      finally
        Mem.Free;
      end;
    end;
    ResWin.FOnStartDownLoad := @Startdownload;
    ResWin.OnCloseRequest := @CloseRes;
    ResWin.Openreslist(SRes);
  end;
end;

procedure Tmainwindow.Closeres(Sender: Tobject; var Closeaction: Tcloseaction);
begin
  //
  Self.Sleep := False;
end;

function IsNewerVersion(NewVers: TMyVersion): Boolean;
begin
  Result := NewVers.Major > MyVersion.Major;
  if Result then
    Exit;
  if (NewVers.Major = MyVersion.Major) then
  begin
    Result := NewVers.Minor > MyVersion.Minor;
    if Result then
      Exit;
    if NewVers.Minor = MyVersion.Minor then
    begin
      if (not NewVers.IsBeta) and MyVersion.isBeta then
        Result := True;
    end;
  end;
end;

procedure Tmainwindow.Checkforupdate(Sender: Tobject);
var
  Mem: TMemoryStream;
  OnlineVersion, Link, s, TargetName: string;
begin
  Mem := TMemoryStream.Create;
  try
    try
      OnlineVersion := '';
      Link := '';
      if GetFile(UpdateURL, Mem) then
      begin
        Mem.Position := 0;

        with TStringList.Create do
        begin
          LoadFromStream(Mem);
          if Count > 0 then
            OnlineVersion := Strings[0];
          {$ifdef AROS}
          if Count > 2 then
            Link := Strings[2];
          {$else}
          if Count > 1 then
            Link := Strings[1];
          {$endif}
          Free;
        end;
        //
        //Check for Version
        if IsNewerVersion(MakeVersionNumber(OnlineVersion)) then
        begin
          TargetName := '';
          s := StringReplace(GetLocString(MSG_GUI_UPDATEAVAIL), '%o', PrintVersionNumber(MyVersion), [rfReplaceAll]);
          s := StringReplace(s, '%n', OnlineVersion, [rfReplaceAll]);
          s := StringReplace(s, '\n', #10, [rfReplaceAll]);
          if MessageBox('Update', s, [GetLocString(MSG_GUI_YES), GetLocString(MSG_GUI_NO)]) = 1 then
          begin
            s := ExtractFilename(Link);
            with TFileDialog.Create do
            begin
              TitleText := GetLocString(MSG_GUI_SELECTFILE);//'Select name/path for file.';
              Pattern := '#?' + ExtractFileExt(s);
              Directory := LastDir;
              Filename := s;
              SaveMode := True;
              if Execute then
              begin
                LastDir := IncludeTrailingPathDelimiter(Directory);
                TargetName := Filename;
              end;
              Free;
            end;
            if TargetName <> '' then
              StartDownloadURL(Link, TargetName);
          end;
      begin
        EnableDownloads(True, False);
        Exit;
      end;
        end
        else
        begin
          ShowMessage(GetLocString(MSG_GUI_NOUPDATE));
        end;
      end;
      if (Link = '') or (OnlineVersion = '') then
      begin
        ShowMessage(GetLocString(MSG_ERROR_UPDATE));
      end;
    except
      on E: Exception do
      begin
        ShowMessage(GetLocString(MSG_ERROR_UPDATE) + ' ' + E.Message);
      end;
    end;
  finally
    Mem.Free;
  end;
end;

procedure Tmainwindow.Setstatustext(Atext: String; Apos: Longint);
begin
  StatusLabel.Contents := AText;
  if APos >= 0 then
    Progress.Current := APos;
end;

procedure Tmainwindow.Destroydtobj;
begin
  if Assigned(DTObj) then
  begin
    ReleaseDTDrawInfo(DTObj, DrawHandle);
    DisposeDTObject(DTObj);
    if IconName <> '' then
      DeleteFile(IconName);
  end;
  DTObj := nil;
  DrawHandle := nil;
  IconName := '';
  Icon.ShowMe := False;
  LoadIconBtn.ShowMe := True;
end;

procedure Tmainwindow.Enabledownloads(Enabled: Boolean; Playbuttons: Boolean);
var
  i: Integer;
begin
  BtnGroup.InitChange;
  if Prefs.AllFormats then
  begin
    for i := low(DownloadBtn) to High(DownloadBtn) do
    begin
      DownloadBtn[i].Disabled := False;
      DownloadBtn[i].ShowMe := Enabled;
    end;
  end
  else
  begin
    for i := low(DownloadBtn) to High(DownloadBtn) do
    begin
      DownloadBtn[i].Disabled := False;
      DownloadBtn[i].ShowMe := Enabled and (Prefs.Format = i);
    end;
  end;
  PlayBtn.ShowMe := PlayButtons;
  DeleteBtn.ShowMe := PlayButtons;
  ShareBtn.ShowMe := PlayButtons;
  if Enabled then
  begin
    if (List.Row >= 0) and (List.Row < ResultEntries.Count) and Prefs.AllFormats then
      Updatedownloadbtns(ResultEntries[List.Row].Duration)
    else
      Updatedownloadbtns(0);
  end;
  DownloadOriginal.Disabled := not(Enabled or PlayButtons);
  BtnGroup.ExitChange;
end;

procedure Tmainwindow.Updatedownloadbtns(Duration: Integer);
var
  t, i: Integer;
  s: string;
begin
  for i := 0 to High(DownSizes) do
  begin
    t := DownSizes[i] * Duration;
    s := '';
    if (Prefs.Format = i) and Prefs.AllFormats then
      s := MUIX_B;
    s := s + GetLocString(MSG_GUI_DOWNLOAD_AS) + ' ' + DownName[i];
    if t > 0 then
      if t > 1024 then
        s := s + ' ' + FloatToStrF(t/1024, ffFixed, 8,1) + ' MByte'
      else
        s := s + ' ' + IntToStr(t) + ' kByte';
    DownloadBtn[i].Contents := s;
  end;
end;

procedure Tmainwindow.Updatefreemem;
var
  InfoData: TInfoData;
  FreeAmount: Int64;
  f: Single;
begin
  //
  Info(MovieLock, @InfoData);
  FreeAmount := (Int64(InfoData.id_NumBlocks) - Int64(InfoData.id_NumBlocksUsed)) * InfoData.id_BytesPerBlock;

  if FreeAmount <> OldFreeAmount then
  begin
    OldFreeAmount := FreeAmount;
    if FreeAmount = 0 then
    begin
      StatText.Contents := MUIX_R +  GetLocString(MSG_GUI_DISK_FULL);
      Exit;
    end;
    if FreeAmount < 1024 then
    begin
      StatText.Contents := MUIX_R + IntToStr(FreeAmount) + ' byte ' + GetLocString(MSG_GUI_FREE);
      Exit;
    end;
    f := FreeAmount / 1024;
    if f < 1024 then
    begin
      StatText.Contents := MUIX_R + FloatToStrF(F, ffFixed, 8,1) + ' kB ' + GetLocString(MSG_GUI_FREE);
      Exit;
    end;
    f := f / 1024;
    if f < 1024 then
    begin
      StatText.Contents := MUIX_R + FloatToStrF(F, ffFixed, 8,1) + ' MB ' + GetLocString(MSG_GUI_FREE);
      Exit;
    end;
    f := f / 1024;
    StatText.Contents := MUIX_R + FloatToStrF(F, ffFixed, 8,1) + ' GB ' + GetLocString(MSG_GUI_FREE);
  end;
end;

procedure Tmainwindow.Startdownload(Aid, Aformatid, Afilename: String);
begin
  //
  if Assigned(ConvertThread) then
  begin
    if not ConvertThread.Terminated then
    begin
      ShowMessage(GetLocString(MSG_ERROR_ALREADYRUN));
      Exit;
    end;
    ConvertThread.WaitFor;
    ConvertThread.Free;
  end;
  SetStatusText(GetLocString(MSG_STATUS_DOWNLOADING) + '...', 50);
  ConvertThread := TStartConvertThread.Create(True);
  ConvertThread.Id := Aid;
  ConvertThread.FormatID := AFormatID;
  ConvertThread.filename := AFilename;
  ConvertThread.OnProgress := @ProgressEvent;
  ConvertThread.OnEnd := @EndCThread;
  ConvertThread.start;
  Timer.Enabled := True;
  ResWin.Close;
  self.Sleep := False;
end;

procedure Tmainwindow.StartDownloadURL(URL, AFilename: string);
begin
  if Assigned(ConvertThread) then
  begin
    if not ConvertThread.Terminated then
    begin
      ShowMessage(GetLocString(MSG_ERROR_ALREADYRUN));
      Exit;
    end;
    ConvertThread.WaitFor;
    ConvertThread.Free;
  end;
  SetStatusText(GetLocString(MSG_STATUS_DOWNLOADING) + '...', 50);
  ConvertThread := TStartConvertThread.Create(True);
  ConvertThread.FormatID := Url;
  ConvertThread.filename := AFilename;
  ConvertThread.OnProgress := @ProgressEvent;
  ConvertThread.OnEnd := @EndCThread;
  ConvertThread.start;
  Timer.Enabled := True;
  ResWin.Close;
  self.Sleep := False;
end;

function GetStrToolType(DObj: PDiskObject; Entry: string; Default: string): string;
var
  Res: PChar;
begin
  Result := Default;
  if not assigned(Dobj) then
    Exit;
  if not Assigned(Dobj^.do_Tooltypes) then
    Exit;
  Res := FindToolType(Dobj^.do_Tooltypes, PChar(Entry));
  if Assigned(Res) then
    Result := Res;
end;

constructor Tmainwindow.Create;
var
  Grp1, Grp2: TMUIGroup;
  Menu: TMUIMenu;
  MI: TMUIMenuItem;
  i: Integer;
  DObj: PDiskObject;
  TextView: TMUIListView;
begin
  inherited Create;
  DTObj := nil;
  OnCloseRequest := @CloseWindow;
  ValLock := TCriticalSection.Create;
  ResultEntries := TResultEntries.Create(True);
  ID := Make_ID('A','M','T','U');
  ConvertThread := nil;
  SearchThread := nil;
  //
  Title := ShortVer;

  Movies := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + MovieTemplateFolder;
  BaseServer := '';

  DObj := GetDiskObject(PChar(ParamStr(0)));
  if Assigned(DObj) then
  begin
    Movies := ExcludeTrailingPathDelimiter(GetStrToolType(DObj, 'MOVIEDIR', Movies));
    if not DirectoryExists(Movies) then
      Movies := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + MovieTemplateFolder;
    BaseServer := GetStrToolType(DObj, 'SERVERURL', '');
    FreeDiskObject(DObj);
  end;

  if (BaseServer <> '') and (Pos('http://', lowercase(BaseServer)) = 1) then
  begin
    if BaseServer[Length(BaseServer)] <> '/' then
      BaseServer := BaseServer + '/';
    MakeURLs(BaseServer);
  end
  else
    BaseServer := '';

  if not DirectoryExists(Movies) then
    CreateDir(Movies);


  MovieLock := Lock(Pchar(Movies), SHARED_LOCK);

  Horizontal := False;

  // Top Group;
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := False;
    Parent := Self;
  end;

  SearchField := TMUIString.Create;
  with SearchField do
  begin
    OnAcknowledge := @SearchEntry;
    Parent := Grp1;
  end;

  Grp2 := TMUIGroup.create;
  With Grp2 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Grp1
  end;
  StatusLabel := TMUIText.Create(GetLocString(MSG_STATUS_IDLE));
  StatusLabel.Parent := Grp2;

  StopButton := TMUIButton.Create;
  with StopButton do
  begin
    FixWidthTxt := GetLocString(MSG_GUI_BREAK);
    Contents := GetLocString(MSG_GUI_BREAK);
    OnClick := @StopAll;
    Disabled := True;
    Parent := Grp2;
  end;

  Progress := TMUIGauge.Create;
  with Progress do
  begin
    Horiz := True;
    Max := 100;
    FixHeight := 20;
    Current := 0;
    Parent := Grp1;
  end;

  // Main Group (with splitter)
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Self;
  end;

  StatText := TMUIText.Create('');
  with StatText do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Self;
  end;

  // Main Lister
  List := TMUIStringGrid.Create;
  with List do
  begin
    ShowLines := True;
    OnClick := @ListClick;
    {$ifdef AROS}
    OnSelectChange := @ListClick;
    {$endif}
    Parent := Grp1;
  end;
  // Splitter
  with TMUIBalance.Create do
  begin
    Parent := Grp1;
  end;
  //

  // Right Group over each other: buttons/Image, Text
  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := False;
    Parent := Grp1;
  end;
  // first top group, left Image, right Buttons
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Grp2;
  end;
  //
  // Image group
  IconGrp := TMUIGroup.Create;
  with IconGrp do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Grp1;
  end;

  Icon := TMUIDrawPanel.Create;
  with Icon do
  begin
    MinWidth := 160;
    MinHeight := 80;
    FillArea := True;
    ShowMe := False;
    OnDrawObject := @DrawIcon;
    Parent := IconGrp;
  end;

  LoadIconBtn := TMUIButton.Create(GetLocString(MSG_GUI_LOAD_ICON));
  with LoadIconBtn do
  begin
    OnClick := @LoadIcon;
    ShowMe := False;
    Parent := IconGrp;
  end;

  TMUIRectangle.Create.Parent := IconGrp;
  //
  // Button Group
  BtnGroup := TMUIGroup.Create;
  with BtnGroup do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := False;
    Parent := Grp1;
  end;


  for i := 0 to High(DownloadBtn) do
  begin
    DownloadBtn[i] := TMUIButton.Create(GetLocString(MSG_GUI_DOWNLOAD_AS) + ' ' + DownName[i]);
    DownloadBtn[i].OnClick := @DownloadClick;
    DownloadBtn[i].Disabled := True;
    DownloadBtn[i].Tag := i;
    DownloadBtn[i].Parent := BtnGroup;
  end;

  DownloadOriginal := TMUIButton.Create(GetLocString(MSG_GUI_GETORIGINAL){'Get Original'});
  DownloadOriginal.OnClick := @GetOriginal;
  DownloadOriginal.Disabled := True;
  DownloadOriginal.Parent := BtnGroup;

  PlayBtn := TMUIButton.Create(GetLocString(MSG_GUI_PLAY));
  PlayBtn.OnClick := @PlayClick;
  PlayBtn.Showme := False;
  PlayBtn.Parent := BtnGroup;

  ShareBtn := TMUIButton.Create(GetLocString(MSG_GUI_SHARE));
  ShareBtn.OnClick := @ShareClick;
  ShareBtn.Showme := False;
  ShareBtn.Parent := BtnGroup;

  DeleteBtn := TMUIButton.Create(GetLocString(MSG_GUI_DELETE));
  DeleteBtn.OnClick := @DeleteClick;
  DeleteBtn.Showme := False;
  DeleteBtn.Parent := BtnGroup;




  TextOut := TMUIFloatText.Create;
  {with TextOut do
  begin
    Parent := Grp2;
  end;}

  TextView := TMUIListView.Create;
  with TextView do
  begin
    List := TextOut;
    Parent := Grp2;
  end;


  // the menu

  MenuStrip := TMUIMenuStrip.Create;

  Menu := TMUIMenu.Create;
  Menu.Parent := MenuStrip;
  Menu.Title := GetLocString(MSG_MENU_PROJECT);// 'Project';

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_MENU_LOCAL_FILES); //'Local Files';
  MI.ShortCut := GetLocString(MSG_MENU_LOCAL_FILES_KEY); //'f';
  MI.OnTrigger := @LoadLocalFiles;
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_MENU_SHARED); //'Remote Shared';
  MI.ShortCut := GetLocString(MSG_MENU_SHARED_KEY); //'r';
  MI.OnTrigger := @LoadSharedList;
  MI.Parent := Menu;
  SharedMenu := MI;

  MI := TMUIMenuItem.Create;
  MI.Title := '-';
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_MENU_MAIN_QUIT); //'Quit';
  MI.ShortCut := GetLocString(MSG_MENU_MAIN_QUIT_KEY); //'q';
  MI.OnTrigger := @QuitEvent;
  MI.Parent := Menu;

  Menu := TMUIMenu.Create;
  Menu.Parent := MenuStrip;
  Menu.Title := GetLocString(MSG_MENU_SETTINGS); //'Settings';

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_MENU_PREFS); //'Prefs ...';
  MI.ShortCut := GetLocString(MSG_MENU_PREFS_KEY); //'p';
  MI.OnTrigger := @PrefsStart;
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := 'MUI ...';
  MI.OnTrigger := @MUISettingsStart;
  MI.Parent := Menu;


  Menu := TMUIMenu.Create;
  Menu.Parent := MenuStrip;
  Menu.Title := GetLocString(MSG_MENU_ABOUT); //'About';

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_MENU_ABOUT_AMITUBE); //'About AmiTube ...';
  MI.OnTrigger := @AboutAmiTube;
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_MENU_CHECK_UPDATES); //'Check for Update ...';
  MI.OnTrigger := @CheckForUpdate;
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_MENU_ABOUT_MUI); //'About MUI ...';
  MI.OnTrigger := @AboutMUI;
  MI.Parent := Menu;

  Timer := TMUITimer.Create;
  With Timer do
  begin
    Interval := 100;
    OnTimer := @TimerEvent;
    Enabled := False;
  end;

  ClipTimer := TMUITimer.Create;
  With ClipTimer do
  begin
    Interval := 500;
    OnTimer := @ClipTimerEvent;
    Enabled := True;
  end;

  OnShow := @FormShow;
end;

destructor Tmainwindow.Destroy;
begin
  UnLock(MovieLock);
  StopAll(nil);
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
  DestroyDTObj;
  ResultEntries.Free;
  ValLock.Free;
  HPsLock.Free;
  HPs.Free;
  inherited Destroy;
end;

procedure MakeVersions;
var
  NumVer: string;
begin
  ShortVer := VERSION;
  Delete(ShortVer, 1, Pos(':', ShortVer) + 1);
  Delete(ShortVer, Pos('(', ShortVer) - 1, Length(ShortVer));

  NumVer := ShortVer;
  Delete(NumVer, 1, Pos(' ', NumVer));
  NumVer := Trim(NumVer);
  // make the number
  MyVersion := MakeVersionNumber(NumVer);

  //writeln('my numVersion := ', PrintVersionNumber(MyVersion));
  {$ifdef CPU68000}
  ShortVer := ShortVer + ' 68000';
  {$endif}

end;

var
  Main: TMainWindow;
begin
  HPsLock := TCriticalSection.Create;
  HPs := TList.Create;
  MakeURLs(BaseURL);
  MakeVersions;
  Main := TMainWindow.Create;
  Prefs := TPrefsWindow.Create;
  ResWin := TResWindow.Create;
  Prefs.OnFormatChanged := @Main.FormatChangeEvent;
  Prefs.OnFormatChanged(nil);
  MUIApp.Title := ShortVer;
  MUIApp.Version := VERSION;
  MUIApp.Author := 'Marcus "ALB42" Sackrow';
  MUIApp.Description := 'YouTube for classic Amiga';
  MUIApp.Base := 'AMITUBE';
  MUIApp.Run;
end.

