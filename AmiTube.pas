program AmiTube;
// search and download Youtube videos to CDXL
{$mode objfpc}{$H+}
uses
  AThreads, clipboard, iffparse, AGraphics, Intuition,
  Datatypes, Utility,
  Classes, SysUtils, fphttpclient, mui, muihelper, SyncObjs,
  MUIClass.Base, MUIClass.Window, MUIClass.Group, MUIClass.Area, MUIClass.Gadget,
  MUIClass.Menu, MUIClass.DrawPanel,
  MUIClass.StringGrid, MUIClass.Dialog, MUIClass.List, filedownloadunit, prefsunit,
  XMLRead, DOM, AmiTubelocale;

const
  BaseURL = 'http://amitube.alb42.de/ytsearch2.php?q=';
  BaseURLID = 'http://amitube.alb42.de/ytsearch2.php?id=';
  ConvertURL = 'http://amitube.alb42.de/ytcdxl2.php?id=';
  ShareURL = 'http://amitube.alb42.de/ytshare2.php?id=';
  SharedURL = 'http://amitube.alb42.de/ytshares.xml';
  IconURL = 'http://amitube.alb42.de/yticon.php?id=';

  MovieTemplateFolder = 'movies';
  DefaultTextLimit = 33;

const
  VERSION = '$VER: AmiTube 0.6 beta (15.12.2021)';

type

  TProgressEvent = procedure(Sender: TObject; Percent: Integer; Text: string) of object;

  { TSearchThread }

  TSearchThread = class(TThread)
  protected
    procedure DoOnEnd;
    procedure DoProgress(APercent: integer; AText: string);
    procedure Execute; override;
  public
    Results: array of record
      Name: string;
      ID: string;
      Duration: string;
      Icon: string;
      Desc: string;
    end;
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
    procedure ProgressUpdate(Sender: TObject; Percent: integer; Speed: Integer);
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
    DownloadBtn, PlayBtn, DeleteBtn, ShareBtn, StopButton: TMUIButton;
    SharedMenu: TMUIMenuItem;
    StatusLabel: TMUIText;
    Progress: TMUIGauge;
    Icon: TMUIDrawPanel;
    IconGrp: TMUIGroup;
    LoadIconBtn: TMUIButton;
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
    TextLimit: Integer;
    Movies: string;
    Results: array of record
      ID: string;
      Desc: string;
      Icon: string;
      Duration: string;
    end;
    SearchThread: TSearchThread;
    ConvertThread: TStartConvertThread;
    ImgSize: TPoint;
    procedure SetStatusText(AText: string);
    procedure DestroyDTObj;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  hp: TFPHTTPClient;

{
const
  AFF_68080 = 1 shl 10;

function CheckMe: Boolean; inline;
begin
  Result := (PExecBase(AOS_ExecBase)^.AttnFlags and AFF_68080) <> 0;
end;}

procedure KillSearch;
begin
  if Assigned(hp) then
  begin
    hp.Terminate;
  end;
end;

function GetFile(address: string; AStream: TStream): Boolean;
begin
  Result := False;
  //if not IsOnline then
  //  Exit;
  hp := TFPHTTPClient.Create(nil);
  try
    hp.AllowRedirect := True;
    hp.AddHeader('User-Agent', ShortVer + ' ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%});
    hp.Get(address, AStream);
    Result := True;
  finally
    hp.Free;
    hp := nil;
  end;
end;

procedure TStartConvertThread.ProgressUpdate(Sender: TObject; Percent: integer; Speed: Integer);
var
  t1: Cardinal;
begin
  t1 := GetTickCount;
  if t1-LastTime > 500 then
  begin
    if Speed > 0 then
      DoProgress(Percent, GetLocString(MSG_STATUS_DOWNLOADING) + '...' + FloatToStrF(Speed/1000, ffFixed, 8,2) + ' kbyte/s')
    else
      DoProgress(Percent, GetLocString(MSG_STATUS_DOWNLOADING) + '...');
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
  URL, Ext: string;
  Mem: TMemoryStream;
  SL: TStringList;
  Format: Integer;
begin
  try
    DoProgress(0, GetLocString(MSG_STATUS_CONVERT));
    Format := Prefs.Format;
    Url := ConvertURL + ID + '&format=' + IntToStr(Format);
    Mem := TMemoryStream.Create;
    try
      //if CheckMe then
      //  Exit;
      if GetFile(Url, Mem) then
      begin
        if Terminated then
          Exit;
        SL := TStringList.Create;
        Mem.Position := 0;
        SL.LoadFromStream(Mem);
        //
        LastTime := GetTickCount;
        DoProgress(0, GetLocString(MSG_STATUS_DOWNLOADING) + '...');
        if Pos('http', sl[0]) >= 1 then
        begin
          //writeln('download file');
          if Format = 2 then
            Ext := '.mpeg'
          else
            Ext := '.cdxl';
          DonwloadFile(@ProgressUpdate, sl[0], IncludeTrailingPathDelimiter(Movies) + ID + Ext);
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
        end
        else
          DoProgress(0, GetLocString(MSG_ERROR_CONVERT) +  ' ' + SL.Text);//writeln('no download');
      end;
      Mem.Free;
      SL.Free;
    except
      on e:Exception do
        writeln('Exception ' + E.Message);
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

procedure TSearchThread.DoOnEnd;
begin
  if Assigned(OnEnd) then
    OnEnd(Self);
end;


procedure TSearchThread.DoProgress(APercent: integer; AText: string);
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

procedure TSearchThread.Execute;
var
  Url, SearchTerm, EncStr: string;
  i, Idx: Integer;
  Mem: TMemoryStream;
  Doc: TXMLDocument;
  Child, Node: TDOMNode;
  s: String;
  p: Integer;
  Count: Integer;
  AsID: Boolean;
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
        Url := BaseURLID + EncStr
      else
        Url := BaseURL + EncStr + '&num=' + IntToStr(Prefs.NumSearch);
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
          Idx := Length(Results);
          SetLength(Results, Idx + 1);
          Results[Idx].Name := GetStringAttribute(Child, 'fulltitle');
          Results[Idx].Id := GetStringAttribute(Child, 'id');
          Results[Idx].Icon := GetStringAttribute(Child, 'icon');
          Results[Idx].Duration := GetStringAttribute(Child, 'duration');
          Results[Idx].Desc := Results[Idx].Name + #10#10;
          s := GetStringAttribute(Child, 'uploader');
          if s <> '' then
            Results[Idx].Desc := Results[Idx].Desc + 'Uploader: ' + s + #10;
          s := GetStringAttribute(Child, 'like_count');
          if s <> '' then
            Results[Idx].Desc := Results[Idx].Desc + 'Likes: ' + s + #10;
          s := GetStringAttribute(Child, 'view_count');
          if s <> '' then
            Results[Idx].Desc := Results[Idx].Desc + 'Views: ' + s + #10;
          s := GetStringAttribute(Child, 'license');
          if s <> '' then
            Results[Idx].Desc := Results[Idx].Desc + 'License: ' + s + #10;
          Node := Child.FirstChild;
          if Assigned(Node) then
            Results[Idx].Desc := Results[Idx].Desc + #10 + string(Node.TextContent);
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
  SearchThread := TSearchThread.Create(True);
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
    List.NumRows := Length(SearchThread.Results);
    SetLength(Results, Length(SearchThread.Results));
    for i := 0 to List.NumRows - 1 do
    begin
      Results[i].id := SearchThread.Results[i].id;
      Results[i].Desc := SearchThread.Results[i].Desc;
      Results[i].Duration := SearchThread.Results[i].Duration;
      Results[i].Icon := SearchThread.Results[i].Icon;
      List.Cells[0, i] := IntToStr(i + 1);
      st := SearchThread.Results[i].Name;
      if Length(st) > TextLimit then
        st := Copy(st, 1, TextLimit - 3) + '...';
      List.Cells[1, i] := UTF8ToAnsi(st);
      t := StrToIntDef(SearchThread.Results[i].Duration, 0);
      s := t mod 60;
      List.Cells[2, i] := IntToStr(t div 60) + ':' + Format('%2.2d',[s]);
      case Prefs.Format of
        1: t := t * 300;
        2: t := t * 170;
      else
        t := t * 150;
      end;
      if t > 1000 then
        List.Cells[3, i] := FloatToStrF(t/1024, ffFixed, 8,1) + ' MByte'
      else
        List.Cells[3, i] := IntToStr(t) + ' kByte'
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
begin
  ProgressEvent(Self, 0, GetLocString(MSG_STATUS_IDLE));

  for i := 0 to High(Results) do
  begin
    if ConvertThread.Id = Results[i].Id then
    begin
      List.Row := i;
      Break;
    end;
  end;
  if (List.Row >= 0) and (List.Row <= High(Results)) then
  begin
    PlayFormat := 0;
    if FileExists(IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.mpeg') then
      PlayFormat := 2
    else
      if FileExists(IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.cdxl') then
        PlayFormat := 1;
    DownloadBtn.Disabled := PlayFormat > 0;
    PlayBtn.Disabled := not DownloadBtn.Disabled;
    DeleteBtn.Disabled := not DownloadBtn.Disabled;
    ShareBtn.Disabled := not DownloadBtn.Disabled;
  end
  else
  begin
    DownloadBtn.Disabled := True;
    PlayBtn.Disabled := True;
    DeleteBtn.Disabled := True;
    ShareBtn.Disabled := True;
  end;
  StopButton.Disabled := True;
  TimerEvent(Timer);
  Timer.Enabled := False;
  ClipChanged(nil);
  if Prefs.AutoStart and (not PlayBtn.Disabled) then
    PlayClick(PlayBtn)
end;

procedure Tmainwindow.Listclick(Sender: Tobject);
begin
  Destroydtobj;
  if (List.Row >= 0) and (List.Row <= High(Results)) then
  begin
    TextOut.Text := UTF8ToAnsi(Results[List.Row].Desc);
    PlayFormat := 0;
    if FileExists(IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.mpeg') then
      PlayFormat := 2
    else
      if FileExists(IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.cdxl') then
        PlayFormat := 1;
    DownloadBtn.Disabled := PlayFormat > 0;
    PlayBtn.Disabled := not DownloadBtn.Disabled;
    DeleteBtn.Disabled := not DownloadBtn.Disabled;
    ShareBtn.Disabled := not DownloadBtn.Disabled;
    //
    if Prefs.AutoIcon then
      LoadIcon(Sender);
  end
  else
  begin
    DownloadBtn.Disabled := True;
    PlayBtn.Disabled := True;
    DeleteBtn.Disabled := True;
    ShareBtn.Disabled := True;
  end;
end;

procedure Tmainwindow.Downloadclick(Sender: Tobject);
var
  CT: TStartConvertThread;
begin
  DownloadBtn.Disabled := True;
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
  if (List.Row >= 0) and (List.Row <= High(Results)) then
  begin
    MyID := Results[List.Row].ID;
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
  if (List.Row >= 0) and (List.Row <= High(Results)) then
  begin
    MyID := Results[List.Row].ID;
    MovieName := IncludeTrailingPathDelimiter(Movies) + MyID + '.cdxl';
    MPEGName := IncludeTrailingPathDelimiter(Movies) + MyID + '.mpeg';
    ReadmeName := IncludeTrailingPathDelimiter(Movies) + MyID + '.txt';
    if FileExists(MovieName) then
      DeleteFile(MovieName);
    if FileExists(MPEGName) then
      DeleteFile(MPEGName);
    if FileExists(ReadMeName) then
      DeleteFile(ReadMeName);
    DownloadBtn.Disabled := False;
    PlayBtn.Disabled := True;
    DeleteBtn.Disabled := True;
    ShareBtn.Disabled := True;
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
    SetLength(Results, 0);

    List.Quiet := True;
    List.NumColumns := 3;
    List.NumRows := Length(MyRes);
    SetLength(Results, Length(MyRes));
    for i := 0 to List.NumRows - 1 do
    begin
      Results[i].id := MyRes[i].id;
      Results[i].Desc := MyRes[i].Desc;
      List.Cells[0, i] := IntToStr(i + 1);
      st := MyRes[i].Name;
      if Length(st) > TextLimit then
        st := Copy(st, 1, TextLimit - 3) + '...';
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
  if Prefs.Format = 2 then
    DownloadBtn.Contents := GetLocString(MSG_GUI_DOWNLOAD_MPEG)
  else
    DownloadBtn.Contents := GetLocString(MSG_GUI_DOWNLOAD_CDXL);
  if List.NumColumns > 3 then
  begin
    for i := 0 to High(Results) do
    begin
      t := StrToIntDef(SearchThread.Results[i].Duration, 0);
      case Prefs.Format of
        1: t := t * 300;
        2: t := t * 170;
      else
        t := t * 150;
      end;
      if t > 1000 then
        List.Cells[3, i] := FloatToStrF(t/1024, ffFixed, 8,1) + ' MByte'
      else
        List.Cells[3, i] := IntToStr(t) + ' kByte'
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
    if (List.Row >= 0) and (List.Row <= High(Results)) then
    begin
      MyID := Results[List.Row].ID;
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
  SearchThread := TSearchThread.Create(True);
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
begin
  ShowMessage(MUIX_C + #10 + MUIX_B + '---   ' + ShortVer + '   ---' + MUIX_N+ #10#10 +
              'made with Free Pascal for Amiga by ALB42'#10 +
              'special thanks to Michal Bergseth for idea and encouragement.'#10#10 +
              'Check ' + MUIX_U + 'https://blog.alb42.de' + MUIX_N + ' for updates.'#10);
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
    if Assigned(SearchThread) and (not SearchThread.Terminated) or SearchField.Disabled then
      Exit;
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
  finally
    ClipTimer.Enabled := Prefs.ObserveClip;
  end;
end;

procedure Tmainwindow.Clipchanged(Sender: Tobject);
begin
  ClipTimer.Enabled := Prefs.ObserveClip;
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
  if (List.Row>=0) and (List.Row <= High(Results)) then
  begin
    Setstatustext('Load Icon');
    URL := IconURL + Results[List.Row].ID;

    if FileExists(IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.jpg') then
    begin
      Filename := IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.jpg';
      IconName := '';
    end
    else
    begin
      if FileExists(IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.txt') then
      begin
        Filename := IncludeTrailingPathDelimiter(Movies) + Results[List.Row].ID + '.jpg';
        IconName := '';
      end
      else
      begin
        IconName := 'T:' + Results[List.Row].ID + '.jpg';
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
          SetStatusText('Error get icon: ' + E.Message);
          FS.Free;
          DeleteFile(Filename);
          Exit;
        end;
      end;
      if FS.Size = 0 then
      begin
        SetStatusText('Error get icon file');
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
        //PDTA_Screen, AsTag(scr),
        OBP_Precision, Precision_Image,
        TAG_END, TAG_END]);
    if not Assigned(DTObj) then
    begin
      SetStatusText('Error load icon ' + '1');
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
      SetStatusText('Error load icon ' + '2');
      Exit;
    end;
    ImgSize.x := bmhd^.bmh_Width;
    ImgSize.Y := bmhd^.bmh_Height;
    DrawHandle := ObtainDTDrawInfoA(DTObj, nil);

    IconGrp.ShowMe := True;
    IconGrp.InitChange;
    Icon.MinHeight := ImgSize.Y;
    Icon.MaxHeight := ImgSize.Y;
    Icon.MinWidth := ImgSize.X;
    Icon.MaxWidth := ImgSize.X;
    IconGrp.ExitChange;
    LoadIconBtn.ShowMe := False;
    Setstatustext(GetLocString(MSG_STATUS_IDLE));
  end;
end;

procedure Tmainwindow.Drawicon(Sender: Tobject; Rp: Prastport; Arect: Trect);
begin
  //
  //sysdebugln('draw ' + IntToStr(ARect.Left) + '; ' + IntToStr(AREct.Top) + ' size = ' + IntToStr(ImgSize.X) + '; ' + IntToStr(ImgSize.Y));
  if Assigned(DTObj) then
    DrawDTObjectA(RP, DTObj, ARect.Left, ARect.Top, ImgSize.x, ImgSize.y, 0, 0, nil);
  //
end;

procedure Tmainwindow.Setstatustext(Atext: String);
begin
  StatusLabel.Contents := AText;
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
  IconGrp.ShowMe := False;
  LoadIconBtn.ShowMe := True;
end;

constructor Tmainwindow.Create;
var
  Grp1, Grp2: TMUIGroup;
  Menu: TMUIMenu;
  MI: TMUIMenuItem;
begin
  inherited Create;

  DTObj := nil;

  OnCloseRequest := @CloseWindow;

  ValLock := TCriticalSection.Create;
  //
  Title := ShortVer;
  ID := Make_ID('A','M','T','U');
  TextLimit := DefaultTextLimit;
  ConvertThread := nil;
  SearchThread := nil;

  Movies := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + MovieTemplateFolder;

  if not DirectoryExists(Movies) then
    CreateDir(Movies);

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

  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Self;
  end;

  List := TMUIStringGrid.Create;
  with List do
  begin
    ShowLines := True;
    OnClick := @ListClick;
    Parent := Grp1;
  end;

  with TMUIBalance.Create do
  begin
    Parent := Grp1;
  end;

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := False;
    Parent := Grp1;
  end;

  IconGrp := TMUIGroup.Create;
  with IconGrp do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    ShowMe := False;
    Parent := Grp2;
  end;

  Icon := TMUIDrawPanel.Create;
  with Icon do
  begin
    MinWidth := 160;
    MinHeight := 80;
    FillArea := True;
    OnDrawObject := @DrawIcon;
    Parent := IconGrp;
  end;

  TMUIRectangle.Create.Parent := IconGrp;

  LoadIconBtn := TMUIButton.Create('Load Icon');
  with LoadIconBtn do
  begin
    OnClick := @LoadIcon;
    ShowMe := False;
    Parent := Grp2;
  end;

  TextOut := TMUIFloatText.Create;
  with TextOut do
  begin
    Parent := Grp2;
  end;

  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Frame := MUIV_FRAME_NONE;
    Columns := 2;
    Parent := Grp2;
  end;

  DownloadBtn := TMUIButton.Create(GetLocString(MSG_GUI_DOWNLOAD_CDXL));
  DownloadBtn.OnClick := @DownloadClick;
  DownloadBtn.Disabled := True;
  DownloadBtn.Parent := Grp1;

  PlayBtn := TMUIButton.Create(GetLocString(MSG_GUI_PLAY));
  PlayBtn.OnClick := @PlayClick;
  PlayBtn.Disabled := True;
  PlayBtn.Parent := Grp1;

  DeleteBtn := TMUIButton.Create(GetLocString(MSG_GUI_DELETE));
  DeleteBtn.OnClick := @DeleteClick;
  DeleteBtn.Disabled := True;
  DeleteBtn.Parent := Grp1;

  ShareBtn := TMUIButton.Create(GetLocString(MSG_GUI_SHARE));
  ShareBtn.OnClick := @ShareClick;
  ShareBtn.Disabled := True;
  ShareBtn.Parent := Grp1;

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
    Enabled := False;
  end;

  OnShow := @FormShow;
end;

destructor Tmainwindow.Destroy;
begin
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
  ValLock.Free;
  inherited Destroy;
end;

procedure MakeVersions;
begin
  ShortVer := VERSION;
  Delete(ShortVer, 1, Pos(':', ShortVer) + 1);
  Delete(ShortVer, Pos('(', ShortVer) - 1, Length(ShortVer));
  {$ifdef CPU68000}
  ShortVer := ShortVer + ' 68000';
  {$endif}

end;

var
  Main: TMainWindow;
begin
  MakeVersions;
  Main := TMainWindow.Create;
  Prefs := TPrefsWindow.Create;
  Prefs.OnFormatChanged := @Main.FormatChangeEvent;
  Prefs.OnFormatChanged(nil);
  MUIApp.Title := ShortVer;
  MUIApp.Version := VERSION;
  MUIApp.Author := 'Marcus "ALB42" Sackrow';
  MUIApp.Description := 'YouTube for classic Amiga';
  MUIApp.Base := 'AMITUBE';
  MUIApp.Run;
end.

