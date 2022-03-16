program AmiTube;
// search and download Youtube videos to CDXL

{$mode objfpc}{$H+}
uses
  AThreads, clipboard, iffparse, AGraphics, Intuition, AmigaDos, Exec,
  Datatypes, Utility, workbench, icon, fgl, Math,
  Classes, SysUtils, fphttpclient, mui, muihelper, SyncObjs,
  MUIClass.Base, MUIClass.Window, MUIClass.Group, MUIClass.Area, MUIClass.Gadget,
  MUIClass.Menu, MUIClass.DrawPanel, MUIClass.Image,
  MUIClass.StringGrid, MUIClass.Dialog, MUIClass.List, filedownloadunit, prefsunit,
  XMLRead, DOM, AmiTubelocale, resolutionselunit, historyunit, convertthreadunit, searchthreadunit, downloadlistunit;

const
  // base URL on my server
  BaseURL = 'http://amitube.alb42.de/atv2/';

  // php scripts for different functions will be added to BaseURL
  SearchBase = 'ytsearch.php?q=';         // search by keyword
  SearchBaseID = 'ytsearch.php?id=';      // search by ID
  ConvertBase = 'ytconvert.php?id=';      // convert to CDXL
  ShareBase = 'ytshare.php?id=';          // share an id
  SharedFile = 'ytshares.xml';            // the actual shared list
  IconBase = 'yticon.php?id=';            // preview icon
  DownloadBase = 'ytdownload.php?id=';    // direct download

  UpdateURL = 'http://amitube.alb42.de/amitubeversion'; // check for update always the same, even with 3rd party server

  MovieTemplateFolder = 'movies'; // default Progdir:movies to save the videos

const
  // Version info for Amiga
  VERSION = '$VER: AmiTube 1.0 (16.03.2022)';

  // format settings, atm we have:
  NumFormats = 4;
  // for every setting a name, kbyte/s size estimation, and format in convert call
  DownName: array[0..NumFormats - 1] of string = ('CDXL OCS', 'CDXL AGA', 'CDXL AGA+', 'MPEG1');
  DownSizes: array[0..NumFormats - 1] of Integer = (150, 300, 850, 170);
  DownFormat: array[0..NumFormats - 1] of Integer = (0, 1, 3, 2);

type
  // Version structure for update checking
  TMyVersion = record
    Major: Integer;
    Minor: Integer;
    isBeta: Boolean;
  end;


  { TMainWindow }

  TMainWindow = class(TMUIWindow)
  public
    SearchField: TMUIString;
    ArrowButton: TMUIImage;
    List: TMUIStringGrid;
    TextOut: TMUIFloatText;
    DownloadBtn: array[0..NumFormats - 1] of TMUIButton;
    DownloadOriginal, PlayBtn, DeleteBtn, ShareBtn, StopButton: TMUIButton;
    SharedMenu: TMUIMenuItem;
    StatusLabel: TMUIText;
    Progress: TMUIGauge;
    Icon: TMUIDrawPanel;
    IconGrp: TMUIGroup;
    LoadIconBtn: TMUIButton;
    BtnGroup: TMUIGroup;
    StatText: TMUIText;
    ListClickTimer: TMUITimer;
    procedure SearchEntry(Sender: TObject);
    procedure ClickHistory(Sender: TObject);
    procedure EndThread(Sender: TObject);
    procedure EndCThread(Sender: TObject);
    //
    procedure ListClick(Sender: TObject);
    procedure ListSelAROS(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure StopAll(Sender: TObject);
    //
    procedure DownloadClick(Sender: TObject);
    procedure GetOriginal(Sender: TObject);
    procedure PlayClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure ShareClick(Sender: TObject);

    procedure ProgressEvent(Sender: TObject; Percent: Integer; Text: string);

    procedure LoadLocalFiles(Sender: TObject);
    procedure LoadSharedList(Sender: TObject);
    procedure CheckForUpdate(Sender: TObject);
    procedure PrefsStart(Sender: TObject);
    procedure MenuSortByColumn(Sender: TObject);
    procedure FormatChangeEvent(Sender: TObject);
    procedure ShowDList(Sender: TObject);

    procedure AboutMUI(Sender: TObject);
    procedure AboutAmiTube(Sender: TObject);
    procedure MUISettingsStart(Sender: TObject);

    procedure MainTimerEvent(Sender: TObject);
    procedure CheckStatusForChange;
    procedure ClipChanged(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
    procedure CloseRes(Sender: TObject; var CloseAction: TCloseAction);
    procedure QuitEvent(Sender: TObject);

    procedure LoadIcon(Sender: TObject);
    procedure DrawIcon(Sender: TObject; RP: PRastPort; ARect: TRect);
    //
    function RexxEvent(Sender: TObject; Msg: string; out ReturnMessage: string): LongInt;
  private
    PlayFormat: Integer;
    DTObj: PObject_;
    IconName: string;
    DrawHandle: Pointer;
    ValLock: TCriticalSection;
    NewVal: Boolean;
    FPerc: Integer;
    FTxt: string;
    MainTimer: TMUITimer;
    Movies: string;
    MovieLock: BPTR;
    OldFreeAmount: Int64;
    ResultEntries: TResultEntries;
    SearchThread: TSearchThread;
    ConvertThread: TStartConvertThread;
    OldCThread: TStartConvertThread;
    ImgSize: TPoint;
    BaseServer: string;
    procedure SetStatusText(AText: string; APos: LongInt = -1);
    procedure DestroyDTObj;

    procedure EnableDownloads(Enabled: Boolean; PlayButtons: Boolean);
    procedure UpdateDownloadBtns(Duration: Integer);
    procedure UpdateFreeMem;
    procedure StartDownload(AID, AFormatID, AFilename: string);
    procedure StartDownloadURL(URL, AFilename: string);
    procedure RecreateSortedList;
    procedure SortByColumm(ColClick: Integer);
    procedure TryCThread(Sender: TObject);
    procedure PlayStart(Filename: string);
    function RexxConvert(Url: string; FormatID: Integer; AutoPlay: Boolean; out ErrMsg: string): LongInt;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  MyVersion: TMyVersion;

{Create Urls from base URL (default of given by tooltype) to create all URLs}
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



{ extract Version from string... could be made as record function}
function MakeVersionNumber(s: string): TMyVersion;
begin
  s := Trim(s);
  Result.IsBeta := Pos('beta', s) > 0;
  if Pos(' ', s) > 0 then
    Delete(s, Pos(' ', s), Length(s));
  Result.Major := StrToIntDef(Copy(s, 1, Pos('.', s) - 1), -1);
  Result.Minor := StrToIntDef(Copy(s, Pos('.', s) + 1, Length(s)), -1);
end;

{ format Version number string from version number record... could be made as record function}
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


{ TMainWindow }

{Event for search by the contents of the search edit}
procedure TMainWindow.SearchEntry(Sender: TObject);
begin
  Unused(Sender);
  SearchField.Disabled := True;
  SharedMenu.Enabled := False;
  // reset old result
  List.NumRows := 0;
  // there is an old result... how can that happen?, ait for it to finish first
  if Assigned(SearchThread) then
  begin
    SearchThread.Terminate;
    SearchThread.WaitFor;
    SearchThread.Free;
  end;
  // start new search thread
  SearchThread := TSearchThread.Create;
  SearchThread.GetSharedList := False;
  SearchThread.Search := SearchField.Contents;
  SearchThread.OnProgress := @ProgressEvent;
  SearchThread.OnEnd := @EndThread;
  SearchThread.Start;
  AddToHistory(SearchField.Contents);
end;

procedure TMainWindow.ClickHistory(Sender: TObject);
begin
  Unused(Sender);
  // Calculate the window position
  HistWin.Free;
  HistWin := THistoryWin.Create;
  HistWin.Execute(SearchField);
end;

//##### End Search Thread
procedure TMainWindow.EndThread(Sender: TObject);
var
  i: Integer;
  t: Int64;
  s: Integer;
  st: string;
  SRes: TResultEntry;
begin
  Unused(Sender);
  ProgressEvent(Self, 0, GetLocString(MSG_STATUS_IDLE));
  if SearchThread.IsError then
  begin
    ShowMessage('SearchThread Error: ' + SearchThread.ErrMsg); // something was wrong, no search results
  end
  else
  begin
    // get lists
    List.Quiet := True;
    if List.NumColumns <> 4 then
    begin
      List.NumColumns := 4; // we have a time, -> 4 columns
      List.Titles[0] := GetLocString(MSG_GUI_LISTNUMBER);
      List.Titles[1] := GetLocString(MSG_GUI_LISTNAME);
      List.Titles[2] := GetLocString(MSG_GUI_LISTDURATION);
      List.Titles[3] := GetLocString(MSG_GUI_LISTSIZE);
    end;
    List.NumRows := SearchThread.SearchRes.Count;
    // copy the results from thread
    ResultEntries.Clear;
    for i := 0 to List.NumRows - 1 do
    begin
      SRes := SearchThread.SearchRes[i];
      SRes.Num := ResultEntries.Add(SRes); // results now belong to main Result list and will be destroyed there!
      List.Cells[0, i] := IntToStr(i + 1);
      // Name, cutted if needed
      st := SRes.Name;
      if Length(st) > Prefs.MaxTitleLen + 3 then
        st := Copy(st, 1, Prefs.MaxTitleLen) + '...';
      List.Cells[1, i] := UTF8ToAnsi(st);
      // duration, pretty printed
      t := SRes.Duration;
      s := t mod 60;
      List.Cells[2, i] := IntToStr(t div 60) + ':' + Format('%2.2d',[s]) + ' ';
      // estimated size, pretty printed
      t := t * DownSizes[Prefs.Format];
      SRes.FileSize := t * 1024;
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
end;


{###### end conversation thread}
procedure TMainWindow.EndCThread(Sender: TObject);
var
  Filename: String;
begin
  Unused(Sender);
  ProgressEvent(Self, 0, GetLocString(MSG_STATUS_IDLE));
  // look if in the list, the video it's still there ;) change button status
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    if ResultEntries[List.Row].ID = ConvertThread.DL.ID then
    begin
      PlayFormat := ConvertThread.DL.Format;
      EnableDownloads(False, True);
    end;
  end;
  ClipChanged(nil);
  //
  Filename := '';
  if ConvertThread.DL.FormatID = '' then
    Filename := ConvertThread.DL.Filename;
  // auto start?
  if ConvertThread.DL.AutoPlay and (Filename <> '') then
    PlayStart(Filename);
  // Start Next
  if Assigned(ConvertThread.DL) then
    ConvertThread.DL.Status := dsFinished;
  OldCThread := ConvertThread;
  ConvertThread := nil;
  DownloadListWin.StartNextFree;
  //
end;

{####### event for clicking the list}
procedure TMainWindow.ListClick(Sender: TObject);
begin
  Unused(Sender);
  ListClickTimer.Enabled := False;
  Destroydtobj; // destroy loaded preview image, if any
  // check for new clicked entry
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    TextOut.Text := UTF8ToAnsi(ResultEntries[List.Row].Desc); // show desc
    // check if movie already exists in movies dir
    PlayFormat := 0;
    if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.mpeg') then
      PlayFormat := 2
    else
      if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.cdxl') then
        PlayFormat := 1;
    // enable download or play buttons
    EnableDownloads(PlayFormat = 0, PlayFormat > 0);
    // want to see the icon ;)
    if Prefs.AutoIcon then
      LoadIcon(Sender);
  end
  else
  begin
    EnableDownloads(False, False); // nothing to do
  end;
end;

procedure TMainWindow.ListSelAROS(Sender: TObject);
begin
  Unused(Sender);
  ListClickTimer.Enabled := True;
end;

var
  SortColumn: Integer = 0;
  SortForward: boolean = True;

// compare the items in the search list, dor sorting
function CompareRE(const Item1, Item2: TResultEntry): LongInt;
begin
  case SortColumn of
    0: Result := Item1.Num - Item2.Num;
    1: Result := CompareStr(Item1.Name, Item2.Name);
    2: Result := Item1.Duration - Item2.Duration;
    3: begin
      if Item1.FileSize - Item2.FileSize > MaxInt then
        Result := MaxInt
      else if Item1.FileSize - Item2.FileSize < -MaxInt then
        Result := -MaxInt
      else
        Result := Item1.FileSize - Item2.FileSize;
    end
    else
      Result := Item1.Num - Item2.Num;
  end;
  if not SortForward then
    Result := - Result;
end;

{double click on an entry sorts by that column}
procedure TMainWindow.ListDblClick(Sender: TObject);
begin
  Unused(Sender);
  SortByColumm(List.ClickColumn);
end;

{menu sort entries, use the tag to define which sorting}
procedure TMainWindow.MenuSortByColumn(Sender: TObject);
begin
  if Sender is TMUIMenuItem then
    SortByColumm(TMUIMenuItem(Sender).Tag);
end;

{central function to sort by a column}
procedure TMainWindow.SortByColumm(ColClick: Integer);
var
  i, ClickCol, OldRow: Integer;
begin
  OldRow := -1;
  if List.Row >= 0 then
    OldRow := ResultEntries[List.Row].Num; // remember selected entry, reselect it after sorting
  ClickCol := ColClick;
  // when loaded from HD, duration not exists, here also sort by size
  if (List.NumColumns = 3) and (ClickCol = 2) then
    ClickCol := 3;
  // revert sort order if the same sorting selected
  if SortColumn = ClickCol then
    SortForward := not SortForward
  else
  begin // define column and forward by default
    SortColumn := ClickCol;
    SortForward := True;
  end;
  // we have something defined, lets go sorting
  if SortColumn >= 0 then
    ResultEntries.Sort(@CompareRE);
  // update the visual list
  ReCreateSortedList;
  // reselect the selected entry
  for i := 0 to ResultEntries.Count - 1 do
  begin
    if ResultEntries[i].Num = OldRow then
    begin
      List.Row := i;
      ListClick(nil);
      Break;
    end;
  end;
end;

procedure TMainWindow.TryCThread(Sender: TObject);
var
  DT: TDownloadEntry;
  CT: TStartConvertThread;
begin
  // a
  if Assigned(ConvertThread) and (not ConvertThread.Terminated) then
    Exit;
  if Sender is TDownloadEntry then
  begin
    DT := TDownloadEntry(Sender);
    DT.Status := dsRunning;
    if Assigned(ConvertThread) then
      OldCThread := ConvertThread;
    ConvertThread := nil;
    CT := TStartConvertThread.Create(True);
    CT.OnProgress := @ProgressEvent;
    CT.OnEnd := @EndCThread;
    //
    CT.DL := DT;
    //
    if DT.FormatID = '' then
      SetStatusText(GetLocString(MSG_STATUS_DOWNLOADING) + '...', 0)
    else
      SetStatusText(GetLocString(MSG_STATUS_CONVERT) + '...', 0);
    //
    CT.Start;

    ConvertThread := CT;
  end;
end;

{Try to make a filename from the title, remove strange chars and so on}
function MakeFilename(AName, AltName: string):string;
var
  i: Integer;
  s: string;
  c: Char;
begin
  s := '';
  for i := 1 to Length(AName) do
  begin
    c := AName[i];
    if c in ['a'..'z','A'..'Z','0'..'9', '_', ' '] then
    begin
      s := s + c;
      if Length(s) > 25 then
        Break;
    end;
  end;
  if Length(s) < 3 then
    Result := AltName
  else
    Result := s;
end;

{ Convert and download.
  It starts he convert thread}
procedure TMainWindow.DownloadClick(Sender: TObject);
var
  Format, i: Integer;
  FileSize: Int64;
  Ext: string;
  NewName: string;
  ShowList: Boolean;
begin
  Unused(Sender);
  // if something selected
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    Format := 0;
    if Sender is TMUIButton then
      Format := DownFormat[TMUIButton(Sender).Tag];
    for i := 0 to High(DownloadBtn) do
      DownloadBtn[i].Disabled := True;
    FileSize := Int64(ResultEntries[List.Row].Duration) * DownSizes[Format] * 1024;
    // get extension
    if Format = 2 then
      Ext := '.mpeg'
    else
      Ext := '.cdxl';
    NewName := '';
    // settings want me to ask for the file destintion
    if Prefs.AskDest then
    begin
      with TFileDialog.Create do
      begin
        TitleText := GetLocString(MSG_GUI_SELECTFILE);//'Select name/path for file.';
        Pattern := '#?.' +  Ext;
        Directory := LastDir;
        Filename := MakeFilename(ResultEntries[List.Row].Name, ResultEntries[List.Row].ID) + Ext;
        SaveMode := True;
        if Execute then
        begin
          LastDir := IncludeTrailingPathDelimiter(Directory);
          NewName := Filename;
        end;
        Free;
      end;
    end;
    // nothing selected in Filedialog, then create Filename and path
    if NewName = '' then
      NewName := IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + Ext;
    // check if we have enough memory
    UpdateFreeMem;
    if FileSize > OldFreeAmount then
    begin
      if MessageBox('Error', GetLocString(MSG_ERROR_NO_SPACE), [GetLocString(MSG_GUI_YES), GetLocString(MSG_GUI_NO)]) <> 1 then
      begin
        EnableDownloads(True, False);
        Exit;
      end;
    end;
    // actual starting the thread
    ShowList := Assigned(ConvertThread) and not (ConvertThread.Terminated);
    DownloadListWin.AddToList(ResultEntries[List.Row].Name, ResultEntries[List.Row].ID, '', NewName, ResultEntries[List.Row].Desc, Format, Prefs.AutoStart);
    if ShowList then
      DownloadListWin.Show;
  end;
end;

var
  LastStart: Cardinal = 0;

procedure TMainWindow.PlayStart(Filename: string);
var
  Param, Ext: String;
  Me: PTask;
begin
  if not FileExists(Filename) then
    Exit;
  Ext := LowerCase(ExtractFileExt(Filename));
  Me := FindTask(nil);
  if Ext = '.cdxl' then
  begin
    Param := Prefs.PlayerParam;
    Param := StringReplace(Param, '%f', '"' + Filename + '"', [rfReplaceAll]);
    MySystem(Prefs.PlayerPath + ' ' + Param,
      [NP_StackSize, Abs(PtrInt(Me^.tc_SPUpper) - PtrInt(Me^.tc_SPLower)), // stack size same as myself]
      TAG_END]
    );
    LastStart := GetTickCount;
    Exit;
  end
  else
  begin
    Param := Prefs.MPEGPlayerParam;
    Param := StringReplace(Param, '%f', '"' + Filename + '"', [rfReplaceAll]);
    MySystem(Prefs.MPEGPlayerPath + ' ' + Param,
      [NP_StackSize, Abs(PtrInt(Me^.tc_SPUpper) - PtrInt(Me^.tc_SPLower)), // stack size same as myself]
      TAG_END]
    );
    //ExecuteProcess(Prefs.MPEGPlayerPath, Param, []);
    LastStart := GetTickCount;
  end;

end;

{ play the currently selected movie}
procedure TMainWindow.PlayClick(Sender: TObject);
var
  MyID: string;
  MovieName: string;
  Param: string;
  Me: PTask;
begin
  Unused(Sender);
  // do not start directly after last movie ended
  // dirty hack for agablaster seems to resent the button press
  if GetTickCount - LastStart < 200 then
    Exit;
  Me := FindTask(nil);
  // something selected?
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    MyID := ResultEntries[List.Row].ID;
    // special case for mpeg
    if PlayFormat = 2 then
    begin
      MovieName := IncludeTrailingPathDelimiter(Movies) + MyID + '.mpeg';
      if FileExists(MovieName) then
      begin
        Param := Prefs.MPEGPlayerParam;
        Param := StringReplace(Param, '%f', '"' + MovieName + '"', [rfReplaceAll]);
        MySystem(Prefs.MPEGPlayerPath + ' ' + Param,
          [NP_StackSize, Abs(PtrInt(Me^.tc_SPUpper) - PtrInt(Me^.tc_SPLower)), // stack size same as myself]
          TAG_END]
        );
        //ExecuteProcess(Prefs.MPEGPlayerPath, Param, []);
        LastStart := GetTickCount;
      end;
    end
    else
    begin // all other is CDXL
      MovieName := IncludeTrailingPathDelimiter(Movies) + MyID + '.cdxl';
      if FileExists(MovieName) then
      begin
        Param := Prefs.PlayerParam;
        Param := StringReplace(Param, '%f', '"' + MovieName + '"', [rfReplaceAll]);
        MySystem(Prefs.PlayerPath + ' ' + Param,
          [NP_StackSize, Abs(PtrInt(Me^.tc_SPUpper) - PtrInt(Me^.tc_SPLower)), // stack size same as myself]
          TAG_END]
        );
        LastStart := GetTickCount;
      end;
    end;
  end;
end;

{* Delete the currently selelcted video}
procedure TMainWindow.DeleteClick(Sender: TObject);
var
  MyID: string;
  MovieName, ReadMeName, MPEGName, ImageName: string;
begin
  Unused(Sender);
  // something selected?
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    MyID := ResultEntries[List.Row].ID;
    MovieName := IncludeTrailingPathDelimiter(Movies) + MyID + '.cdxl';
    MPEGName := IncludeTrailingPathDelimiter(Movies) + MyID + '.mpeg';
    ReadmeName := IncludeTrailingPathDelimiter(Movies) + MyID + '.txt';
    ImageName := IncludeTrailingPathDelimiter(Movies) + MyID + '.jpg';
    // ask use if it is ok?
    if MessageBox(GetLocString(MSG_GUI_DELETE), GetLocString(MSG_GUI_DELETE)+ #10' "' + List.Cells[1, List.Row] + '"?' , [GetLocString(MSG_GUI_YES), GetLocString(MSG_GUI_NO)]) = 1 then
    begin
      // try to delete the CDXL
      if FileExists(MovieName) then
        DeleteFile(MovieName);
      // and the mpeg
      if FileExists(MPEGName) then
        DeleteFile(MPEGName);
      // and the movie
      if FileExists(ReadMeName) then
        DeleteFile(ReadMeName);
      // and the Image, if exists
      if FileExists(ImageName) then
        DeleteFile(ImageName);
      // remove the size, if no duration, not valid anymore and no way to get it
      if ResultEntries[List.Row].Duration = 0 then
        List.Cells[2, List.Row] := '-';
      // enable download buttons
      EnableDownloads(True, False);
    end;
  end;
end;

{ Event when a new stauts change arrive.
it basically just copies the Values on transfer variables, which will be read by CheckStatusForChange }
procedure TMainWindow.ProgressEvent(Sender: TObject; Percent: Integer; Text: string);
begin
  Unused(Sender);
  // lock variables first
  ValLock.Enter;
  try
    NewVal := True; // marker that the contents was changed
    FTxt := Text;
    FPerc := Percent;
  finally
    ValLock.Leave;
  end;
end;

{$ALIGN 8}

{ make a list of all the files in movies dir}
procedure TMainWindow.LoadLocalFiles(Sender: TObject);
var
  FileName, TXTFilename: string;
  SL: TStringList;
  // we use a intermediate list before we copy all the the final result entries
  MyRes: array of record
    Name: string;
    id: string;
    Desc: string;
    FileSize: Int64;
    Size: string;
  end;
  Idx,i : Integer;
  st: string;
  SRes: TResultEntry;
  FI: TFileInfoBlock;
begin
  Unused(Sender);
  // make a list of files availabe, we use the TXT and Video file file
  MyRes := [];
  SL := TStringList.Create;
  FI.fib_DirEntryType := 0;

  FillChar(FI, SizeOf(TFileInfoBlock), #0);
  if Boolean(Examine(MovieLock, @FI)) then
  begin
    repeat
      if not ((ExtractFileExt(FI.fib_FileName) = '.mpeg') or (ExtractFileExt(FI.fib_FileName) = '.cdxl')) then
        Continue;
      //
      Filename := IncludeTrailingPathDelimiter(Movies) + FI.fib_FileName;
      TxtFilename := ChangeFileExt(FileName, '.txt');
      if FileExists(TXTFilename) then
      begin
        Idx := Length(MyRes);
        SetLength(MyRes, Idx + 1);
        MyRes[Idx].Id := ExtractFilename(ChangeFileExt(TXTFilename, ''));
        MyRes[Idx].FileSize := FI.fib_Size;
        if (FI.fib_Size / 1024) > 1024 then
          MyRes[Idx].Size := FloatToStrF(FI.fib_Size / 1024 / 1024, ffFixed, 8, 1) + ' MByte'
        else
          MyRes[Idx].Size := IntToStr(Round(FI.fib_Size / 1024)) + ' kByte';
        SL.Clear;
        SL.LoadFromFile(TxtFileName);
        if SL.Count > 0 then
          MyRes[Idx].Name := SL[0];
        MyRes[Idx].Desc := SL.Text;
      end;
    until not Boolean(ExNext(Movielock ,@FI));
  end;
  List.NumRows := Length(MyRes);
  ResultEntries.Clear;
  // check if we have some results
  if Length(MyRes) > 0 then
  begin
    List.Quiet := True;
    if List.NumColumns <> 3 then
    begin
      List.NumColumns := 3;
      List.Titles[0] := GetLocString(MSG_GUI_LISTNUMBER);
      List.Titles[1] := GetLocString(MSG_GUI_LISTNAME);
      List.Titles[2] := GetLocString(MSG_GUI_LISTSIZE);
    end;

    // get results from list
    for i := 0 to List.NumRows - 1 do
    begin
      SRes := TResultEntry.Create;
      SRes.Name := MyRes[i].Name;
      SRes.Num := ResultEntries.Add(SRes);
      SRes.id := MyRes[i].id;
      SRes.Desc := MyRes[i].Desc;
      SRes.FileSize := MyRes[i].FileSize;
      SRes.Duration := 0;
      // couldalso be done by updatelist
      List.Cells[0, i] := IntToStr(i + 1);
      st := MyRes[i].Name;
      if Length(st) > Prefs.MaxTitleLen + 3 then
        st := Copy(st, 1, Prefs.MaxTitleLen) + '...';
      List.Cells[1, i] := UTF8ToAnsi(st);
      List.Cells[2, i] := MyRes[i].Size;
    end;
    List.Quiet := False;
    // remove search edit contents
    SearchField.Contents := '';
  end
  else
    ShowMessage(GetLocString(MSG_ERROR_LOCAL));  // nothing found
  SL.Free;
end;

{ menu close}
procedure TMainWindow.QuitEvent(Sender: TObject);
begin
  Unused(Sender);
  Close;
end;

{ start prefs }
procedure TMainWindow.PrefsStart(Sender: TObject);
begin
  Unused(Sender);
  Prefs.UpdateSettings;
  Prefs.Open := True;
end;

{ TRy to stop all current tasks}
procedure TMainWindow.StopAll(Sender: TObject);
begin
  Unused(Sender);
  if Assigned(SearchThread) then
    SearchThread.Terminate;
  if Assigned(ConvertThread) then
    ConvertThread.Terminate;
  KillSearch;
  KillDownload;
end;

{ check if the status has a new value and put that to GUI
this must be called by GUI task, timer for example}
procedure TMainWindow.CheckStatusForChange;
begin
  // lock it
  ValLock.Enter;
  try
    // something to do
    if NewVal then
    begin
      // it is idle... but convert thread is still running... change back to convert (if downloading, it will changed by )
      if (FTxt = GetLocString(MSG_STATUS_IDLE)) and Assigned(ConvertThread) and not (ConvertThread.Terminated)  then
        StatusLabel.Contents := GetLocString(MSG_STATUS_CONVERT)
      else
        StatusLabel.Contents := FTxt;
      Progress.Current := FPerc;
      NewVal := False;
    end;
  finally
    ValLock.Leave;
  end;
end;

{ Format is changed in Prefs, recalc the size of the movies}
procedure TMainWindow.FormatChangeEvent(Sender: TObject);
var
  t,i : Integer;
begin
  Unused(Sender);
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

procedure TMainWindow.ShowDList(Sender: TObject);
begin
  Unused(Sender);
  DownloadListWin.Show;
end;

{ Share movie with other people}
procedure TMainWindow.ShareClick(Sender: TObject);
var
  s, Url, MyID, EncStr: string;
  hp: TFPHTTPClient;
  i: Integer;
begin
  Unused(Sender);
  //
  ShareBtn.Disabled := True;
  hp := nil;
  try
    if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
    begin
      // sanatize id
      MyID := ResultEntries[List.Row].ID;
      EncStr := '';
      for i := 1 to Length(MyID) do
        EncStr := EncStr + '%' + IntToHex(Ord(MyID[i]),2);
      Url := ShareURL + EncStr;
      try
        // we could use getfile...
        hp := TFPHTTPClient.Create(nil);
        hp.AllowRedirect := True;
        hp.AddHeader('User-Agent', ShortVer + ' ' +  {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%});
        s := hp.Get(URL);
        ShowMessage(GetLocString(MSG_STATUS_SHARED) + #10 + s); // server will tell if already in the list
      except
        On E:Exception do
          ShowMessage(GetLocString(MSG_ERROR_SHARE)+ ' ' + E.Message); // error
      end;
    end;
  finally
    hp.Free;
    ShareBtn.Disabled := False; // do not share again
  end;
end;

{ load the shared list from the web}
procedure TMainWindow.LoadSharedList(Sender: TObject);
begin
  Unused(Sender);
  SearchField.Disabled := True;
  SharedMenu.Enabled := False;
  List.NumRows := 0;
  if Assigned(SearchThread) then
  begin
    SearchThread.Terminate;
    SearchThread.WaitFor;
    SearchThread.Free;
  end;
  // we use the search thread to load the shared list
  SearchThread := TSearchThread.Create;
  SearchThread.GetSharedList := True;
  SearchThread.OnProgress := @ProgressEvent;
  SearchThread.OnEnd := @EndThread;
  SearchThread.Start;
end;

{ menu about MUI }
procedure TMainWindow.AboutMUI(Sender: TObject);
begin
  Unused(Sender);
  MUIApp.AboutMUI;
end;

{ menu about AmiTube }
procedure TMainWindow.AboutAmiTube(Sender: TObject);
var
  s: string;
begin
  Unused(Sender);
  s := (MUIX_C + #10 + MUIX_B + '---   ' + ShortVer + '   ---' + MUIX_N+ #10 +
        {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%} + #10#10 +
       'made with Free Pascal for Amiga by ALB42'#10 +
       'special thanks to Michal Bergseth for idea and encouragement.'#10#10 +
       'Make Amiga programs, not war'#10#10 +
       'Check ' + MUIX_U + 'https://blog.alb42.de' + MUIX_N + ' for updates.'#10);
  if BaseServer <> '' then
    s := s + 'Used Server: ' + BaseServer + #10;
  ShowMessage(s);
end;

{ menu mui settings }
procedure TMainWindow.MUISettingsStart(Sender: TObject);
begin
  Unused(Sender);
  MUIApp.OpenConfigWindow;
end;

{ form show }
procedure TMainWindow.FormShow(Sender: TObject);
begin
  Unused(Sender);
  // what we load on startup
  case Prefs.Startup of
    1: LoadLocalFiles(nil);
    2: LoadSharedList(nil);
  end;
  // clip check is on?
  Prefs.OnClipChanged := @ClipChanged;
  ClipChanged(nil);
end;

// special function copied from AROS clipboard

const
  ID_FTXT = 1179932756;
  ID_CHRS = 1128813139;

// get text from Clipboard
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
  LastClip: string; // last contents of clip... to notice that the clipboard changed

{ originally only for clipboard and only running when clipboard observation is on
  now it's always running and also check freemem and status.}
procedure TMainWindow.MainTimerEvent(Sender: TObject);
var
  s: string;
begin
  Unused(Sender);
  MainTimer.Enabled := False;
  try
    // some checks
    UpdateFreeMem;
    if Assigned(OldCThread) then
      FreeAndNil(OldCThread);
    CheckStatusForChange;
    // if search not checking for clipboard
    if Assigned(SearchThread) and (not SearchThread.Terminated) or SearchField.Disabled then
      Exit;
    // the user want that?
    if Prefs.ObserveClip then
    begin
      // get clip contents
      s := GetTextFromClip(PRIMARY_CLIP);
      // has changed since last time
      if s = LastClip then
        Exit;
      LastClip := s;
      // trim it
      s := Trim(s);
      // check if it's an URL of YouTube
      if ((Pos('https://', lowercase(s)) = 1) and (Pos('youtube', lowercase(s)) > 0)) or (Pos('https://youtu.be/', lowercase(s)) = 1) then
      begin
        // ask if we want to do it
        if MessageBox(GetLocString(MSG_GUI_GOTURL), StringReplace(GetLocString(MSG_GUI_GOTURLTEXT), '%s', s, [rfReplaceAll]), [GetLocString(MSG_GUI_YES), GetLocString(MSG_GUI_NO)]) = 1 then
        begin
          SearchField.Contents := s; // just put to search bar and press "enter"
          SearchEntry(SearchField);
        end;
      end;
    end;
  finally
    MainTimer.Enabled := True; // always enable again
  end;
end;

{ clip setting changed }
procedure TMainWindow.ClipChanged(Sender: TObject);
begin
  Unused(Sender);
  MainTimer.Enabled := True;
end;

{ event when close main window -> save settings }
procedure TMainWindow.CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
begin
  Unused(Sender);
  Unused(CloseAction);
  Prefs.SaveSettings;
end;

{ Load the icon }
procedure TMainWindow.LoadIcon(Sender: TObject);
var
  URL: string;
  FS: TFileStream;
  bmhd: PBitMapHeader;
  bm: PBitMap;
  Filename: string;
begin
  Unused(Sender);
  // destroy old icon if any
  Destroydtobj;
  // something selected
  if (List.Row>=0) and (List.Row < ResultEntries.Count) then
  begin
    SetStatusText('Load Icon');
    // form url
    URL := IconURL + ResultEntries[List.Row].ID;
    // check if there is already a image file
    if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.jpg') then
    begin
      Filename := IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.jpg';
      IconName := '';
    end
    else
    begin
      // not existing, but video is saved on the HD -> save the jpeg along with it
      if FileExists(IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.txt') then
      begin
        Filename := IncludeTrailingPathDelimiter(Movies) + ResultEntries[List.Row].ID + '.jpg';
        IconName := '';
      end
      else
      begin
        // just use temporary folder for jpg
        IconName := 'T:' + ResultEntries[List.Row].ID + '.jpg';
        FileName := IconName;
      end;
      // if already existing, remove it (could happen for T:)
      if FileExists(Filename) then
        DeleteFile(Filename);
      //
      FS := TFileStream.Create(Filename, fmCreate);
      try
        GetFile(URL, FS); // get the actual file
      except
        on E:Exception do
        begin
          SetStatusText(GetLocString(MSG_ERROR_LOAD_ICON) + '(' + E.Message + ')');
          FS.Free;
          DeleteFile(Filename);
          Exit;
        end;
      end;
      // hmm nothing saved...
      if FS.Size = 0 then
      begin
        SetStatusText(GetLocString(MSG_ERROR_LOAD_ICON) + '(1)');
        FS.Free;
        DeleteFile(Filename); // remove that file
      end
      else
        FS.Free;
    end;
    // try to open the file  with Datatype
    DTObj := NewDTObject(PChar(FileName), [
        DTA_GroupID, GID_PICTURE,
        PDTA_Remap, AsTag(TRUE),
        PDTA_DestMode,PMODE_V43,
        PDTA_Screen, AsTag(Self.Screen),
        OBP_Precision, Precision_Image,
        TAG_END, TAG_END]);
    // check if the Datatype was created
    if not Assigned(DTObj) then
    begin
      SetStatusText(GetLocString(MSG_ERROR_LOAD_ICON) + '(2)');
      if IconName <> '' then
        DeleteFile(IconName);
      Exit;
    end;
    // process the image
    DoMethod(DTObj, [DTM_PROCLAYOUT, 0 , 1]);
    // get the Bitmap
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
    // get the size
    ImgSize.x := bmhd^.bmh_Width;
    ImgSize.Y := bmhd^.bmh_Height;
    // we want to draw it
    DrawHandle := ObtainDTDrawInfoA(DTObj, nil);
    // show the icon image
    Icon.ShowMe := True;
    LoadIconBtn.ShowMe := False;
    // resize it
    IconGrp.InitChange;
    Icon.MinHeight := ImgSize.Y;
    Icon.MaxHeight := ImgSize.Y;
    Icon.MinWidth := ImgSize.X;
    Icon.MaxWidth := ImgSize.X;
    IconGrp.ExitChange;
    // done
    SetStatusText(GetLocString(MSG_STATUS_IDLE));
  end;
end;

{ the actual drawing of the icon, event for Icon}
procedure TMainWindow.DrawIcon(Sender: TObject; RP: PRastPort; ARect: TRect);
var
  s: string;
begin
  Unused(Sender);
  // just let the Datatype draw it
  if Assigned(DTObj) then
    DrawDTObjectA(RP, DTObj, ARect.Left, ARect.Top, ImgSize.x, ImgSize.y, 0, 0, nil)
  else
  begin
    // just clear it
    SetRast(RP, 0);
    GFXMove(RP, ARect.Left + 2, ARect.Top + Icon.Height div 2);
    s := GetLocString(MSG_GUI_LOAD_ICON);
    SetDrmd(RP, JAM1);
    SetAPen(RP, 1);
    GfxText(RP, PChar(S), Length(s));
  end;
  //
end;

function TMainWindow.RexxConvert(Url: string; FormatID: Integer; AutoPlay: Boolean; out ErrMsg: string): LongInt;
var
  AId, Filename, EncStr, AName, ADesc, s: string;
  p: SizeInt;
  i: Integer;
  Mem: TMemoryStream;
  Doc: TXMLDocument;
  XMLChild: TDOMNode;
begin
  ErrMsg := '';
  Result := 20;
  //
  AID := '';
  if (Pos('https://', LowerCase(Trim(URL))) = 1) then
  begin
    if Pos('https://youtu.be/', LowerCase(URL)) = 1 then
    begin
      Delete(URL, 1, 17);
      AId := Url;
    end
    else
    begin
      p := Pos('v=', URL); // search for "v=" then search for https://
      if p > 0 then
      begin
        AID := Copy(URL, P + 2, Length(URL));
        p := Pos('&', AID);
        if p > 1 then
          Delete(AID, p, Length(AID));
      end;
    end;
  end;
  if AID = '' then
  begin
    ErrMsg := 'Parameter is not a valid YouTube URL';
    Exit;
  end;
  // check if already on the HD
  //
  if FormatID = 2 then
    FileName := IncludeTrailingPathDelimiter(Movies) + AID + '.mpeg'
  else
    FileName := IncludeTrailingPathDelimiter(Movies) + AID + '.cdxl';
  if FileExists(Filename) then
  begin
    // todo: Play
    if AutoPlay then
      PlayStart(FileName);
    ErrMsg := 'File already exists.';
    Result := 0;
    Exit;
  end;
  // download the infos for it
  // form url, encoding GET parameter
  EncStr := '';
  for i := 1 to Length(AID) do
    EncStr := EncStr + '%' + IntToHex(Ord(AID[i]),2);
  // put it URL together
  Url := SearchURLID + EncStr;
  // get the ID contents
  Mem := TMemoryStream.Create;
  try
    // actual GET
    if GetFile(Url, Mem) then
    begin
      try
        Mem.Position := 0;
        // process the XML
        ReadXMLFile(Doc, Mem);
      except
        on E: Exception do
        begin  // how that happens
          ErrMsg := 'Exception in ReadXMLFile ' + E.Message;
          Mem.Position := 0;
          // how, why
          With TStringList.Create do
          begin
            LoadFromStream(Mem);
            SaveToFile('PROGDIR:ErrorLog.log');
            //Writeln(Text);
            Exit;
            Free;
          end;
        end;
      end;
      XMLChild := Doc.DocumentElement.FirstChild;
      i := 0;
      if Assigned(XMLChild) then
      begin
        AName := GetStringAttribute(XMLChild, 'fulltitle');
        AId := GetStringAttribute(XMLChild, 'id');
        ADesc := AName + #10#10;
        s := GetStringAttribute(XMLChild, 'uploader');
        if s <> '' then
          ADesc := ADesc + 'Uploader: ' + s + #10;
        s := GetStringAttribute(XMLChild, 'like_count');
        if s <> '' then
          ADesc := ADesc + 'Likes: ' + s + #10;
        s := GetStringAttribute(XMLChild, 'view_count');
        if s <> '' then
          ADesc := ADesc + 'Views: ' + s + #10;
        s := GetStringAttribute(XMLChild, 'license');
        if s <> '' then
          ADesc := ADesc + 'License: ' + s + #10;
        // add to download queue
        DownloadListWin.AddToList(AName, AId, '', Filename, ADesc, FormatID, AutoPlay);
        Result := 0;
      end
      else
      begin
        ErrMsg := 'Movie not found';
      end;
    end
    else
    begin
      ErrMsg := 'Movie ID not found';
    end;
  finally
    Mem.Free;
  end;
end;

function TMainWindow.RexxEvent(Sender: TObject; Msg: string; out ReturnMessage: string): LongInt;
var
  SL: TStringList;
  Cmd: string;
  NName: string;
  i: LongInt;
  FormatN: Integer;
begin
  Unused(Sender);
  Result := 20;
  ReturnMessage := 'unknown message';
  SL := TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(Msg), SL);
    if SL.Count = 0 then
      Exit;
    Cmd := SL[0];
    SL.Delete(0);
    case UpperCase(Cmd) of
      'SEARCH': begin
        if SL.Count = 0 then
        begin
          ReturnMessage := 'search argument missing: SEARCH <search words>';
          Exit;
        end;
        NName := SL[0];
        for i := 1 to SL.Count - 1 do
          NName := NName + ' ' + SL[i];
        if Assigned(SearchThread) and (not SearchThread.Terminated) or SearchField.Disabled then
        begin
          ReturnMessage := 'Search running already';
          Exit;
        end;
        // the user want that?
        SearchField.Contents := NName; // just put to search bar and press "enter"
        SearchEntry(SearchField);
        ReturnMessage := '';
        Result := 0;
      end;
      'CONVERT', 'PLAY': begin
        if SL.Count = 0 then
        begin
          ReturnMessage := 'argument missing: ' + cmd + ' <Youtube URL> <Format number>';
          Exit;
        end;
        NName := SL[0];
        FormatN := Prefs.Format;
        if SL.Count > 1 then
        begin
          FormatN := StrToIntDef(SL[1], FormatN);
          if not InRange(FormatN, 0, 3) then
            FormatN := Prefs.Format;
        end;
        Result := RexxConvert(NName, FormatN, cmd = 'PLAY', ReturnMessage);
      end;
    end;
  finally
    SL.Free;
  end;
end;

{ download directly from YouTube, open the resolution window for that}
procedure TMainWindow.GetOriginal(Sender: TObject);
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
  Unused(Sender);
  if (List.Row >= 0) and (List.Row < ResultEntries.Count) then
  begin
    Self.Sleep := True; // disable main window
    //
    SRes := ResultEntries[List.Row];
    // no formats until now, so go and get them
    if Length(SRes.Formats) = 0 then
    begin
      // form url, encoding GET parameter
      EncStr := '';
      for i := 1 to Length(SRes.ID) do
        EncStr := EncStr + '%' + IntToHex(Ord(SRes.ID[i]),2);
      // put it URL together
      Url := SearchURLID + EncStr;
      // get the ID contents
      Mem := TMemoryStream.Create;
      try
        // actual GET
        if GetFile(Url, Mem) then
        begin
          try
            Mem.Position := 0;
            // process the XML
            ReadXMLFile(Doc, Mem);
          except
            on E: Exception do
            begin  // how that happens
              writeln('Exception in ReadXMLFile ', E.Message);
              Mem.Position := 0;
              // how, why
              With TStringList.Create do
              begin
                LoadFromStream(Mem);
                SaveToFile('PROGDIR:ErrorLog.log');
                //Writeln(Text);
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
    // Resolution Window with some events
    ResWin.FOnStartDownLoad := @Startdownload;
    ResWin.OnCloseRequest := @CloseRes;
    ResWin.Openreslist(SRes);
  end;
end;


{ event for resolution window, enable main window again }
procedure TMainWindow.CloseRes(Sender: TObject; var CloseAction: TCloseAction);
begin
  Unused(Sender);
  Unused(CloseAction);
  //
  Self.Sleep := False;
end;

{ check a Version against my local Version }
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

{ Check for AmiTube Updates }
procedure TMainWindow.CheckForUpdate(Sender: TObject);
var
  Mem: TMemoryStream;
  OnlineVersion, Link, s, TargetName: string;
begin
  Unused(Sender);
  Mem := TMemoryStream.Create;
  try
    try
      OnlineVersion := '';
      Link := '';
      // GET the update url
      if GetFile(UpdateURL, Mem) then
      begin
        Mem.Position := 0;
        // check the update contents
        with TStringList.Create do
        begin
          LoadFromStream(Mem);
          if Count > 0 then
            OnlineVersion := Strings[0];
          // Amiga is the first URL, AROS is the 2nd URL
          {$ifdef Amiga68k}
          if Count > 1 then
            Link := Strings[1];
          {$endif}
          {$ifdef AROS}
          if Count > 2 then
            Link := Strings[2];
          {$endif}
          {$ifdef MorphOS}
          if Count > 3 then
            Link := Strings[3];
          {$endif}
          {$ifdef AmigaOS4}
          if Count > 4 then
            Link := Strings[4];
          {$endif}
          Free;
        end;
        //
        //Check for Version
        if IsNewerVersion(MakeVersionNumber(OnlineVersion)) and (Link <> '') then
        begin
          // ask user if we want to download the Update
          TargetName := '';
          s := StringReplace(GetLocString(MSG_GUI_UPDATEAVAIL), '%o', PrintVersionNumber(MyVersion), [rfReplaceAll]);
          s := StringReplace(s, '%n', OnlineVersion, [rfReplaceAll]);
          s := StringReplace(s, '\n', #10, [rfReplaceAll]);
          if MessageBox('Update', s, [GetLocString(MSG_GUI_YES), GetLocString(MSG_GUI_NO)]) = 1 then
          begin
            // ask for target to save
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
            // to the download
            if TargetName <> '' then
              StartDownloadURL(Link, TargetName);
          end;
        end
        else
        begin
          ShowMessage(GetLocString(MSG_GUI_NOUPDATE)); // nothing to update
        end;
      end;
      if (Link = '') or (OnlineVersion = '') then
        ShowMessage(GetLocString(MSG_ERROR_UPDATE));  // error update
    except
      on E: Exception do
        ShowMessage(GetLocString(MSG_ERROR_UPDATE) + ' ' + E.Message); // error update
    end;
  finally
    Mem.Free;
  end;
end;

{ Set the status text and progessbar directly, not from thread }
procedure TMainWindow.SetStatusText(AText: string; APos: LongInt);
begin
  if (AText = GetLocString(MSG_STATUS_IDLE)) and Assigned(ConvertThread) and not ConvertThread.Terminated then
    AText := GetLocString(MSG_STATUS_CONVERT);
  StatusLabel.Contents := AText;
  // do not change if APos < 0
  if APos >= 0 then
    Progress.Current := APos;
end;

{ Destroy of Datatype loaded Preview image }
procedure TMainWindow.DestroyDTObj;
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

{ Short cut to show/hide Convert and play buttons all together}
procedure TMainWindow.EnableDownloads(Enabled: Boolean; PlayButtons: Boolean);
var
  i: Integer;
begin
  BtnGroup.InitChange;   // make sure MUI notice the change
  // show all convert buttons or
  if Prefs.AllFormats then
  begin
    for i := low(DownloadBtn) to High(DownloadBtn) do
    begin
      DownloadBtn[i].Disabled := False;
      DownloadBtn[i].ShowMe := Enabled;
    end;
  end
  else
  begin // show only the selcted one
    for i := low(DownloadBtn) to High(DownloadBtn) do
    begin
      DownloadBtn[i].Disabled := False;
      DownloadBtn[i].ShowMe := Enabled and (Prefs.Format = DownFormat[i]);
    end;
  end;
  // playbuttons
  PlayBtn.ShowMe := PlayButtons;
  DeleteBtn.ShowMe := PlayButtons;
  ShareBtn.ShowMe := PlayButtons;
  // update the size on the Convert buttons
  if Enabled then
  begin
    if (List.Row >= 0) and (List.Row < ResultEntries.Count) and Prefs.AllFormats then
      Updatedownloadbtns(ResultEntries[List.Row].Duration)
    else
      Updatedownloadbtns(0);
  end;
  // original available
  DownloadOriginal.Disabled := not(Enabled or PlayButtons);
  // done
  BtnGroup.ExitChange;
end;

{ Convert button update text (est. size and bold if main format) }
procedure TMainWindow.UpdateDownloadBtns(Duration: Integer);
var
  t, i: Integer;
  s: string;
begin
  for i := 0 to High(DownSizes) do
  begin
    t := DownSizes[i] * Duration;
    s := '';
    if (Prefs.Format = DownFormat[i]) and Prefs.AllFormats then
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

{ Update the Free size text in main window}
procedure TMainWindow.UpdateFreeMem;
var
  InfoData: TInfoData;
  FreeAmount: Int64;
  f: Single;
begin
  //
  // get the movie folder informations
  Info(MovieLock, @InfoData);
  // calculate Free hd space
  FreeAmount := (Int64(InfoData.id_NumBlocks) - Int64(InfoData.id_NumBlocksUsed)) * InfoData.id_BytesPerBlock;

  if FreeAmount <> OldFreeAmount then
  begin
    OldFreeAmount := FreeAmount;
    // byte prefix scaling
    // special case if full
    if FreeAmount = 0 then
    begin
      StatText.Contents := MUIX_R +  GetLocString(MSG_GUI_DISK_FULL);
      Exit;
    end;
    // byte
    if FreeAmount < 1024 then
    begin
      StatText.Contents := MUIX_R + IntToStr(FreeAmount) + ' byte ' + GetLocString(MSG_GUI_FREE);
      Exit;
    end;
    // kByte
    f := FreeAmount / 1024;
    if f < 1024 then
    begin
      StatText.Contents := MUIX_R + FloatToStrF(F, ffFixed, 8,1) + ' kB ' + GetLocString(MSG_GUI_FREE);
      Exit;
    end;
    // MByte
    f := f / 1024;
    if f < 1024 then
    begin
      StatText.Contents := MUIX_R + FloatToStrF(F, ffFixed, 8,1) + ' MB ' + GetLocString(MSG_GUI_FREE);
      Exit;
    end;
    // more, ok just show GByte
    f := f / 1024;
    StatText.Contents := MUIX_R + FloatToStrF(F, ffFixed, 8,1) + ' GB ' + GetLocString(MSG_GUI_FREE);
  end;
end;

{ Start downloading triggered by resolution window }
procedure TMainWindow.StartDownload(AID, AFormatID, AFilename: string);
begin
  DownloadListWin.AddToList(ExtractFileName(AFilename), AId, AFormatID, AFileName, '', -1, False);
  ResWin.Close;
  StopButton.Disabled := False;
  Self.Sleep := False; // make sure main window is active again
end;

{ Download the URL to File using the Convert thread, used for UpdateDownload }
procedure TMainWindow.StartDownloadURL(URL, AFilename: string);
begin
  //
  DownloadListWin.AddToList(ExtractFileName(AFilename), '', Url, AFileName, '', -1, False);
  //
  ResWin.Close;
  self.Sleep := False;
end;

{ After the List is sorted the listview must be updated}
procedure TMainWindow.RecreateSortedList;
var
  IsLocalList: Boolean;
  i, s: Integer;
  SRes: TResultEntry;
  SizeStr, st: string;
begin
  IsLocalList := List.NumColumns = 3; // local list has one column less
  // make sure it does not redraw for all update
  List.Quiet := True;
  // update all
  for i := 0 to ResultEntries.Count - 1 do
  begin
    SRes := ResultEntries[i]; // get the result
    // the number
    List.Cells[0, i] := IntToStr(SRes.Num + 1);
    // the cutted title
    st := SRes.Name;
    if Length(st) > Prefs.MaxTitleLen + 3 then
      st := Copy(st, 1, Prefs.MaxTitleLen) + '...';
    List.Cells[1, i] := UTF8ToAnsi(st);
    // the filesize
    if (SRes.FileSize / 1024) > 1024 then
      SizeStr := FloatToStrF(SRes.FileSize / 1024 / 1024, ffFixed, 8, 1) + ' MByte'
    else
      SizeStr := IntToStr(Round(SRes.FileSize / 1024)) + ' kByte';
    if IsLocalList then
    begin
      List.Cells[2, i] := SizeStr;
    end
    else
    begin
      // and the duration if available
      s := SRes.Duration mod 60;
      List.Cells[2, i] := IntToStr(SRes.Duration div 60) + ':' + Format('%2.2d',[s]) + ' ';
      List.Cells[3, i] := SizeStr;
    end;
  end;
  List.Quiet := False;
end;

{ load a string tooltype from icon, if not found, return the "Default" string}
function GetStrToolType(DObj: PDiskObject; Entry: string; Default: string): string;
var
  Res: PChar;
begin
  Result := Default;
  // easier here to check if icon is found
  if not assigned(Dobj) then
    Exit;
  // and if there are tooltypes at all
  if not Assigned(Dobj^.do_Tooltypes) then
    Exit;
  // the actual search
  Res := FindToolType(Dobj^.do_Tooltypes, PChar(Entry));
  // check if found
  if Assigned(Res) then
    Result := Res;
end;

{ main window create
  Creates all the GUI object and initialize some variables s}
constructor TMainWindow.Create;
var
  Grp1, Grp2: TMUIGroup;
  Menu: TMUIMenu;
  MI: TMUIMenuItem;
  i: Integer;
  DObj: PDiskObject;
  TextView: TMUIListView;
  SB: TMUIScrollGroup;
begin
  inherited Create;
  //
  MUIApp.OnRexxMsg := @RexxEvent;
  //
  HelpNode := 'MainWindow';
  //
  // Events
  OnCloseRequest := @CloseWindow;
  OnShow := @FormShow;
  //
  ValLock := TCriticalSection.Create;
  ResultEntries := TResultEntries.Create(True);
  ID := Make_ID('A','M','T','U');
  //
  ConvertThread := nil;
  OldCThread := nil;
  SearchThread := nil;
  DTObj := nil;
  //
  // titla of window, just use the version/name
  Title := ShortVer;
  // default movies dir just in the same folder as the executable
  Movies := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + MovieTemplateFolder;
  // by default no convert server
  BaseServer := '';
  // get the icon
  DObj := GetDiskObject(PChar(ParamStr(0)));
  if Assigned(DObj) then
  begin
    // get movies dir
    Movies := ExcludeTrailingPathDelimiter(GetStrToolType(DObj, 'MOVIEDIR', Movies));
    // bugfix, check if it exists, if not just use the default
    if not DirectoryExists(Movies) then
      Movies := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + MovieTemplateFolder;
    // if there is a BsaseServer given use that, no check is done
    // later Version number of Server should be checked?
    BaseServer := GetStrToolType(DObj, 'SERVERURL', '');
    // done
    FreeDiskObject(DObj);
  end;
  // check if the BaseServer is set
  if (BaseServer <> '') and (Pos('http://', LowerCase(BaseServer)) = 1) then
  begin
    // make sure it ends with /
    if BaseServer[Length(BaseServer)] <> '/' then
      BaseServer := BaseServer + '/';
    // create all URLs
    MakeURLs(BaseServer);
  end
  else
    BaseServer := ''; // nothing to do just use the default URLs
  //
  // if movies not exist, create it
  if not DirectoryExists(Movies) then
    CreateDir(Movies);
  //
  MovieLock := Lock(PChar(Movies), SHARED_LOCK); // get a lock on the Movie dir for checking Free space

  // ############################
  // now GUI stuff follow
  //
  Horizontal := False;

  // Top Group;
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    HelpNode := 'SearchBar';
    Frame := MUIV_FRAME_NONE;
    Horiz := False;
    Parent := Self;
  end;

  Grp2 := TMUIGroup.create;
  With Grp2 do
  begin
    HelpNode := 'StatusBar';
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Grp1
  end;


  // Search field
  SearchField := TMUIString.Create;
  with SearchField do
  begin
    OnAcknowledge := @SearchEntry;
    Parent := Grp2;
  end;

  ArrowButton := TMUIImage.Create;
  with ArrowButton do
  begin
    InputMode := MUIV_InputMode_RelVerify;
    Frame := MUIV_FRAME_BUTTON;
    OnClick := @ClickHistory;
    Spec.Spec := MUII_PopUp;//MUII_ArrowDown;
    Parent := Grp2;
  end;

  // Status bar, progress and label
  Grp2 := TMUIGroup.create;
  With Grp2 do
  begin
    HelpNode := 'StatusBar';
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Grp1
  end;
  StatusLabel := TMUIText.Create(GetLocString(MSG_STATUS_IDLE));
  StatusLabel.Parent := Grp2;
  // Break/Stop button
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
    HelpNode := 'StatusBar';
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
    HelpNode := 'FreeSpace';
    Frame := MUIV_FRAME_NONE;
    Parent := Self;
  end;

  // Main Lister
  List := TMUIStringGrid.Create;
  with List do
  begin
    HelpNode := 'List';
    Input := True;
    ShowLines := True;
    ShowTitle := True;
    OnClick := @ListClick;
    OnDoubleClick := @ListDblClick;
    {$ifdef AROS}
    OnSelectChange := @ListSelAROS;
    {$endif}
    Parent := Grp1;
  end;
  List.NumColumns := 4; // we have a time, -> 4 columns
  List.Titles[0] := GetLocString(MSG_GUI_LISTNUMBER);
  List.Titles[1] := GetLocString(MSG_GUI_LISTNAME);
  List.Titles[2] := GetLocString(MSG_GUI_LISTDURATION);
  List.Titles[3] := GetLocString(MSG_GUI_LISTSIZE);

  // Splitter
  with TMUIBalance.Create do
  begin
    Parent := Grp1;
  end;
  //

  SB := TMUIScrollGroup.Create;
  with SB do
  begin
    Frame := MUIV_FRAME_NONE;
    FreeVert := False;
    Parent := Grp1;
  end;

  // Right Group over each other: buttons/Image, Text
  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := False;
    Parent := SB.Contents; // Grp1;
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
    HelpNode := 'PreviewIcon';
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

  // Convert Buttons
  for i := 0 to High(DownloadBtn) do
  begin
    DownloadBtn[i] := TMUIButton.Create(GetLocString(MSG_GUI_DOWNLOAD_AS) + ' ' + DownName[i]);
    DownloadBtn[i].OnClick := @DownloadClick;
    DownloadBtn[i].HelpNode := 'Convert';
    DownloadBtn[i].Disabled := True;
    DownloadBtn[i].Tag := i;
    DownloadBtn[i].Parent := BtnGroup;
  end;

  // direct download
  DownloadOriginal := TMUIButton.Create(GetLocString(MSG_GUI_GETORIGINAL){'Get Original'});
  DownloadOriginal.HelpNode := 'Convert';
  DownloadOriginal.OnClick := @GetOriginal;
  DownloadOriginal.Disabled := True;
  DownloadOriginal.Parent := BtnGroup;

  // Play Button
  PlayBtn := TMUIButton.Create(GetLocString(MSG_GUI_PLAY));
  with PlayBtn do
  begin
    HelpNode := 'VideoButtons';
    OnClick := @PlayClick;
    Showme := False;
    Parent := BtnGroup;
  end;

  ShareBtn := TMUIButton.Create(GetLocString(MSG_GUI_SHARE));
  with ShareBtn do
  begin
    HelpNode := 'VideoButtons';
    OnClick := @ShareClick;
    Showme := False;
    Parent := BtnGroup;
  end;

  DeleteBtn := TMUIButton.Create(GetLocString(MSG_GUI_DELETE));
  with DeleteBtn do
  begin
    HelpNode := 'VideoButtons';
    OnClick := @DeleteClick;
    Showme := False;
    Parent := BtnGroup;
  end;
  //
  // Text display
  TextOut := TMUIFloatText.Create;
  // scroller for text
  TextView := TMUIListView.Create;
  with TextView do
  begin
    HelpNode := 'VideoText';
    List := TextOut;
    Parent := Grp2;
  end;

  //############ the Menu
  MenuStrip := TMUIMenuStrip.Create;

  // #### Project
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
  MI.Title := GetLocString(MSG_MENU_DOWNLOADLIST);  // Download list
  MI.OnTrigger := @ShowDList;
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := '-';
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_MENU_MAIN_QUIT); //'Quit';
  MI.ShortCut := GetLocString(MSG_MENU_MAIN_QUIT_KEY); //'q';
  MI.OnTrigger := @QuitEvent;
  MI.Parent := Menu;

  // #### Sort By

  Menu := TMUIMenu.Create;
  Menu.Parent := MenuStrip;
  Menu.Title := GetLocString(MSG_MENU_SORTBY); //'Sort By';

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_GUI_LISTNUMBER); //'Nr.';
  MI.Tag := 0;
  MI.OnTrigger := @MenuSortByColumn;
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_GUI_LISTNAME); //'Name';
  MI.Tag := 1;
  MI.OnTrigger := @MenuSortByColumn;
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_GUI_LISTDURATION); //'Duration';
  MI.Tag := 2;
  MI.OnTrigger := @MenuSortByColumn;
  MI.Parent := Menu;

  MI := TMUIMenuItem.Create;
  MI.Title := GetLocString(MSG_GUI_LISTSIZE); //'Filesize';
  MI.Tag := 3;
  MI.OnTrigger := @MenuSortByColumn;
  MI.Parent := Menu;

  //#### Settings
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

  //### About
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

  // main Timer
  MainTimer := TMUITimer.Create;
  With MainTimer do
  begin
    Interval := 500;
    OnTimer := @MainTimerEvent;
    Enabled := True;
  end;

  ListClickTimer := TMUITimer.Create;
  with ListClickTimer do
  begin
    Interval := 100;
    OnTimer := @ListClick;
    Enabled := False;
  end;
end;

destructor TMainWindow.Destroy;
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
  FreeAndNil(OldCThread);
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
  MakeURLs(BaseURL);
  MakeVersions;
  Main := TMainWindow.Create;
  Prefs := TPrefsWindow.Create;
  ResWin := TResWindow.Create;
  DownloadListWin := TDownloadListWin.Create;
  Prefs.OnFormatChanged := @Main.FormatChangeEvent;
  Prefs.OnFormatChanged(nil);
  DownloadListWin.OnDownloadStart := @Main.TryCThread;
  MUIApp.Title := ShortVer;
  MUIApp.Version := VERSION;
  MUIApp.Author := 'Marcus "ALB42" Sackrow';
  MUIApp.Copyright := '(c) 2022 Marcus "ALB42" Sackrow';
  MUIApp.Description := 'YouTube for classic Amiga';
  MUIApp.HelpFile := 'PROGDIR:AmiTube.guide';
  MUIApp.Base := 'AMITUBE';
  MUIApp.Run;
end.

