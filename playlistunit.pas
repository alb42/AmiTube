unit playlistunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Exec, AmigaDos, MUI, Math, Utility, Intuition, AGraphics,
  MUIClass.Base, MUIClass.Area, MUIClass.Image, MUIClass.Gadget,
  MUIClass.List, MUIClass.Window, MUIClass.Group, MUIClass.Dialog,
  Amitubelocale, PrefsUnit, Datatypes,
  resolutionselunit;

type

  { TPlaylistWin }

  TPlaylistWin = class(TMUIWindow)
  private
    procedure ListDisplay(Sender: TObject; ToPrint: PPChar; Entry: PChar);
    procedure LoadFiles(AMoviePath: string);

    procedure PlayStart(Filename: string);

    procedure PlayClick(Sender: TObject);

    function ShowNextMovie(Res: TResultEntry): Boolean;
  public
    FMovieLock: BPTR;
    PlayEntries: TResultEntries;
    EntryArray: array of string;

    ChooseAnnounce, ChooseRandom, ChooseLoop: TMUICheckmark;
    ChoosePreview: TMUICheckmark;
    WaitEdit: TMUIString;
    List: TMUIListView;
    PlayButton, ShuffleButton: TMUIButton;
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenWindow(AMovieLock: BPTR);
  end;

var
  PlayListwin: TPlaylistWin;

implementation


{ make a list of all the files in movies dir}
procedure LoadLocalFiles(MovieLock: BPTR; ResultEntries: TResultEntries);
var
  FileName, TXTFilename: string;
  SL: TStringList;
  // we use a intermediate list before we copy all the the final result entries
  MyRes: array of record
    Name: string;
    Filename: string;
    id: string;
    Desc: string;
    FileSize: Int64;
    Size: string;
  end;
  Idx,i : Integer;
  //st: string;
  SRes: TResultEntry;
  FI: TFileInfoBlock;
begin
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
        if (SL.Count > 1) and (Pos('ID:', SL[1]) = 1) then
          MyRes[Idx].Id := Trim(Copy(SL[1], 4, Length(SL[1])));
        MyRes[Idx].Desc := SL.Text;
        MyRes[Idx].FileName := Filename;
      end;
    until not Boolean(ExNext(Movielock ,@FI));
  end;
  ResultEntries.Clear;
  // check if we have some results
  if Length(MyRes) > 0 then
  begin
    // get results from list
    for i := 0 to High(MyRes) do
    begin
      SRes := TResultEntry.Create;
      SRes.Name := MyRes[i].Name;
      SRes.Filename := MyRes[i].Filename;
      SRes.Num := ResultEntries.Add(SRes);
      SRes.id := MyRes[i].id;
      SRes.Desc := MyRes[i].Desc;
      SRes.FileSize := MyRes[i].FileSize;
      SRes.Duration := 0;
    end;
  end
  else
    ShowMessage(GetLocString(MSG_ERROR_LOCAL));  // nothing found
  SL.Free;
end;

{ TPlaylistWin }

const
  EmptyString = '';

procedure TPlaylistWin.ListDisplay(Sender: TObject; ToPrint: PPChar; Entry: PChar);
var
  Idx, Idx1: Integer;
  p: PPtrInt;
begin
  P := PPtrInt(ToPrint);
  Dec(P);
  Idx := P^;
  ToPrint[0] := EmptyString;
  if Idx >= 0 then
  begin
    Idx1 := StrToIntDef(Entry, -1);
    if InRange(Idx1, 0, PlayEntries.Count - 1) then
      ToPrint[0] := PChar(PlayEntries[Idx1].Name)
    else
      ToPrint[0] := Entry;
  end;
end;

procedure TPlaylistWin.LoadFiles(AMoviePath: string);
var
  MLock: BPTR;
  UseDefault: Boolean;
  i: Integer;
begin
  UseDefault := AMoviePath = '';
  if UseDefault then
    MLock := FMovieLock
  else
  begin
    // TODO: lock the path
  end;

  LoadLocalFiles(MLock, PlayEntries);

  List.List.Quiet := True;
  List.List.Clear;
  SetLength(EntryArray, PlayEntries.Count);
  for i := 0 to PlayEntries.Count - 1 do
  begin
    EntryArray[i] := IntToStr(i);
    List.List.InsertSingle(PChar(EntryArray[i]), i);
  end;
  List.List.Quiet := False;


  if not UseDefault then
  begin
    // todo unlock the lock
  end;


end;

procedure TPlaylistWin.PlayStart(Filename: string);
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
      [NP_StackSize, Abs(PtrUInt(Me^.tc_SPUpper) - PtrUInt(Me^.tc_SPLower)), // stack size same as myself]
      TAG_END]
    );
    //LastStart := GetTickCount;
    Exit;
  end
  else
  begin
    Param := Prefs.MPEGPlayerParam;
    Param := StringReplace(Param, '%f', '"' + Filename + '"', [rfReplaceAll]);
    MySystem(Prefs.MPEGPlayerPath + ' ' + Param,
      [NP_StackSize, Abs(PtrUInt(Me^.tc_SPUpper) - PtrUInt(Me^.tc_SPLower)), // stack size same as myself]
      TAG_END]
    );
    //LastStart := GetTickCount;
  end;

end;

procedure TPlaylistWin.PlayClick(Sender: TObject);
var
  Count, i, Idx: Integer;
  FileName: string;
  ShowAnnoucement: Boolean;
  First: Boolean;
  LList, TempList: array of Integer;
begin
  First := True;
  ShowAnnoucement := ChooseAnnounce.Selected;
  // play all Files
  if List.List.Entries = 0 then
    Exit;
  //
  if ChooseLoop.Selected then
  begin
    Count := 0;
    repeat
      if ChooseRandom.Selected then
        Idx := StrToIntDef(PChar(List.List.GetEntry(Random(List.List.Entries))), -1)
      else
      begin
        if Count >= List.List.Entries then
          Break;
        Idx := StrToIntDef(PChar(List.List.GetEntry(Count)), -1)
      end;
      if InRange(Idx, 0, PlayEntries.Count - 1) then
      begin
        Filename := PlayEntries[Idx].Filename;
        if not First and ShowAnnoucement then
        begin
          // Show next movie announcement
          if not ShowNextMovie(PlayEntries[Idx]) then
            Exit;
        end;
        First := False;
        PlayStart(Filename);
      end;
      //writeln('idx = ', idx);
      Inc(Count);

    until False;
  end
  else
  begin
    SetLength(TempList, List.List.Entries);
    SetLength(LList, List.List.Entries);
    if ChooseRandom.Selected then
    begin
      for i := 0 to List.List.Entries - 1 do
        TempList[i] := i;
      for i := 0 to High(LList) do
      begin
        Idx := Random(Length(TempList));
        LList[i] := Idx;
        Delete(TempList, Idx, 1);
      end;
    end
    else
    begin
      for i := 0 to List.List.Entries - 1 do
        LList[i] := i;
    end;
    //

    for i := 0 to High(LList) do
    begin
      Idx := StrToIntDef(PChar(List.List.GetEntry(LList[i])), -1);
      if InRange(Idx, 0, PlayEntries.Count - 1) then
      begin
        Filename := PlayEntries[Idx].Filename;
        if not First and ShowAnnoucement then
        begin
          // Show next movie announcement
          if not ShowNextMovie(PlayEntries[Idx]) then
            Exit;
        end;
        First := False;
        PlayStart(Filename);
      end;
      //writeln('idx = ', idx);
    end;
  end;
end;

function TPlaylistWin.ShowNextMovie(Res: TResultEntry): Boolean;
var
  SC: PScreen;
  WD: PWindow;
  tl, y: Integer;
  s: string;
  Msg: PIntuiMessage;
  IClass: LongWord;
  Code: Integer;
  EndTime, T1: LongWord;
  LastTime, sx, sy: Integer;
  DTObj: pObject_;
  FileName: string;
  Size: Classes.TPoint;
  bmhd: PBitMapHeader;
  bm: PBitMap;
  DrawHandle: Pointer;
begin
  Result := True;
  //
  SC := OpenScreenTags(nil, [
    SA_LikeWorkbench, AsTag(True),
    SA_DetailPen, 0,
    SA_BlockPen, 0,
    SA_ShowTitle, AsTag(False),
    TAG_END]);
  WD := OpenWindowTags(nil, [
    WA_Left, 0, WA_TOP, 0,
    WA_Width, SC^.Width, WA_Height, SC^.Height,
    WA_PubScreen, AsTag(SC),
    WA_Borderless, AsTag(True),
    WA_IDCMP, IDCMP_VANILLAKEY,
    WA_Flags, WFLG_ACTIVATE,
    TAG_END]);

  SetRGB4(@SC^.ViewPort, 0, 0, 0, 0);
  SetRGB4(@SC^.ViewPort, 1, $FF, $FF, $FF);
  ObtainPen(SC^.ViewPort.ColorMap, 0, 0, 0, 0, PEN_EXCLUSIVE);
  ObtainPen(SC^.ViewPort.ColorMap, 1, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, PEN_EXCLUSIVE);
  SetRast(WD^.RPort, 0);

  SetAPen(WD^.RPort, 1);
  s := 'Next Video:';
  tl := TextLength(WD^.RPort, PChar(s), Length(s));
  y := SC^.Height div 2;
  GfxMove(WD^.RPort, SC^.width  div 2 - tl div 2, y);
  GfxText(WD^.RPort, PChar(s), Length(s));

  s := Res.Name;
  tl := TextLength(WD^.RPort, PChar(s), Length(s));
  y := y + 11;
  GfxMove(WD^.RPort, SC^.width  div 2 - tl div 2, y);
  GfxText(WD^.RPort, PChar(s), Length(s));

  s := 'starts in ';
  tl := TextLength(WD^.RPort, PChar(s), Length(s));
  y := y + 11;
  GfxMove(WD^.RPort, SC^.width  div 2 - tl div 2, y);
  GfxText(WD^.RPort, PChar(s), Length(s));

  //
  sx := SC^.width  div 2 - tl div 2 + tl;
  sy := y;

  s := 'Press ''Esc'' to stop, ''Space'' to start now.';
  tl := TextLength(WD^.RPort, PChar(s), Length(s));
  y := y + 22;
  GfxMove(WD^.RPort, SC^.width  div 2 - tl div 2, y);
  GfxText(WD^.RPort, PChar(s), Length(s));

  DTObj := nil;
  Filename := ChangeFileExt(Res.FileName, '.jpg');
  if FileExists(Filename) and ChoosePreview.Selected then
  begin
    DTObj := NewDTObject(PChar(FileName), [
      DTA_GroupID, GID_PICTURE,
      PDTA_Remap, AsTag(TRUE),
      PDTA_DestMode,PMODE_V43,
      PDTA_Screen, AsTag(SC),
      OBP_Precision, Precision_Image,
      TAG_END, TAG_END]);
  end;
  if Assigned(DTObj) then
  begin
    Size := Point(0,0);
    DoMethod(DTObj, [DTM_PROCLAYOUT, 0 , 1]);
    // get the Bitmap
    GetDTAttrs(DTObj,
      [
      PDTA_DestBitMap, AsTag(@bm),
      PDTA_BitMapHeader,AsTag(@bmhd),
      TAG_END]);
    if not Assigned(bm) or not Assigned(bmhd) then
    begin
      //SetStatusText(GetLocString(MSG_ERROR_LOAD_ICON) + '(3)');
      DisposeDTObject(DTObj);
      DTObj := nil;
    end;
    if Assigned(bmhd) then
    begin
      // get the size
      Size.x := bmhd^.bmh_Width;
      Size.Y := bmhd^.bmh_Height;
    end;
    // we want to draw it
    DrawHandle := ObtainDTDrawInfoA(DTObj, nil);

    DrawDTObjectA(WD^.RPort, DTObj, SC^.width  div 2 - Size.X div 2, SC^.Height div 2 - (Size.Y + 11), Size.x, Size.Y, 0, 0, nil);

    if Assigned(DrawHandle) then
      ReleaseDTDrawInfo(DTObj, DrawHandle);
    if Assigned(DTObj) then
      DisposeDTObject(DTObj);
  end;


  LastTime := 0;
  EndTime := GetTickCount + Max(1, WaitEdit.IntegerValue) * 1000;
  repeat
    T1 := GetTickCount;
    if T1 >= EndTime then
      Break;

    if LastTime <> (EndTime- t1) div 1000 then
    begin
      LastTime := (EndTime- t1) div 1000;
      s := ' ' + IntToStr(LastTime) + ' s';
      GfxMove(WD^.RPort, sx, sy);
      GfxText(WD^.RPort, PChar(s), Length(s));
    end;

    Msg := PIntuiMessage(GetMsg(Wd^.UserPort));
    if Assigned(Msg) then
    begin
      IClass := Msg^.IClass;
      Code := Msg^.Code;
      ReplyMsg(PMessage(Msg));
      if IClass = IDCMP_VANILLAKEY then
      begin
        if Code = 27 then  // ESC
        begin
          Result := False;
          Break;
        end;
        if Code = $20 then // Space
        begin
          Result := True;
          Break;
        end;
      end;
    end;
    SysUtils.Sleep(10);
  until False;


  ReleasePen(SC^.Viewport.ColorMap, 0);
  ReleasePen(SC^.Viewport.ColorMap, 1);
  CloseWindow(WD);
  CloseScreen(SC);
end;

constructor TPlaylistWin.Create;
var
  Grp: TMUIGroup;
begin
  inherited Create;
  Horizontal := True;
  PlayEntries := TResultEntries.Create(True);

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Frame := MUIV_FRAME_NONE;
    Parent := Self;
  end;

  // Main Lister
  List := TMUIListView.Create;
  List.List := TMUIList.Create;
  with List do
  begin
    HelpNode := 'List';
    Input := True;
    ShowMe := False;
    DragType := MUIV_Listview_DragType_Immediate;
    List.OnDisplay  := @ListDisplay;
    Parent := Grp;
  end;
  List.List.DragSortable := True;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Columns := 2;
    Title := 'Settings';
    Parent := Self;
  end;


  ChooseRandom := TMUICheckmark.Create;
  with ChooseRandom do
  begin
    Parent := Grp;
  end;
  with TMUIText.Create('Random') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  ChooseLoop := TMUICheckmark.Create;
  with ChooseLoop do
  begin
    Parent := Grp;
  end;
  with TMUIText.Create('Loop List') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  ChooseAnnounce := TMUICheckmark.Create;
  with ChooseAnnounce do
  begin
    Parent := Grp;
  end;
  with TMUIText.Create('Show wait screen') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  WaitEdit :=  TMUIString.Create;
  with WaitEdit do
  begin
    IntegerValue := 5;
    Parent := Grp;
  end;
  with TMUIText.Create('Wait time between movies') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  ChoosePreview := TMUICheckmark.Create;
  with ChoosePreview do
  begin
    Parent := Grp;
  end;
  with TMUIText.Create('Show Preview in wait screen') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  TMUIRectangle.Create.Parent := Grp;

  ShuffleButton := TMUIButton.Create('Shuffle');
  ShuffleButton.Parent := Grp;
  //ShuffleButton.OnClick := @PlayClick;

  TMUIRectangle.Create.Parent := Grp;

  TMUIRectangle.Create.Parent := Grp;

  TMUIRectangle.Create.Parent := Grp;

  PlayButton := TMUIButton.Create('Play');
  PlayButton.Parent := Grp;
  PlayButton.OnClick := @PlayClick;



end;

destructor TPlaylistWin.Destroy;
begin
  PlayEntries.Free;
  inherited Destroy;
end;

procedure TPlaylistWin.OpenWindow(AMovieLock: BPTR);
begin
  FMovieLock := AMovieLock;
  Open := True;

  if List.List.Entries = 0 then
  begin
    LoadFiles('');
  end;
end;

end.

