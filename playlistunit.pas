unit playlistunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Exec, AmigaDos, MUI, Math,
  MUIClass.Base, MUIClass.List, MUIClass.Window, MUIClass.Group, MUIClass.Dialog,
  Amitubelocale, MUIClass.StringGrid,
  resolutionselunit;

type

  { TPlaylistWin }

  TPlaylistWin = class(TMUIWindow)
  private
    procedure ListDisplay(Sender: TObject; ToPrint: PPChar; Entry: PChar);
    procedure LoadFiles(AMoviePath: string);
  public
    FMovieLock: BPTR;
    PlayEntries: TResultEntries;
    EntryArray: array of string;

    List: TMUIListView;
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
      end;
    until not Boolean(ExNext(Movielock ,@FI));
  end;
  //List.NumRows := Length(MyRes);
  ResultEntries.Clear;
  // check if we have some results
  if Length(MyRes) > 0 then
  begin
    //List.Quiet := True;
    {if List.NumColumns <> 3 then
    begin
      List.NumColumns := 3;
      List.Titles[0] := GetLocString(MSG_GUI_LISTNUMBER);
      List.Titles[1] := GetLocString(MSG_GUI_LISTNAME);
      List.Titles[2] := GetLocString(MSG_GUI_LISTSIZE);
    end;}

    // get results from list
    for i := 0 to High(MyRes) do
    begin
      SRes := TResultEntry.Create;
      SRes.Name := MyRes[i].Name;
      SRes.Num := ResultEntries.Add(SRes);
      SRes.id := MyRes[i].id;
      SRes.Desc := MyRes[i].Desc;
      SRes.FileSize := MyRes[i].FileSize;
      SRes.Duration := 0;
      // couldalso be done by updatelist
      //List.Cells[0, i] := IntToStr(i + 1);
      //st := MyRes[i].Name;
      //if Length(st) > Prefs.MaxTitleLen + 3 then
      //  st := Copy(st, 1, Prefs.MaxTitleLen) + '...';
      //List.Cells[1, i] := st;
      //List.Cells[2, i] := MyRes[i].Size;
    end;
    //List.Quiet := False;
    // remove search edit contents
    //SearchField.Contents := '';
  end
  else
    ShowMessage(GetLocString(MSG_ERROR_LOCAL));  // nothing found
  //FancyList.UpdateList;
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

  //List.NumRows := PlayEntries.Count;
  //List.NumColumns :=  1;
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

constructor TPlaylistWin.Create;
begin
  inherited Create;
  PlayEntries := TResultEntries.Create(True);

  // Main Lister
  List := TMUIListView.Create;
  List.List := TMUIList.Create;
  with List do
  begin
    HelpNode := 'List';
    Input := True;
    ShowMe := False;
    //ShowLines := True;
    //ShowTitle := True;
    DragType := MUIV_Listview_DragType_Immediate;
    List.OnDisplay  := @ListDisplay;
    //OnClick := @ListClick;
    //OnDoubleClick := @ListDblClick;
    {$ifdef AROS}
    //OnSelectChange := @ListSelAROS;
    {$endif}
    Parent := Self;
  end;
  List.List.DragSortable := True;
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

