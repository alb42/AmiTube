unit resolutionselunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Sysutils, fgl, Exec, AmigaDos, Utility,
  MUIClass.Dialog,
  MUIClass.StringGrid, MUIClass.Window, MUIClass.Group, MUIClass.Area;

type
  {a result entry}
  TResultEntry = class
    Num: Integer;      // number in the original list (to restore original sorting)
    Name: string;      // Title of the video
    ID: string;        // youtubes id of the video
    Duration: Integer; // duration in s
    Icon: string;      // icon url
    Desc: string;      // description of video
    FileSize: Int64;   // filesize shown in the list (main format)
    Formats: array of record  // formats for direct download
      Title: string;    // title of format
      ACodec: string;   // audio codec
      VCodec: string;   // video codec
      URL: string;      // Url to download
      Ext: string;      // extension for the file
      FormatID: string; // format id to download
    end;
  end;
  { result list}
  TResultEntries = specialize TFPGObjectList<TResultEntry>;

  TOnStartDownload = procedure(ID, FormatID, Filename: string) of object;

  { TResWindow }

  TResWindow = class(TMUIWindow)
  private
    Link: TResultEntry;
    procedure URLToPlayer(Sender: TObject);
    procedure URLToWGet(Sender: TObject);
    procedure GetURLByServer(Sender: TObject);
  public
    List: TMUIStringGrid;
    FOnStartDownLoad: TOnStartDownload;
    constructor Create; override;

    procedure OpenResList(ResEntry: TResultEntry);
  end;
var
  ResWin: TResWindow;
  LastDir: string = 'Ram:'; // for all file requesters

function MySystem(Name: string; const Tags: array of NativeUInt): LongInt; inline;

implementation

uses
  prefsunit, AmiTubeLocale, FileDownloadUnit;

{ TResWindow }

{System taglist overload does not exists for all systems?... ah we just create it}
function MySystem(Name: string; const Tags: array of NativeUInt): LongInt; inline;
begin
  Result := AmigaDos.DosSystem(PChar(Name), @Tags[0]);
end;

{ sent YouTube URL to player like mplayer to stream directly}
procedure TResWindow.URLToPlayer(Sender: Tobject);
var
  OutP: BPTR;
  Param, url: string;
  Me: PTask;
begin
  // must check if the player exists
  if FileExists(Prefs.UrlPlayerPath) then
  begin
    // task
    Me := FindTask(nil);
    // create a CON window to show the output of the player
    OutP := DOSOpen('CON:0/0/640/256/PlayerOutput/WAIT/AUTO/CLOSE', MODE_OLDFILE);
    // put the url to parameter list

    if List.Row > High(Link.Formats) then
    begin
      url := DownloadURL + Link.ID + '&format=' + List.Cells[1, List.Row];
      Param := StringReplace(Prefs.URLPlayerParam, '%u', Url, [rfReplaceAll]);
    end
    else
      Param := StringReplace(Prefs.URLPlayerParam, '%u', Link.Formats[List.Row].Url, [rfReplaceAll]);
    // and action
    // notice... input and output must be connected or some programs omit the output (even when only input is missing)
    MySystem(Prefs.UrlPlayerPath + ' ' + Param, [
      SYS_OUTPUT, AsTag(OutP),
      SYS_INPUT, AsTag(OutP),
      NP_StackSize, Abs(PtrInt(Me^.tc_SPUpper) - PtrInt(Me^.tc_SPLower)), // stack size same as myself
      TAG_DONE]);
    DOSClose(OutP); // close the console window, usually will stay open AUTO is given
  end
  else
    Showmessage(StringReplace(GetLocString(MSG_ERROR_PLAYER), '%s', Prefs.UrlPlayerPath, [rfReplaceAll]));
end;

{ use WGET to download the YouTube URL}
procedure TResWindow.Urltowget(Sender: Tobject);
var
  OutP: BPTR;
  Param: string;
  TargetName, url: string;
  Me: pTask;
begin
  // check if wget is there
  if FileExists(Prefs.WgetPath) then
  begin
    Param := Prefs.WGetParam;
    //
    // if there is not %f makes not sense to ask for it, maybe he just set a fixed name into the parameter?
    if Pos('%f', Param) > 0 then
    begin  // ask for filename
      TargetName := IncludeTrailingPathDelimiter(LastDir) + Link.Id + '.' + List.Cells[1, List.Row];
      with TFileDialog.Create do
      begin
        TitleText := GetLocString(MSG_GUI_SELECTFILE);//'Select name/path for file.';
        Pattern := '#?.' +  List.Cells[1, List.Row];
        Directory := LastDir;
        Filename := Link.Id + '.' + List.Cells[1, List.Row];
        SaveMode := True;
        if not Execute then
        begin
          Free;
          Exit;
        end;
        LastDir := IncludeTrailingPathDelimiter(Directory);
        TargetName := Filename;
        Free;
      end;
      // put the filename to parameter
      Param := StringReplace(Param, '%f', '"' + TargetName + '"', [rfReplaceAll]);
    end;
    // create console to see the progress
    OutP := DOSOpen('CON:0/0/640/256/HttpGetOutput/WAIT/CLOSE', MODE_OLDFILE);
    // url to parameter
    if List.Row > High(Link.Formats) then
    begin
      url := '"' + DownloadURL + Link.ID + '&format=' + List.Cells[1, List.Row] + '"';
      Param := StringReplace(Param, '%u', Url, [rfReplaceAll]);
    end
    else
      Param := StringReplace(Param, '%u', '"' + Link.Formats[List.Row].Url + '"', [rfReplaceAll]);
    //writeln(Param);
    // mysefl for stack
    Me := FindTask(nil);
    // do the action
    MySystem(Prefs.WgetPath + ' ' + Param, [
      SYS_OUTPUT, AsTag(OutP),
      SYS_INPUT, AsTag(OutP),
      //NP_StackSize, Abs(PtrInt(Me^.tc_SPUpper) - PtrInt(Me^.tc_SPLower)),
      TAG_DONE]);
    // close console, but should stay open because AUTO is given
    DOSClose(OutP);
  end
  else
    Showmessage(StringReplace(GetLocString(MSG_ERROR_PLAYER), '%s', Prefs.WgetPath, [rfReplaceAll]));
end;

{ proxy the YouTube download throug hthe download server, no SSL support included, so thats easier
  maybe later alsao include AmiSSL? then could spare the wget option}
procedure TResWindow.GetUrlbyServer(Sender: Tobject);
var
  TargetName: string;
begin
  if Assigned(FOnStartDownLoad) then
  begin
    // ask for filename
    with TFileDialog.Create do
    begin
      TitleText := GetLocString(MSG_GUI_SELECTFILE);//'Select name/path for file.';
      Pattern := '#?.' +  List.Cells[1, List.Row];
      Directory := LastDir;
      Filename := Link.Id + '.' + List.Cells[1, List.Row];
      SaveMode := True;
      if not Execute then
      begin
        Free;
        Exit;
      end;
      LastDir := IncludeTrailingPathDelimiter(Directory);
      TargetName := Filename;
      Free;
    end;
    with TStringList.Create do
    begin
      Text := Link.Desc;
      SaveToFile(ChangeFileExt(TargetName, '.txt'));
      Free;
    end;
    // do it
    if List.Row > High(Link.Formats) then
      FOnStartDownLoad(Link.id, List.Cells[1, List.Row], TargetName)
    else
      FOnStartDownLoad(Link.id, Link.Formats[List.Row].FormatID, TargetName);
  end;
end;

{ create reaolution window with all gui elements }
constructor TResWindow.Create;
var
  Grp: TMUIGroup;
begin
  inherited Create;
  Title := GetLocString(MSG_GUI_GETORIGINAL);
  HelpNode := 'ResWindow';
  //
  // lsit of resolutions
  List := TMUIStringGrid.Create;
  with List do
  begin
    ShowLines := True;
    Parent := Self;
  end;
  // lower group of Buttons
  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Parent := Self;
  end;

  with TMUIButton.Create(GetLocString(MSG_GUI_DOWNLOADHD){'Download to HD'}) do
  begin
    OnClick := @GetURLByServer;
    Parent := Grp;
  end;

  with TMUIButton.Create(GetLocString(MSG_GUI_DOWNLOADTOOL){'Use Download Tool'}) do
  begin
    OnClick := @URLToWGet;
    Parent := Grp;
  end;

  with TMUIButton.Create(GetLocString(MSG_GUI_USEURLPLAYER){'Use URL Player'}) do
  begin
    OnClick := @URLToPlayer;
    Parent := Grp;
  end;

end;


procedure TResWindow.OpenResList(ResEntry: TResultEntry);
var
  i: Integer;
begin
  Self.Close; // if still open close first for a fresh start
  // examine the result entry selected
  Link := ResEntry;
  List.Quiet := True;
  List.NumRows := Length(Link.Formats);
  List.NumColumns := 4;
  List.ShowTitle := true;
  List.Titles[0] := 'ID';
  List.Titles[1] := 'Ext';
  List.Titles[2] := 'Video';
  List.Titles[3] := 'Audio';
  // populate formats list
  for i := 0 to List.NumRows - 1 do
  begin
    List.Cells[0, i] := Link.Formats[i].Title;
    List.Cells[1, i] := Link.Formats[i].Ext;
    if Link.Formats[i].VCodec = 'none' then
      List.Cells[2, i] := '-'
    else
      List.Cells[2, i] := Link.Formats[i].VCodec;
    if Link.Formats[i].ACodec = 'none' then
      List.Cells[3, i] := '-'
    else
      List.Cells[3, i] := Link.Formats[i].ACodec;
  end;
  i := List.NumRows;
  List.NumRows := i + 2;
  List.Cells[0, i] := 'MPEG1 Layer III';
  List.Cells[1, i] := 'mp3';
  List.Cells[2, i] := '-';
  List.Cells[3, i] := 'mp3';
  i := i + 1;
  List.Cells[0, i] := 'Amiga 8SVX';
  List.Cells[1, i] := '8svx';
  List.Cells[2, i] := '-';
  List.Cells[3, i] := 'raw';
  List.Quiet := False;
  // show to User
  Self.Show;
end;

end.

