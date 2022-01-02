unit resolutionselunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Sysutils, fgl, Exec, AmigaDos, Utility,
  MUIClass.Dialog,
  MUIClass.StringGrid, MUIClass.Window, MUIClass.Group, MUIClass.Area;

type

  TResultEntry = class
    Name: string;
    ID: string;
    Duration: Integer;
    Icon: string;
    Desc: string;
    Formats: array of record
      Title: string;
      ACodec: string;
      VCodec: string;
      URL: string;
      Ext: string;
      FormatID: string;
    end;
  end;
  TResultEntries = specialize TFPGObjectList<TResultEntry>;

  TOnStartDownload = procedure(ID, FormatID, Filename: String) of object;

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
    destructor Destroy; override;

    procedure OpenResList(ResEntry: TResultEntry);
  end;
var
  ResWin: TResWindow;
  LastDir: string = 'Ram:';

implementation

uses
  prefsunit, AmiTubeLocale;

{ TResWindow }

function MySystem(Name: string; const Tags: array of NativeUInt): LongInt;
begin
  Result := AmigaDos.DosSystem(PChar(Name), @Tags[0]);
end;

procedure Treswindow.Urltoplayer(Sender: Tobject);
var
  OutP: BPTR;
  Param: string;
begin
  if FileExists(Prefs.UrlPlayerPath) then
  begin
    OutP := DOSOpen('CON:0/0/640/256/PlayerOutput/WAIT/CLOSE', MODE_OLDFILE);
    Param := StringReplace(Prefs.URLPlayerParam, '%u', Link.Formats[List.Row].Url, [rfReplaceAll]);
    MySystem(Prefs.UrlPlayerPath + ' ' + Param, [
    SYS_OUTPUT, AsTag(OutP),
    TAG_DONE]);
    DOSClose(OutP);
  end
  else
    Showmessage(StringReplace(GetLocString(MSG_ERROR_PLAYER), '%s', Prefs.UrlPlayerPath, [rfReplaceAll]));
end;

procedure Treswindow.Urltowget(Sender: Tobject);
var
  OutP: BPTR;
  Param: string;
  TargetName: string;
begin
  if FileExists(Prefs.WgetPath) then
  begin
    Param := Prefs.WGetParam;
    //
    if Pos('%f', Param) > 0 then
    begin
      TargetName := IncludeTrailingPathDelimiter(LastDir) + Link.Id + '.' + Link.Formats[List.Row].Ext;
      with TFileDialog.Create do
      begin
        TitleText := GetLocString(MSG_GUI_SELECTFILE);//'Select name/path for file.';
        Pattern := '#?.' +  Link.Formats[List.Row].Ext;
        Directory := LastDir;
        Filename := Link.Id + '.' + Link.Formats[List.Row].Ext;
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
      Param := StringReplace(Param, '%f', '"' + TargetName + '"', [rfReplaceAll]);
    end;
    OutP := DOSOpen('CON:0/0/640/256/HttpGetOutput/WAIT/CLOSE', MODE_OLDFILE);
    Param := StringReplace(Param, '%u', '"' + Link.Formats[List.Row].Url + '"', [rfReplaceAll]);
    {writeln(Prefs.WgetPath + ' ' + Param);
    with TStringList.create do
    begin
      Text := Prefs.WgetPath + ' ' + Param;
      SaveToFile('t:startwget');
      Free;
    end;}
    MySystem(Prefs.WgetPath + ' ' + Param, [
    SYS_OUTPUT, AsTag(OutP),
    TAG_DONE]);
    DOSClose(OutP);
  end
  else
    Showmessage(StringReplace(GetLocString(MSG_ERROR_PLAYER), '%s', Prefs.WgetPath, [rfReplaceAll]));
end;

procedure Treswindow.Geturlbyserver(Sender: Tobject);
var
  TargetName: string;
begin
  if (List.Row >= 0) and (List.Row < Length(Link.Formats)) then
  begin
    if Assigned(FOnStartDownLoad) then
    begin
      //TargetName := IncludeTrailingPathDelimiter(LastDir) + Link.Id + '.' + Link.Formats[List.Row].Ext;
      with TFileDialog.Create do
      begin
        TitleText := GetLocString(MSG_GUI_SELECTFILE);//'Select name/path for file.';
        Pattern := '#?.' +  Link.Formats[List.Row].Ext;
        Directory := LastDir;
        Filename := Link.Id + '.' + Link.Formats[List.Row].Ext;
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
      FOnStartDownLoad(Link.id, Link.Formats[List.Row].FormatID, TargetName);
    end;
  end;
end;

constructor Treswindow.Create;
var
  Grp: TMUIGroup;
begin
  inherited Create;
  Title := 'Choose Resolution';
  //
  {Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Parent := Self;
  end;}

  List := TMUIStringGrid.Create;
  with List do
  begin
    ShowLines := True;
    Parent := Self;
  end;

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

destructor Treswindow.Destroy;
begin
  inherited Destroy;
end;

procedure Treswindow.Openreslist(Resentry: Tresultentry);
var
  i: Integer;
begin
  Self.Close;
  Link := ResEntry;
  List.Quiet := True;
  List.NumRows := Length(Link.Formats);
  List.NumColumns := 4;
  List.ShowTitle := true;
  List.Titles[0] := 'ID';
  List.Titles[1] := 'Ext';
  List.Titles[2] := 'Video';
  List.Titles[3] := 'Audio';
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
  List.Quiet := False;
  Self.Show;
end;

end.

