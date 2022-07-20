unit prefsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, Math, muihelper, mui,
  MUIClass.Window, MUIClass.Group, MUIClass.Gadget,
  MUIClass.Area, MUIClass.Dialog, MUIClass.Image, AmiTubelocale;

type

  { TPrefsWindow }

  TPrefsWindow = class(TMUIWindow)
  private
    FOnFancyListChange: TNotifyEvent;
    Ini: TIniFile;
    NumEdit: TMUIString;
    MaxLenEdit: TMUIString;
    MPEGPlayerEdit, PlayerEdit, URLPlayerEdit, WGetEdit: TMUIString;
    MPEGParamEdit, ParamEdit, URLParamEdit, WGetParamEdit: TMUIString;
    ChooseFormat: TMUICycle;
    ChooseAllFormats: TMUICheckmark;
    ChooseAskDest: TMUICheckmark;
    FOnFormatChanged: TNotifyEvent;
    ChooseAutoStart: TMUICheckmark;
    ChooseAutoIcon: TMUICheckmark;
    ChooseFancyList: TMUICheckmark;
    ChooseBootup: TMUICycle;
    ChooseClip: TMUICheckmark;
    FOnClipChanged: TNotifyEvent;
    Reg: TMUIRegister;
    procedure ChoosePlayerClick(Sender: TObject);
    procedure ChooseMPEGPlayerClick(Sender: TObject);
    procedure ChooseURLPlayerClick(Sender: TObject);
    procedure ChooseWGetClick(Sender: TObject);
    function GetFancyList: Boolean;
    function GetSplitterPos: Integer;
    procedure NumEditACK(Sender: TObject);
    procedure MaxLenACK(Sender: TObject);
    procedure FormatChanged(Sender: TObject);
    procedure SetSplitterPos(AValue: Integer);
    procedure StartupChanged(Sender: TObject);
    procedure AutoChange(Sender: TObject);
    procedure ClipChange(Sender: TObject);
    procedure CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
    procedure AutoIconChange(Sender: TObject);
    procedure FancyListChange(Sender: TObject);
    procedure AllFormatChange(Sender: TObject);
    procedure AskDestChange(Sender: TObject);

    function GetPlayerPath: string;
    function GetMPEGPlayerPath: string;
    function GetURLPlayerPath: string;
    function GetWGetPath: string;

    function GetPlayerParam: string;
    function GetMPEGPlayerParam: string;
    function GetURLPlayerParam: string;
    function GetWGetParam: string;

    function GetNumSearch: Integer;
    function GetFormat: Integer;
    function GetAutoStart: Boolean;
    function GetAutoIcon: Boolean;
    function GetAskDest: Boolean;
    function GetStartup: Integer;
    function GetClip: Boolean;
    function GetAllFormats: Boolean;
    function GetMaxTitleLen: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure UpdateSettings;
    procedure SaveSettings;

    property PlayerPath: string read GetPlayerPath;
    property MPEGPlayerPath: string read GetMPEGPlayerPath;
    property URLPlayerPath: string read GetURLPlayerPath;
    property WGetPath: string read GetWGetPath;

    property PlayerParam: string read GetPlayerParam;
    property MPEGPlayerParam: string read GetMPEGPlayerParam;
    property URLPlayerParam: string read GetURLPlayerParam;
    property WGetParam: string read GetWGetParam;

    property NumSearch: Integer read GetNumSearch;
    property Format: integer read GetFormat;
    property AutoStart: Boolean read GetAutoStart;
    property AutoIcon: Boolean read GetAutoIcon;
    property AskDest: Boolean read GetAskDest;
    property Startup: Integer read GetStartup;
    property ObserveClip: Boolean read GetClip;
    property AllFormats: Boolean read GetAllFormats;
    property MaxTitleLen: Integer read GetMaxTitleLen;
    property FancyList: Boolean read GetFancyList;
    //
    property SplitterPos: Integer read GetSplitterPos write SetSplitterPos;

    //
    property OnFormatChanged: TNotifyEvent read FOnFormatChanged write FOnFormatChanged;
    property OnClipChanged: TNotifyEvent read FOnClipChanged write FOnClipChanged;
    property OnFancyListChange: TNotifyEvent read FOnFancyListChange write FOnFancyListChange;
  end;

var
  Prefs: TPrefsWindow;

implementation

{ TPrefsWindow }

procedure TPrefsWindow.UpdateSettings;
begin
  ChooseFormat.Active := Ini.ReadInteger('General', 'Format', 0);
end;

{ some values are not updated automatically on change, so we change them on exit }
procedure TPrefsWindow.SaveSettings;
begin
  Ini.WriteString('Player', 'Path', PlayerEdit.Contents);
  Ini.WriteString('Player', 'Parameter', PlayerParam);
  Ini.WriteString('Player', 'MPEGPath', MPEGPlayerEdit.Contents);
  Ini.WriteString('Player', 'MPEGParameter', MPEGPlayerParam);
  Ini.WriteString('Player', 'URLPlayerPath', URLPlayerEdit.Contents);
  Ini.WriteString('Player', 'URLParameter', URLPlayerParam);
  Ini.WriteString('Player', 'WGetPath', WGetEdit.Contents);
  Ini.WriteString('Player', 'WGetParameter', WGetParam);
end;

{ Choose player for CDXL }
procedure TPrefsWindow.ChoosePlayerClick(Sender: TObject);
var
  Player: string;
begin
  Player :=  PlayerEdit.Contents;
  with TFileDialog.Create do
  begin
    SaveMode := False;
    Directory := ExtractFilePath(Player);
    Filename := ExtractFilename(Player);
    if Execute then
    begin
      Player := Filename;
      if FileExists(Player) then
      begin
        PlayerEdit.Contents := Player;
        Ini.WriteString('Player', 'Path', PlayerEdit.Contents);
      end
      else
        ShowMessage(StringReplace(GetLocString(MSG_ERROR_PLAYER), '%s', Player, [rfReplaceAll]), GetLocString(MSG_ERROR_ERROR), GetLocString(MSG_GUI_OK));
    end;
    Free;
  end;
end;

procedure TPrefsWindow.ChooseMPEGPlayerClick(Sender: TObject);
var
  Player: string;
begin
  Player :=  MPEGPlayerEdit.Contents;
  with TFileDialog.Create do
  begin
    SaveMode := False;
    Directory := ExtractFilePath(Player);
    Filename := ExtractFilename(Player);
    if Execute then
    begin
      Player := Filename;
      if FileExists(Player) then
      begin
        MPEGPlayerEdit.Contents := Player;
        Ini.WriteString('Player', 'MPEGPath', MPEGPlayerEdit.Contents);
      end
      else
        ShowMessage(StringReplace(GetLocString(MSG_ERROR_PLAYER), '%s', Player, [rfReplaceAll]), GetLocString(MSG_ERROR_ERROR), GetLocString(MSG_GUI_OK));
    end;
    Free;
  end;
end;

procedure TPrefsWindow.ChooseURLPlayerClick(Sender: TObject);
var
  Player: string;
begin
  Player :=  URLPlayerEdit.Contents;
  with TFileDialog.Create do
  begin
    SaveMode := False;
    Directory := ExtractFilePath(Player);
    Filename := ExtractFilename(Player);
    if Execute then
    begin
      Player := Filename;
      if FileExists(Player) then
      begin
        URLPlayerEdit.Contents := Player;
        Ini.WriteString('Player', 'URLPlayerPath', URLPlayerEdit.Contents);
      end
      else
        ShowMessage(StringReplace(GetLocString(MSG_ERROR_PLAYER), '%s', Player, [rfReplaceAll]), GetLocString(MSG_ERROR_ERROR), GetLocString(MSG_GUI_OK));
    end;
    Free;
  end;
end;

procedure TPrefsWindow.ChooseWGetClick(Sender: TObject);
var
  Player: string;
begin
  Player :=  WGetEdit.Contents;
  with TFileDialog.Create do
  begin
    SaveMode := False;
    Directory := ExtractFilePath(Player);
    Filename := ExtractFilename(Player);
    if Execute then
    begin
      Player := Filename;
      if FileExists(Player) then
      begin
        URLPlayerEdit.Contents := Player;
        Ini.WriteString('Player', 'WGetPath', WGetEdit.Contents);
      end
      else
        ShowMessage(StringReplace(GetLocString(MSG_ERROR_PLAYER), '%s', Player, [rfReplaceAll]), GetLocString(MSG_ERROR_ERROR), GetLocString(MSG_GUI_OK));
    end;
    Free;
  end;
end;

function TPrefsWindow.GetFancyList: Boolean;
begin
  Result := ChooseFancyList.Selected;
end;

function TPrefsWindow.GetSplitterPos: Integer;
begin
  Result := Ini.ReadInteger('General', 'Splitter', 100);
end;

function TPrefsWindow.GetPlayerPath: string;
begin
  Result := PlayerEdit.Contents;
end;

function TPrefsWindow.GetMPEGPlayerPath: string;
begin
  Result := MPEGPlayerEdit.Contents;
end;

function TPrefsWindow.GetURLPlayerPath: string;
begin
  Result := URLPlayerEdit.Contents;
end;

function TPrefsWindow.GetWGetPath: string;
begin
  Result := WGetEdit.Contents;
end;

function TPrefsWindow.GetPlayerParam: string;
begin
  Result := ParamEdit.Contents;
  if Pos('%f', Result) = 0 then
    Result := Result + ' %f';
end;

function TPrefsWindow.GetMPEGPlayerParam: string;
begin
  Result := MPEGParamEdit.Contents;
  if Pos('%f', Result) = 0 then
    Result := Result + ' %f';
end;

function TPrefsWindow.GetURLPlayerParam: string;
begin
  Result := URLParamEdit.Contents;
  if Pos('%u', Result) = 0 then
    Result := Result + ' %u';
end;

function TPrefsWindow.GetWGetParam: string;
begin
  Result := WGetParamEdit.Contents;
  if Pos('%u', Result) = 0 then
    Result := Result + ' %u';
end;

function TPrefsWindow.GetNumSearch: Integer;
begin
  Result := EnsureRange(NumEdit.IntegerValue, 1, 100);
end;

function TPrefsWindow.GetMaxTitleLen: Integer;
begin
  Result := EnsureRange(MaxLenEdit.IntegerValue, 1, 999);
end;

function TPrefsWindow.GetFormat: Integer;
begin
  Result := ChooseFormat.Active;
  if Result < 0 then
    Result := 0;
end;

function TPrefsWindow.GetStartup: Integer;
begin
  Result := ChooseBootup.Active;
  if Result < 0 then
    Result := 0;
end;

function TPrefsWindow.GetAutoStart: Boolean;
begin
  Result := ChooseAutoStart.Selected;
end;

function TPrefsWindow.GetAutoIcon: Boolean;
begin
  Result := ChooseAutoIcon.Selected;
end;

function TPrefsWindow.GetAskDest: Boolean;
begin
  Result := ChooseAskDest.Selected;
end;

function TPrefsWindow.GetClip: Boolean;
begin
  Result := ChooseClip.Selected;
end;

function TPrefsWindow.GetAllFormats: Boolean;
begin
  Result := ChooseAllFormats.Selected;
end;

procedure TPrefsWindow.NumEditACK(Sender: TObject);
begin
  //
  NumEdit.IntegerValue := EnsureRange(NumEdit.IntegerValue, 1, 100);
  Ini.WriteInteger('Search', 'MaxNum', NumEdit.IntegerValue);
end;

procedure TPrefsWindow.MaxLenACK(Sender: TObject);
begin
  //
  MaxLenEdit.IntegerValue := EnsureRange(MaxLenEdit.IntegerValue, 1, 999);
  Ini.WriteInteger('Search', 'MaxLen', MaxLenEdit.IntegerValue);
end;

procedure TPrefsWindow.FormatChanged(Sender: TObject);
begin
  Ini.WriteInteger('General', 'Format', ChooseFormat.Active);
  if Assigned(FOnFormatChanged) then
    FOnFormatChanged(Self)
end;

procedure TPrefsWindow.SetSplitterPos(AValue: Integer);
begin
  Ini.WriteInteger('General', 'Splitter', AValue);
end;

procedure TPrefsWindow.StartupChanged(Sender: TObject);
begin
  Ini.WriteInteger('General', 'Startup', ChooseBootup.Active);
end;


procedure TPrefsWindow.AutoChange(Sender: TObject);
begin
  Ini.WriteBool('Player', 'AutoStart', ChooseAutoStart.Selected);
end;

procedure TPrefsWindow.ClipChange(Sender: TObject);
begin
  Ini.WriteBool('Search', 'Clipboard', ChooseClip.Selected);
  if Assigned(FOnClipChanged) then
    FOnClipChanged(Self);
end;

procedure TPrefsWindow.AllFormatChange(Sender: TObject);
begin
  Ini.WriteBool('General', 'AllFormats', ChooseAllFormats.Selected);
end;

procedure TPrefsWindow.AskDestChange(Sender: TObject);
begin
  Ini.WriteBool('General', 'AskDestination', ChooseAskDest.Selected);
end;

procedure TPrefsWindow.AutoIconChange(Sender: TObject);
begin
  Ini.WriteBool('Search', 'AutoIcon', ChooseAutoIcon.Selected);
  if Assigned(FOnFancyListChange) then
    FOnFancyListChange(Self);
end;

procedure TPrefsWindow.FancyListChange(Sender: TObject);
begin
  Ini.WriteBool('Search', 'FancyList', ChooseFancyList.Selected);
  if Assigned(FOnFancyListChange) then
    FOnFancyListChange(Self);
end;

procedure TPrefsWindow.CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;
end;

constructor TPrefsWindow.Create;
var
  Grp1, Grp2: TMUIGroup;
begin
  inherited Create;

  HelpNode := 'PrefsWindow';

  Title := GetLocString(MSG_PREFS_WINDOW);

  OnCloseRequest := @CloseWindow;

  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));

  ID := MAKE_ID('A','T','P', 'r');
  Horizontal := False;

  // ### Startup group
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    FrameTitle := GetLocString(MSG_PREFS_STARTUP);
    Horiz := True;
    Parent := Self;
  end;
  // -- Choose bootup
  ChooseBootup := TMUICycle.Create;
  with ChooseBootup do
  begin
    Entries := [GetLocString(MSG_PREFS_STARTUP1), GetLocString(MSG_PREFS_STARTUP2), GetLocString(MSG_PREFS_STARTUP3)];
    Active := Ini.ReadInteger('General', 'Startup', 0);
    OnActiveChange := @StartupChanged;
    Parent := Grp1;
  end;
  // ### Format group
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    FrameTitle := GetLocString(MSG_PREFS_FORMAT);
    Columns := 2;
    Parent := Self;
  end;

  with TMUIText.Create(''{GetLocString(MSG_PREFS_SEARCHNUM)}) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;
  // -- choose format
  ChooseFormat := TMUICycle.Create;
  with ChooseFormat do
  begin
    Entries := [GetLocString(MSG_PREFS_FORMAT1), GetLocString(MSG_PREFS_FORMAT2), GetLocString(MSG_PREFS_FORMAT3), GetLocString(MSG_PREFS_FORMAT4)];
    Active := Ini.ReadInteger('General', 'Format', 0);
    OnActiveChange := @FormatChanged;
    Parent := Grp1;
  end;
  // -- All formats
  with TMUIText.Create(GetLocString(MSG_PREFS_ALLFORMATS)) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;

  ChooseAllFormats := TMUICheckmark.Create;
  with ChooseAllFormats do
  begin
    Selected := Ini.ReadBool('General', 'AllFormats', False);
    OnSelected := @AllFormatChange;
    Parent := Grp1;
  end;

  // -- Ask for Destination
  with TMUIText.Create(GetLocString(MSG_PREFS_ASKDESTINATION){'Ask for destination path'}) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;

  ChooseAskDest := TMUICheckmark.Create;
  with ChooseAskDest do
  begin
    Selected := Ini.ReadBool('General', 'AskDestination', False);
    OnSelected := @AskDestChange;
    Parent := Grp1;
  end;

  // ### Search group
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    FrameTitle := GetLocString(MSG_PREFS_SEARCH);
    Columns := 2;
    Parent := Self;
  end;
  // -- Number of search results
  with TMUIText.Create(GetLocString(MSG_PREFS_SEARCHNUM)) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;

  NumEdit := TMUIString.Create;
  with NumEdit do
  begin
    Accept := '0123456789';
    MaxLen := 3;
    IntegerValue := Ini.ReadInteger('Search', 'MaxNum', 10);
    OnAcknowledge := @NumEditACK;
    Parent := Grp1;
  end;
  // -- max title length
  with TMUIText.Create(GetLocString(MSG_PREFS_MAXTITLELEN)) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;

  MaxLenEdit := TMUIString.Create;
  with MaxLenEdit do
  begin
    Accept := '0123456789';
    MaxLen := 3;
    IntegerValue := Ini.ReadInteger('Search', 'MaxLen', 30);
    OnAcknowledge := @MaxLenACK;
    Parent := Grp1;
  end;
  // -- observe Clipboard
  with TMUIText.Create(GetLocString(MSG_PREFS_CLIP)) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;

  ChooseClip := TMUICheckmark.Create;
  with ChooseClip do
  begin
    Selected := Ini.ReadBool('Search', 'Clipboard', False);
    OnSelected := @ClipChange;
    Parent := Grp1;
  end;
  // -- Auto load Icon
  with TMUIText.Create(GetLocString(MSG_PREFS_AUTOICON)) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;

  ChooseAutoIcon := TMUICheckmark.Create;
  with ChooseAutoIcon do
  begin
    Selected := Ini.ReadBool('Search', 'AutoIcon', False);
    OnSelected := @AutoIconChange;
    Parent := Grp1;
  end;

  // -- Fancy List
  with TMUIText.Create(GetLocString(MSG_PREFS_FANCYLIST)) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;

  ChooseFancyList := TMUICheckmark.Create;
  with ChooseFancyList do
  begin
    Selected := Ini.ReadBool('Search', 'FancyList', False);
    OnSelected := @FancyListChange;
    Parent := Grp1;
  end;

  // ### Player group
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    FrameTitle := GetLocString(MSG_PREFS_PLAYER);
    Parent := Self;
  end;

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Grp1;
  end;
  // -- Auto Start
  with TMUIText.Create(GetLocString(MSG_PREFS_AUTOSTART)) do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp2;
  end;

  ChooseAutoStart := TMUICheckmark.Create;
  with ChooseAutoStart do
  begin
    Selected := Ini.ReadBool('Player', 'AutoStart', False);
    OnSelected := @AutoChange;
    Parent := Grp2;
  end;


  Reg := TMUIRegister.Create;
  with Reg do
  begin
    Titles := ['CDXL', 'MPEG', 'Play URL', 'Download URL'];
    Parent := Grp1;
  end;

  // ++ CDXL Player
  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Columns := 2;
    Parent := Reg;
  end;

  PlayerEdit := TMUIString.Create;
  with PlayerEdit do
  begin
    Contents := Ini.ReadString('Player', 'Path', 'Sys:Utilities/MultiView');
    Parent := Grp2;
  end;

  with TMUIButton.Create do
  begin
    HorizWeight := 20;
    Contents := GetLocString(MSG_PREFS_CHOOSEPLAYER);
    FixWidthTxt := ' '+GetLocString(MSG_PREFS_CHOOSEPLAYER)+' ';
    OnClick := @ChoosePlayerClick;
    Parent := Grp2;
  end;

  ParamEdit := TMUIString.Create;
  with ParamEdit do
  begin
    Contents := Ini.ReadString('Player', 'Parameter', '%f');
    Parent := Grp2;
  end;

  with TMUIText.Create(GetLocString(MSG_PREFS_PLAYERPARAM)) do
  begin
    FixWidthTxt := ' '+GetLocString(MSG_PREFS_PLAYERPARAM)+' ';
    Frame := MUIV_FRAME_NONE;
    Parent := Grp2;
  end;

  //++ MPEG Player

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Columns := 2;
    Parent := Reg;
  end;

  MPEGPlayerEdit := TMUIString.Create;
  with MPEGPlayerEdit do
  begin
    Contents := Ini.ReadString('Player', 'MPEGPath', 'Sys:Utilities/MultiView');
    Parent := Grp2;
  end;

  with TMUIButton.Create do
  begin
    HorizWeight := 20;
    Contents := GetLocString(MSG_PREFS_CHOOSEPLAYER);
    FixWidthTxt := ' '+GetLocString(MSG_PREFS_CHOOSEPLAYER)+' ';
    OnClick := @ChooseMPEGPlayerClick;
    Parent := Grp2;
  end;

  MPEGParamEdit := TMUIString.Create;
  with MPEGParamEdit do
  begin
    Contents := Ini.ReadString('Player', 'MPEGParameter', '%f');
    Parent := Grp2;
  end;

  with TMUIText.Create(GetLocString(MSG_PREFS_PLAYERPARAM)) do
  begin
    FixWidthTxt := ' ' + GetLocString(MSG_PREFS_PLAYERPARAM) + ' ';
    Frame := MUIV_FRAME_NONE;
    Parent := Grp2;
  end;

  //++ URL Player

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Columns := 2;
    Parent := Reg;
  end;

  URLPlayerEdit := TMUIString.Create;
  with URLPlayerEdit do
  begin
    Contents := Ini.ReadString('Player', 'URLPlayerPath', 'MPlayer:MPlayer');
    Parent := Grp2;
  end;

  with TMUIButton.Create do
  begin
    HorizWeight := 20;
    Contents := GetLocString(MSG_PREFS_CHOOSEPLAYER);
    FixWidthTxt := ' '+GetLocString(MSG_PREFS_CHOOSEPLAYER)+' ';
    OnClick := @ChooseURLPlayerClick;
    Parent := Grp2;
  end;

  URLParamEdit := TMUIString.Create;
  with URLParamEdit do
  begin
    Contents := Ini.ReadString('Player', 'URLParameter', '%u');
    Parent := Grp2;
  end;

  with TMUIText.Create(GetLocString(MSG_PREFS_PLAYERPARAM)) do
  begin
    FixWidthTxt := ' ' + GetLocString(MSG_PREFS_PLAYERPARAM) + ' ';
    Frame := MUIV_FRAME_NONE;
    Parent := Grp2;
  end;

  //++ WGet Player

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Columns := 2;
    Parent := Reg;
  end;

  WGetEdit := TMUIString.Create;
  with WGetEdit do
  begin
    Contents := Ini.ReadString('Player', 'WGetPath', {$ifdef MorphOS}'gg:bin/wget'{$else}'C:wget'{$endif});
    Parent := Grp2;
  end;

  with TMUIButton.Create do
  begin
    HorizWeight := 20;
    Contents := GetLocString(MSG_PREFS_CHOOSEPLAYER);
    FixWidthTxt := ' '+GetLocString(MSG_PREFS_CHOOSEPLAYER)+' ';
    OnClick := @ChooseWGetClick;
    Parent := Grp2;
  end;

  WGetParamEdit := TMUIString.Create;
  with WGetParamEdit do
  begin
    Contents := Ini.ReadString('Player', 'WGetParameter', '%u -O %f');
    Parent := Grp2;
  end;

  with TMUIText.Create(GetLocString(MSG_PREFS_PLAYERPARAM)) do
  begin
    FixWidthTxt := ' ' + GetLocString(MSG_PREFS_PLAYERPARAM) + ' ';
    Frame := MUIV_FRAME_NONE;
    Parent := Grp2;
  end;

end;

destructor TPrefsWindow.Destroy;
begin
  Ini.Free;
  inherited Destroy;
end;

end.

