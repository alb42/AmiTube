unit prefsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, Math, muihelper, mui,
  MUIClass.Window, MUIClass.Group, MUIClass.Gadget,
  MUIClass.Area, MUIClass.Dialog, MUIClass.Image;

type

  { TPrefsWindow }

  TPrefsWindow = class(TMUIWindow)
  private
    Ini: TIniFile;
    NumEdit: TMUIString;
    MPEGPlayerEdit, PlayerEdit: TMUIString;
    MPEGParamEdit, ParamEdit: TMUIString;
    ChooseFormat: TMUICycle;
    FOnFormatChanged: TNotifyEvent;
    ChooseAutoStart: TMUICheckmark;
    ChooseBootup: TMUICycle;
    ChooseClip: TMUICheckmark;
    FOnClipChanged: TNotifyEvent;
    procedure ChoosePlayerClick(Sender: TObject);
    procedure ChooseMPEGPlayerClick(Sender: TObject);
    procedure NumEditACK(Sender: TObject);
    procedure FormatChanged(Sender: TObject);
    procedure StartupChanged(Sender: TObject);
    procedure AutoChange(Sender: TObject);
    procedure ClipChange(Sender: TObject);
    procedure CloseWindow(Sender: TObject; var CloseAction: TCloseAction);

    function GetPlayerPath: string;
    function GetMPEGPlayerPath: string;
    function GetNumSearch: Integer;
    function GetFormat: Integer;
    function GetAutoStart: Boolean;
    function GetStartup: Integer;
    function GetClip: Boolean;
    function GetPlayerParam: string;
    function GetMPEGPlayerParam: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure UpdateSettings;
    procedure SaveSettings;

    property PlayerPath: string read GetPlayerPath;
    property MPEGPlayerPath: string read GetMPEGPlayerPath;

    property PlayerParam: string read GetPlayerParam;
    property MPEGPlayerParam: string read GetMPEGPlayerParam;

    property NumSearch: Integer read GetNumSearch;
    property Format: integer read GetFormat;
    property AutoStart: Boolean read GetAutoStart;
    property Startup: Integer read GetStartup;
    property ObserveClip: Boolean read GetClip;
    //
    property OnFormatChanged: TNotifyEvent read FOnFormatChanged write FOnFormatChanged;
    property OnClipChanged: TNotifyEvent read FOnClipChanged write FOnClipChanged;
  end;

var
  Prefs: TPrefsWindow;

implementation

{ TPrefsWindow }

procedure TPrefsWindow.UpdateSettings;
begin
  ChooseFormat.Active := Ini.ReadInteger('General', 'Format', 0);
end;

procedure TPrefsWindow.SaveSettings;
begin
  Ini.WriteString('Player', 'Path', PlayerEdit.Contents);
  Ini.WriteString('Player', 'Parameter', PlayerParam);
  Ini.WriteString('Player', 'MPEGPath', MPEGPlayerEdit.Contents);
  Ini.WriteString('Player', 'MPEGParameter', MPEGPlayerParam);
end;

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
        ShowMessage('Error: "' + Player + '" not found.');
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
        ShowMessage('Error: "' + Player + '" not found.');
    end;
    Free;
  end;
end;

function TPrefsWindow.GetPlayerPath: string;
begin
  Result := PlayerEdit.Contents;
end;

function TPrefsWindow.GetMPEGPlayerPath: string;
begin
  Result := MPEGPlayerEdit.Contents;
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

function TPrefsWindow.GetNumSearch: Integer;
begin
  Result := EnsureRange(NumEdit.IntegerValue, 1, 100);
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

function TPrefsWindow.GetClip: Boolean;
begin
  Result := ChooseClip.Selected;
end;

procedure TPrefsWindow.NumEditACK(Sender: TObject);
begin
  //
  NumEdit.IntegerValue := EnsureRange(NumEdit.IntegerValue, 1, 100);
  Ini.WriteInteger('Search', 'MaxNum', NumEdit.IntegerValue);
end;

procedure TPrefsWindow.FormatChanged(Sender: TObject);
begin
  Ini.WriteInteger('General', 'Format', ChooseFormat.Active);
  if Assigned(FOnFormatChanged) then
    FOnFormatChanged(Self)
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

procedure TPrefsWindow.CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;
end;

constructor TPrefsWindow.Create;
var
  Grp1, Grp2: TMUIGroup;
begin
  inherited Create;

  OnCloseRequest := @CloseWindow;

  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));

  ID := MAKE_ID('A','T','P', 'r');
  Horizontal := False;

  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    FrameTitle := 'Start up list contents';
    Horiz := True;
    Parent := Self;
  end;

  ChooseBootup := TMUICycle.Create;
  with ChooseBootup do
  begin
    Entries := ['Empty', 'Local Files', 'Remote Shared'];
    Active := Ini.ReadInteger('General', 'Startup', 0);
    OnActiveChange := @StartupChanged;
    Parent := Grp1;
  end;

  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    FrameTitle := 'File Format';
    Horiz := True;
    Parent := Self;
  end;

  ChooseFormat := TMUICycle.Create;
  with ChooseFormat do
  begin
    Entries := ['CDXL 32 Colors OCS', 'CDXL 256 colors AGA', 'MPEG1'];
    Active := Ini.ReadInteger('General', 'Format', 0);
    OnActiveChange := @FormatChanged;
    Parent := Grp1;
  end;


  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    FrameTitle := 'Search Results';
    Columns := 2;
    Parent := Self;
  end;

  with TMUIText.Create('Maximum Number') do
  begin
    Frame := MUIV_FRAME_NONE;
    Parent := Grp1;
  end;

  NumEdit := TMUIString.Create;
  with NumEdit do
  begin
    Accept := '0123456789';
    IntegerValue := Ini.ReadInteger('Search', 'MaxNum', 10);
    OnAcknowledge := @NumEditACK;
    Parent := Grp1;
  end;

  with TMUIText.Create('Observe clipboard for URLs') do
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


  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    FrameTitle := 'Player';
    Parent := Self;
  end;

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Frame := MUIV_FRAME_NONE;
    Horiz := True;
    Parent := Grp1;
  end;

  with TMUIText.Create('Auto start movie after download') do
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

  // CDXL Player

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    FrameTitle := 'CDXL';
    Columns := 2;
    Parent := Grp1;
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
    Contents := 'Choose';
    FixWidthTxt := ' Choose ';
    OnClick := @ChoosePlayerClick;
    Parent := Grp2;
  end;

  ParamEdit := TMUIString.Create;
  with ParamEdit do
  begin
    Contents := Ini.ReadString('Player', 'Parameter', '%f');
    Parent := Grp2;
  end;

  with TMUIText.Create('Parameter') do
  begin
    FixWidthTxt := ' Parameter ';
    Frame := MUIV_FRAME_NONE;
    Parent := Grp2;
  end;

  // MPEG Player

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    FrameTitle := 'MPEG';
    Columns := 2;
    Parent := Grp1;
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
    Contents := 'Choose';
    FixWidthTxt := ' Choose ';
    OnClick := @ChooseMPEGPlayerClick;
    Parent := Grp2;
  end;

  MPEGParamEdit := TMUIString.Create;
  with MPEGParamEdit do
  begin
    Contents := Ini.ReadString('Player', 'MPEGParameter', '%f');
    Parent := Grp2;
  end;

  with TMUIText.Create('Parameter') do
  begin
    FixWidthTxt := ' Parameter ';
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

