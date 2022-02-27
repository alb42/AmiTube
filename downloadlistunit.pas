unit downloadlistunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, muihelper, mui, amitubelocale,
  MUIClass.Window, MUIClass.Group, MUIClass.StringGrid, MUIClass.Area,
  MUIClass.Dialog;

type
  TDownStatus = (dsWaiting, dsRunning, dsFinished);

  { TDownloadEntry }

  TDownloadEntry = class
  private
    FStatus: TDownStatus;
    procedure SetStatus(AValue: TDownStatus);
  public
    Name: string;
    Id: string;
    FormatID: string;
    Format: Integer;
    filename: string;
    Desc: string;
    property Status: TDownStatus read FStatus write SetStatus;
  end;
  TDownloadEntries = specialize TFPGObjectList<TDownloadEntry>;

  { TDownloadListWin }

  TDownloadListWin = class(TMUIWindow)
  private
    DList: TDownloadEntries;
    FOnDownloadStart: TNotifyEvent;
    List: TMUIStringGrid;
    procedure ClearEntries(Sender: TObject);
    procedure RemoveEntry(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddToList(AName, AId, AFormatID, AFilename, ADesc: string; AFormat: Integer);
    procedure StartNextFree;

    procedure UpdateList;
    property OnDownloadStart: TNotifyEvent read FOnDownloadStart write FOnDownloadStart;
  end;

var
  DownloadListWin: TDownloadListWin;

implementation

function StatusToString(AStatus: TDownStatus):string;
begin
  case AStatus of
    dsWaiting: Result := GetLocString(MSG_STATUS_IDLE); //'Waiting';
    dsRunning: Result := GetLocString(MSG_STATUS_DOWNLOADING);//'Running';
    dsFinished: Result := GetLocString(MSG_STATUS_DONE); //'Finished';
    else
      Result := '(' + IntToStr(Ord(AStatus)) + ')';
  end;
end;

{ TDownloadEntry }

procedure TDownloadEntry.SetStatus(AValue: TDownStatus);
begin
  if FStatus = AValue then Exit;
  FStatus := AValue;
  DownloadListWin.UpdateList;
end;

{ TDownloadListWin }

procedure TDownloadListWin.StartNextFree;
var
  DT: TDownloadEntry;
  i: Integer;
begin
  for i := 0 to DList.Count - 1 do
  begin
    DT := DList[i];
    if DT.Status = dsWaiting then
    begin
      if Assigned(FOnDownloadStart) then
      begin
        FOnDownloadStart(DT);
        UpdateList;
        Exit;
      end;
    end;
  end;
  UpdateList;
end;

procedure TDownloadListWin.UpdateList;
var
  DL: TDownloadEntry;
  i: Integer;
  Prefix: String;
begin
  List.Quiet := True;
  List.NumRows := DList.Count;
  for i := 0 to DList.Count - 1 do
  begin
    DL := DList[i];
    Prefix := '';
    if DL.Status = dsRunning then
      Prefix := MUIX_B;
    List.Cells[0, i] := Prefix + IntToStr(i + 1) + MUIX_N;
    List.Cells[1, i] := Prefix + Copy(DL.Name, 1, 30) + MUIX_N;
    List.Cells[2, i] := Prefix + StatusToString(DL.Status) + MUIX_N;
  end;
  List.Quiet := False;
end;

procedure TDownloadListWin.ClearEntries(Sender: TObject);
var
  i: Integer;
  DL: TDownloadEntry;
begin
  for i := DList.Count - 1 downto 0 do
  begin
    DL := DList[i];
    if DL.Status = dsFinished then
      DList.Delete(i);
  end;
  UpdateList;
end;

procedure TDownloadListWin.RemoveEntry(Sender: TObject);
var
  DL: TDownloadEntry;
begin
  if (List.Row >= 0) and (List.Row < DList.Count) then
  begin
    DL := DList[List.Row];
    if DL.Status = dsRunning then
    begin
      ShowMessage(GetLocString(MSG_ERROR_CANNOTREMOVE)); //'You cannot remove running downloads');
      Exit;
    end;
    DList.Remove(DL);
  end;
end;

constructor TDownloadListWin.Create;
var
  Grp: TMUIGroup;
  Btn: TMUIButton;
begin
  inherited Create;
  Title := GetLocString(MSG_GUI_DOWNLOADLIST); // 'Download List';
  ID := MAKE_ID('D','w','n','L');
  Horizontal := False;
  DList := TDownloadEntries.Create(True);

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Parent := Self
  end;

  Btn := TMUIButton.Create(GetLocString(MSG_GUI_CLEAR_FINISHED));//'Clear finished');
  Btn.OnClick := @ClearEntries;
  Btn.Parent := Grp;

  Btn := TMUIButton.Create(GetLocString(MSG_GUI_REMOVE));
  Btn.Parent := Grp;

  List := TMUIStringGrid.Create;
  with List do
  begin
    NumRows := 0;
    NumColumns := 3;
    ShowTitle := True;
    ShowLines := True;
    Titles[0] := '';
    Titles[1] := GetLocString(MSG_GUI_LISTNAME);
    Titles[2] := GetLocString(MSG_GUI_LISTSTATUS);
    Parent := Self;
  end;

end;

destructor TDownloadListWin.Destroy;
begin
  DList.Free;
  inherited Destroy;
end;

procedure TDownloadListWin.AddToList(AName, AId, AFormatID, AFilename, ADesc: string; AFormat: Integer);
var
  DT: TDownloadEntry;
  i: Integer;
begin
  for i := 0 to DList.Count - 1 do
  begin
    DT := DList[i];
    if LowerCase(DT.filename) = LowerCase(AFilename) then
    begin
      ShowMessage(GetLocString(MSG_ERROR_ALREADYINLIST));
      Exit;
    end;
  end;
  DT := TDownloadEntry.Create;
  DT.Name := AName;
  DT.Id := AId;
  Dt.Format := AFormat;
  DT.FormatID := AFormatID;
  DT.filename := AFilename;
  DT.Desc := ADesc;
  DT.Status := dsWaiting;
  DList.Add(DT);
  StartNextFree;
end;

end.

