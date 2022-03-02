unit historyunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  MUIClass.Base, MUIClass.Window, MUIClass.StringGrid, MUIClass.Gadget;

type

  { THistoryWin }

  THistoryWin = class(TMUIWindow)
  private
    List: TMUIStringGrid;
    FEdit: TMUIString;
    procedure DeactivateEvent(Sender: TObject);
    procedure CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
    procedure ListClick(Sender: TObject);
    procedure ListClickAROS(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Execute(Edit: TMUIString);
  end;

  { TDummyClass }

  TDummyClass = class
    procedure CloseHist(Sender: TObject);
  end;

var
  HistWin: THistoryWin = nil;
  Dummy: TDummyClass;


procedure AddToHistory(AText: string);


implementation

var
  History: TStringList = nil;
  HistName: string = '';
  ListTime: TMUITimer;

procedure AddToHistory(AText: string);
var
  Idx: Integer;
begin
  if Assigned(History) then
  begin
    Idx := History.IndexOf(AText);
    if Idx >= 0 then
      History.Move(Idx, 0)
    else
    begin
      History.Insert(0, AText);
      while History.Count > 100 do
        History.Delete(History.Count - 1);
    end;
  end;

end;

procedure InitHistory;
begin
  History := TStringList.Create;
  HistName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'History.dat';
  if FileExists(HistName) then
  begin
    try
      History.LoadFromFile(HistName);
    except
      ;// nothing, just ignore
    end;
  end;
end;

procedure DeInitHistory;
begin
  if History.Count > 0 then
  begin
    try
    History.SaveToFile(HistName);
    except
      ;// ignore
    end;
  end;
  FreeAndNil(History);
end;

{ TDummyClass }

procedure TDummyClass.CloseHist(Sender: TObject);
begin
  Unused(Sender);
  ListTime.Enabled := False;
  if Assigned(HistWin) then
    HistWin.Close;
end;


{ THistoryWin }

procedure THistoryWin.DeactivateEvent(Sender: TObject);
begin
  Unused(Sender);
  ListTime.Enabled := True;
  //Close;
end;

procedure THistoryWin.CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
begin
  Unused(Sender);
  CloseAction := caClose;
  ListTime.Enabled := False;
  ListTime.OnTimer := nil;
end;

procedure THistoryWin.ListClick(Sender: TObject);
begin
  Unused(Sender);
  ListTime.Enabled := False;
  if Assigned(FEdit) and (List.Row >= 0) then
  begin
    FEdit.Contents := List.Cells[0, List.Row];
  end;
  ListTime.Enabled := True;
end;

procedure THistoryWin.ListClickAROS(Sender: TObject);
begin
  Unused(Sender);
  ListTime.Enabled := True;
end;

constructor THistoryWin.Create;
begin
  inherited Create;

  HelpNode := 'SearchBar';

  Horizontal := True;
  BorderLess := True;
  CloseGadget := False;
  DragBar := False;
  DepthGadget := False;
  SizeGadget := False;

  List := TMUIStringGrid.Create;
  with List do
  begin
    Horiz := True;
    Input := True;
    ShowLines := True;
    ShowTitle := False;

    {$ifdef AROS}
    OnDoubleClick := @ListClick;
    //OnSelectChange := @ListClickAROS;
    {$else}
    OnClick := @ListClick;
    {$endif}
    Parent := Self;
  end;

  if not Assigned(ListTime) then;
    ListTime := TMUITimer.Create;
  ListTime.Interval := 100;
  ListTime.Enabled := False;
  ListTime.OnTimer := @Dummy.CloseHist;

  OnDeactivate := @DeactivateEvent;
end;

destructor THistoryWin.Destroy;
begin
  inherited Destroy;
end;

procedure THistoryWin.Execute(Edit: TMUIString);
var
  i: Integer;
  T: TPoint;
  w: TMUIWindow;
begin
  FEdit := Edit;
  T := Point(Edit.LeftEdge, Edit.TopEdge + Edit.Height);
  w := Edit.WindowObject;
  if Assigned(w) then
  begin
    LeftEdge := T.X + w.LeftEdge;
    TopEdge := T.Y + w.TopEdge;
  end;
  Width := Edit.Width;
  with List do
  begin
    NumColumns := 1;
    NumRows := History.Count;
    for i := 0 to NumRows - 1 do
    begin
      Cells[0, i] := History[i];
    end;
  end;
  Show;
end;

initialization
  Dummy := TDummyClass.Create;
  InitHistory;
finalization
  DeInitHistory;
  Dummy.Free;
end.

