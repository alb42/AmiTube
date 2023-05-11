unit AskForIDUnit;

{$mode ObjFPC}{$H+}

interface

uses
  MUIClass.Base, MUIClass.Window, MUIClass.Area, MUIClass.Gadget, MUIClass.Group,
  mui;

type
  TAskForIDWin = class(TMUIWindow)
  private
    FLabel: TMUIText;
    FEdit: TMUIString;
    FOK: TMUIButton;
    FEdit2: TMUIString;
    FEditURL: TMUIString;
    FOK2: TMUIButton;
    FCancel: TMUIButton;
    FID: string;
    procedure EditChange(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure AckURL(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure OK2Click(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Execute;

    property ID: string read FID;
  end;

var
 AskForIDWin: TAskForIDWin;

function Unescape(const s: String): String;


implementation

uses
  AmiTubeLocale, Locale;

constructor TAskForIDWin.Create;
var
 Grp: TMUIGroup;
begin
  inherited;

  Title := GetLocString(MSG_MENU_SEARCHBYID);

  FLabel := TMUIText.Create(GetLocString(MSG_GUI_ENTERVALIDID)); // 'Enter a valid YouTube Video ID (11 chars)');
  FLabel.Frame := MUIV_FRAME_NONE;
  FLabel.Parent := Self;

  Grp := TMUIGroup.Create;
  Grp.Horiz := True;
  Grp.Parent := Self;

  FEdit := TMUIString.Create;
  FEdit.Accept := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabscdefghijklmnopqrstuvwxyz0123456789-_';
  FEdit.MaxLen := 11;
  FEdit.Parent := Grp;
  FEdit.OnContentsChange := @EditChange;

  FOK := TMUIButton.Create(GetLocString(MSG_GUI_SEARCHFORVIDEO)); //'Search for Video');
  FOK.FixWidthTxt := ' Search for Video ';
  FOK.Disabled :=  True;
  FOK.Parent := Grp;
  FOK.OnClick := @OKClick;

  FLabel := TMUIText.Create(GetLocString(MSG_GUI_ENTERVALIDLISTID)); //'Enter a valid YouTube List ID (34 chars)');
  FLabel.Frame := MUIV_FRAME_NONE;
  FLabel.Parent := Self;

  Grp := TMUIGroup.Create;
  Grp.Horiz := True;
  Grp.Parent := Self;

  FEdit2 := TMUIString.Create;
  FEdit2.Accept := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabscdefghijklmnopqrstuvwxyz0123456789-_';
  FEdit2.MaxLen := 34;
  FEdit2.Parent := Grp;
  FEdit2.OnContentsChange := @Edit2Change;

  FOK2 := TMUIButton.Create(GetLocString(MSG_GUI_SEARCHFORLIST)); //'Search for List');
  FOK2.FixWidthTxt := ' Search for Video ';
  FOK2.Disabled :=  True;
  FOK2.Parent := Grp;
  FOK2.OnClick := @OK2Click;


  FLabel := TMUIText.Create(GetLocString(MSG_GUI_ENTERYOUTUBEURL)); //'Insert YouTube URL and press enter to extract ID''s');
  FLabel.Frame := MUIV_FRAME_NONE;
  FLabel.Parent := Self;

  Grp := TMUIGroup.Create;
  Grp.Horiz := True;
  Grp.Parent := Self;

  FEditURL := TMUIString.Create;
  FEditURL.Parent := Grp;
  FEditURL.Contents := 'https://';
  FEditURL.OnAcknowledge := @AckURL;


  Grp := TMUIGroup.Create;
  Grp.Horiz := True;
  Grp.Parent := Self;

  TMUIRectangle.Create.Parent := Grp;

  FCancel := TMUIButton.Create(GetLocString(MSG_GUI_CANCEL));
  FCancel.FixWidthTxt := ' ' + GetLocString(MSG_GUI_CANCEL) + ' ';
  FCancel.Parent := Grp;
  FCancel.OnClick := @CancelClick;
end;

destructor TAskForIDWin.Destroy;
begin
  //
  inherited;
end;

procedure TAskForIDWin.Execute;
begin
  FID := '';
  FOK.Disabled :=  True;
  FOK2.Disabled :=  True;
  FEdit.Contents := '';
  FEdit2.Contents := '';
  FEditURL.Contents := '';
  Show;
end;

procedure TAskForIDWin.EditChange(Sender: TObject);
begin
  FOK.Disabled := Length(FEdit.Contents) <> 11;
end;

procedure TAskForIDWin.Edit2Change(Sender: TObject);
begin
  FOK2.Disabled := Length(FEdit2.Contents) <> 34;
end;

function HexValue(c: Char): Integer;
begin
  case c of
    '0'..'9': Result := ord(c) - ord('0');
    'A'..'F': Result := ord(c) - (ord('A') - 10);
    'a'..'f': Result := ord(c) - (ord('a') - 10);
  else
    Result := 0;
  end;
end;

function Unescape(const s: String): String;
var
  i, RealLength: Integer;
  P: PChar;
begin
  SetLength(Result, Length(s));
  i := 1;
  P := PChar(Result);  { use PChar to prevent numerous calls to UniqueString }
  RealLength := 0;
  while i <= Length(s) do
  begin
    if s[i] = '%' then
    begin
      P[RealLength] := Chr(HexValue(s[i + 1]) shl 4 or HexValue(s[i + 2]));
      Inc(i, 3);
    end else
    begin
      P[RealLength] := s[i];
      Inc(i);
    end;
    Inc(RealLength);
  end;
  SetLength(Result, RealLength);
end;

procedure TAskForIDWin.AckURL(Sender: TObject);
var
  s, s1,s2: string;
  P: LongInt;
  F1, F2: Boolean;
begin
  FEdit.Contents := '';
  FEdit2.Contents := '';
  //
  F1 := False;
  F2 := False;
  // unescape the URL, like https://www.youtube.com/watch%3Fv%3DHo9TbIbtIUY
  s := Unescape(FEditURL.Contents);

  // search for ID
  P := Pos('youtu.be/', LowerCase(s));
  if P > 0 then
  begin
    // with youtu.be/xxxxx
    FEdit.Contents := Copy(s, P + 9);
    F1 := True;
  end
  else
  if Pos('youtube.com/shorts/', Lowercase(s)) > 0 then
  begin
    P := Pos('/shorts/', Lowercase(s));
    FEdit.Contents := Copy(s, P + 8);
    F1 := True;
  end
  else
  begin
    P := Pos('v=', s);
    if P > 0 then
    begin
      P := P + 2;
      s1 := Copy(s, P);
      P := Pos('&', s1);
      if P > 0 then
        s2 := Copy(s1, 1, P - 1)
      else
        s2 := Copy(s1, 1);
      if Length(s2) = 11 then
      begin
        F1 := True;
        FEdit.Contents := s2;
      end;
    end;
    //
    P := Pos('list=', s);
    if P > 0 then
    begin
      P := P + 5;
      s1 := Copy(s, P);
      P := Pos('&', s1);
      if P > 0 then
        s2 := Copy(s1, 1, P - 1)
      else
        s2 := Copy(s1, 1);
      if Length(s2) <= 34 then
      begin
        F2 := True;
        FEdit2.Contents := s2;
      end;
    end;
  end;
  FEditURL.Contents := '';
end;

procedure TAskForIDWin.CancelClick(Sender: TObject);
var
  CloseAction: TCloseAction;
begin
  FID := '';
  Close;
  if Assigned(OnCloseRequest) then
  begin
    CloseAction := caNone;
    OnCloseRequest(Self, CloseAction);
  end;
end;

procedure TAskForIDWin.OKClick(Sender: TObject);
var
  CloseAction: TCloseAction;
begin
  FID := FEdit.Contents;
  Close;
  if Assigned(OnCloseRequest) then
  begin
    CloseAction := caNone;
    OnCloseRequest(Self, CloseAction);
  end;
end;

procedure TAskForIDWin.OK2Click(Sender: TObject);
var
  CloseAction: TCloseAction;
begin
  FID := FEdit2.Contents;
  Close;
  if Assigned(OnCloseRequest) then
  begin
    CloseAction := caNone;
    OnCloseRequest(Self, CloseAction);
  end;
end;

end.

