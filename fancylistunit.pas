unit fancylistunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, AGraphics, Utility, intuition, Datatypes, inputevent, EXEC,
  mui, resolutionselunit, Math,
  MUIClass.Base, MUIClass.Group, MUIClass.DrawPanel, MUIClass.Gadget;

type

  { TFancyList }

  TFancyList = class(TMUIGroup)
  private
    TH: Integer;
    BigFont: PTextFont;
    TinyFont: PTextFont;
    FNormFont: PTextFont;
    FDrawPanel: TMUIDrawPanel;
    FResultList: TResultEntries;
    FScroller: TMUIScrollBar;
    FItemHeight: Integer;
    LT: TLoadImgThread;
    FItemIndex: Integer;
    procedure DrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
    procedure KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X, Y: Integer; var EatEvent: Boolean);
    procedure MouseWheelEvent(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean);
    procedure MouseDblClickEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure ScrollerMove(Sender: TObject);
    procedure DrawEntry(Idx: Integer; RP: PRastPort; ARect: TRect);
    //
    procedure LoadImage(Idx: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure ThreadEnd(Sender: TObject);
    procedure InternUpdateList;
  private // colors
    BGColor: LongInt;
    FOnSelectionChange: TNotifyEvent;
    SelBGColor: LongInt;
    Titlecolor: LongInt;
    TextColor: LongInt;
    FAutoLoadImages: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    property List: TResultEntries read FResultList write FResultList;

    procedure UpdateList;
    procedure Redraw;

    procedure MakeItemVisible(Idx: Integer); // Make the Item visible (scroll to view)

    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property AutoLoadImages: boolean read FAutoLoadImages write FAutoLoadImages;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation

{ TFancyList }

procedure TFancyList.DrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  i: Integer;
  Ext: TTextExtent;
  r: TRect;
  ABak, BBak, DBak: LongWord;
  Buffer: TDrawBuffer;
begin
  ABak := GetAPen(RP);
  BBak := GetBPen(RP);
  DBak := GetDrMd(RP);
  {$ifdef AROS}
  SetRast(RP, 0);
  {$endif}

  FNormFont := Rp^.Font;
  if not Assigned(BigFont) then
    BigFont := OpenMUIFont(fkBig);
  if not Assigned(TinyFont) then
    TinyFont := OpenMUIFont(fkTiny);

  SetAPen(Rp, 1);
  AGraphics.TextExtent(RP, 'W', 1, @Ext);
  TH := Ext.te_Height;
  if Assigned(List) then
  begin
    r := Rect(DrawRect.Left, DrawRect.Top - FScroller.First, DrawRect.Right, DrawRect.Top + FItemHeight - FScroller.First);
    Buffer := TDrawBuffer.Create(r.Width, FItemHeight, RP^.BitMap^.Depth, RP^.BitMap);
    try
      for i := 0 to List.Count - 1 do
      begin
        if ((r.Top >= 0) and (r.Top < Height + DrawRect.Top)) or ((r.Bottom > 0) and (r.Bottom <= Height + DrawRect.Top)) then
        begin
          //DrawEntry(i, RP, r);
          Buffer.Clear(2);
          DrawEntry(i, Buffer.RP, Rect(0, 0, r.Width, FItemHeight));
          Buffer.DrawToRastPort(r.Left, r.Top, RP);
        end;
        R.Offset(0, FItemHeight);
      end;

    finally
      Buffer.Free;
    end;
  end;
  if R.Bottom < DrawRect.Bottom then
  begin
    SetAPen(RP, 0);
    RectFill(RP, R.Left, r.Bottom - FItemHeight, r.Right, DrawRect.Bottom);
  end;
  InternUpdateList;

  SetABPenDrMd(RP, ABak, BBak, DBak);
  if Assigned(FNormFont) then
    SetFont(RP, FNormFont);
  FNormFont := nil;
end;

procedure TFancyList.KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
begin
  if not ShowMe or not Assigned(List) then
    Exit;
  if Code = CURSORDOWN then
  begin
    if mssCtrl in Shift then
      FScroller.First := FScroller.First + FItemHeight div 2
    else
    begin
      ItemIndex := Min(ItemIndex + 1, List.Count - 1);
      MakeItemVisible(ItemIndex);
    end;
    EatEvent := True;
  end;
  if Code = CURSORUP then
  begin
    if mssCtrl in Shift then
      FScroller.First := FScroller.First - FItemHeight div 2
    else
    begin
      ItemIndex := Max(0, ItemIndex - 1);
      MakeItemVisible(ItemIndex);
    end;
    EatEvent := True;
  end;
end;

procedure TFancyList.MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X, Y: Integer; var EatEvent: Boolean);
var
  Num: Integer;
begin
  if ShowMe and (MouseBtn = mmbLeft) then
  begin
    EatEvent := True;
    // item from y
    Num := (y + FScroller.First) div FItemHeight;
    if Assigned(List) and InRange(Num, 0, List.Count - 1) then
      SetItemIndex(Num);
  end;
end;

procedure TFancyList.MouseWheelEvent(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean);
begin
  if ScrollUp then
  begin
    FScroller.First := Max(0, FScroller.First - FItemHeight div 2);
  end
  else
  begin
    FScroller.First := Min(FScroller.Entries - FScroller.Visible, FScroller.First + FItemHeight div 2);
  end;
end;

procedure TFancyList.MouseDblClickEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
begin
  if Assigned(List) and InRange(ItemIndex, 0, List.Count - 1) then
    LoadImage(ItemIndex);
end;

procedure TFancyList.ScrollerMove(Sender: TObject);
begin
  Redraw;
end;

function GetNextSpace(s: string; Start: Integer): Integer;
begin
  for Result := Start + 1 to Length(s) do
  begin
    if s = ' ' then
      Exit;
  end;
  Result := Length(s);
end;

procedure TFancyList.DrawEntry(Idx: Integer; RP: PRastPort; ARect: TRect);
var
  y, d, i, n, GoodN: Integer;
  s: string;
  TE: tTextExtent;
  SL: TStringList;
  TextEnd: LongInt;
  DrawRect: TRect;
begin
  DrawRect := ARect;
  if idx = FItemIndex then
  begin
    SetAPen(RP, SelBGcolor);
    SetBPen(RP, SelBgColor);
  end
  else
  begin
    SetAPen(RP, BGcolor);
    SetBPen(RP, BGColor);
  end;
  RectFill(RP, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  if Assigned(List[Idx].DTObj) and (List[Idx].ImgSize.x > 1) then
  begin
    //writeln(' have image, ', idx, ' ', List[Idx].ImgSize.X, ' x ', List[Idx].ImgSize.Y);
    DrawDTObjectA(RP, List[Idx].DTObj, ARect.Left, ARect.Top + 1, List[Idx].ImgSize.x, FItemHeight - 2, 0, 0, nil);
    ARect.Left := ARect.Left + List[Idx].ImgSize.x;
  end
  else
  begin
    //
    if (List[Idx].ImgSize.x = 0) and FAutoLoadImages then
      LoadImage(Idx);
  end;
  //
  SetDrmd(RP, JAM2);
  if Assigned(BigFont) then
  begin
    SetFont(RP, BigFont);
    SetSoftStyle(rp, FSF_BOLD or BigFont^.tf_Style, FSF_BOLD);
  end;
  SetAPen(RP, Titlecolor);
  //
  s := List[Idx].Name;
  AGraphics.gfxMove(Rp, ARect.Left + 5, ARect.Top + TH);
  AGraphics.GfxText(RP, PChar(s), Length(s));
  //
  y := ARect.Top + TH;
  if Assigned(TinyFont) then
    SetFont(RP, TinyFont);
  SetAPen(RP, TextColor);
  TextEnd := ARect.Left;
  if List[Idx].Duration > 0 then
  begin
    d := List[Idx].Duration mod 60;
    s := 'Duration: ' + IntToStr(List[Idx].Duration div 60) + ':' + Format('%2.2d',[d]) + ' ';
    TextExtent(rp, PChar(s), Length(s), @TE);
    AGraphics.gfxMove(Rp, ARect.Left + 5, y + TH);
    AGraphics.GfxText(RP, PChar(s), Length(s));
    TextEnd := ARect.Left + 5 + TE.te_Width;
  end;
  if List[Idx].FileSize > 0 then
  begin
    if (List[Idx].FileSize / 1024) > 1024 then
      s := 'Size: ' + FloatToStrF(List[Idx].FileSize / 1024 / 1024, ffFixed, 8, 1) + ' MByte '
    else
      s := 'Size: ' + IntToStr(Round(List[Idx].FileSize / 1024)) + ' kByte ';
    TextExtent(rp, PChar(s), Length(s), @TE);
    if ARect.Right - TE.te_Width > TextEnd then
    begin
      AGraphics.gfxMove(Rp, ARect.Right - TE.te_Width, y + TH);
      AGraphics.GfxText(RP, PChar(s), Length(s));
    end;
  end;
  if Assigned(FNormFont) then
    SetFont(RP, FNormFont);
  SL := TStringList.Create;
  SL.Text := List[Idx].Desc;
  y := y + TH;
  if ARect.Width > 100 then
  for i := 5 to  SL.Count-1 do
  begin
    s := SL[i];
    if Trim(s) = '' then
    begin
      y := y + 5;
      Continue;
    end;
    GoodN := -1;
    n := 1;
    repeat
      n := Pos(' ',s, n + 1);
      if n = 0 then
        n := Length(s);
      if n = 0 then
        Break;
      TextExtent(rp, PChar(s), n - 1, @TE);
      if TE.te_Width < ARect.Width then
      begin
        GoodN := n;
      end
      else
      begin
        if GoodN <= 0 then
          GoodN := n;
        AGraphics.gfxMove(rp, ARect.Left + 2, y + TH);
        AGraphics.GfxText(RP, PChar(s), GoodN);
        Delete(s, 1, GoodN);
        y := y + TE.te_Height;
        if y + 2 * TE.te_Height >= ARect.Bottom then
          Break;
        GoodN := -1;
        n := 1;
      end;
    until n = Length(s);
    if GoodN > 1 then
    begin
      AGraphics.gfxMove(rp, ARect.Left + 2, y + TE.te_Height);
      AGraphics.GfxText(RP, PChar(s), Length(s));
      y := y + TH;
    end;
    y := y + 5;
    if y + 2 * TE.te_Height >= ARect.Bottom then
      Break;
  end;
  SL.Free;
  // draw focus line
  if idx = FItemIndex then
  begin
    SetAPen(RP, Titlecolor);
    SetDrPt(RP, $0F0F);
    GfxMove(RP, DrawRect.Left, DrawRect.Top);
    Draw(RP, DrawRect.Right - 1, DrawRect.Top);
    Draw(RP, DrawRect.Right - 1, DrawRect.Bottom - 1);
    Draw(RP, DrawRect.Left, DrawRect.Bottom - 1);
    Draw(RP, DrawRect.Left, DrawRect.Top);
    SetDrPt(RP, $FFFF);
  end;
end;

procedure TFancyList.LoadImage(Idx: Integer);
begin
  if Assigned(List) and InRange(Idx, 0, List.Count - 1) then
  begin
    if List[Idx].ImgSize.x < 2 then
      LT.Restart(List[idx]);
  end;
end;

procedure TFancyList.SetItemIndex(AValue: Integer);
begin
  //if FItemIndex = AValue then Exit;
  FItemIndex := AValue;
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
  Redraw;
end;

procedure TFancyList.ThreadEnd(Sender: TObject);
begin
  if Assigned(List) and (List.IndexOf(LT.ItemLink) >= 0) then
  begin
    if Assigned(LT.DTObj) then
    begin
      LT.ItemLink.DTObj := LT.DTObj;
      LT.DTObj := nil;
      LT.ItemLink.DrawHandle := LT.DrawHandle;
      LT.DrawHandle := nil;
      LT.ItemLink.ImgSize := LT.Size;
      LT.ItemLink.IconName := LT.IconName;
    end
    else
    begin
      LT.ItemLink.DTObj := LT.DTObj;
      LT.ItemLink.ImgSize.x := 1;
    end;
    LT.Running := False;
    Redraw;
  end;
end;

procedure TFancyList.InternUpdateList;
var
  FullHeight: Integer;
begin
  FullHeight := (List.Count) * FItemHeight;
  if (FScroller.Entries <> FullHeight) or (FScroller.Visible <> Height) then
  begin
    FScroller.Entries := FullHeight;
    FScroller.Visible := Height;
  end;
end;

constructor TFancyList.Create;
var
  Sc: pScreen;
begin
  inherited Create;
  FAutoLoadImages := True;
  LT := TLoadImgThread.Create;
  LT.OnThreadEnd := @ThreadEnd;
  BigFont := nil;
  TinyFont := nil;
  FNormFont := nil;
  FItemHeight := 122;
  Horiz := True;
  FDrawPanel := TMUIDrawPanel.Create;
  with FDrawPanel do
  begin
    OnDrawObject  := @DrawObject;
    OnMouseDown  := @MouseDownEvent;
    OnMouseWheel  := @MouseWheelEvent;
    OnKeyDown  := @KeyDownEvent;
    OnDblClick := @MouseDblClickEvent;
    FillArea := False;
    Parent := Self;
  end;
  //
  FScroller := TMUIScrollbar.Create;
  FScroller.OnFirstChange  := @ScrollerMove;
  with FScroller do
  begin
    Frame := MUIV_Frame_None;
    Horiz := False;
    Parent := Self;
  end;

  // get colors
  Sc := IntuitionBase^.ActiveScreen;
  BGColor := ObtainBestPen(SC^.ViewPort.ColorMap, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, [TAG_END]); // white
  if BGColor < 0 then
    BGColor := 2;
  SelBGColor := ObtainBestPen(SC^.ViewPort.ColorMap, $FFFFFFFF, $FFFFFFFF, 0, [TAG_END]); // yellow
  if SelBGColor < 0 then
    SelBGColor := 0;
  Titlecolor := ObtainBestPen(SC^.ViewPort.ColorMap, 0, 0, $FFFFFFFF, [TAG_END]); // blue
  if Titlecolor < 0 then
    TitleColor := 3;
  TextColor := ObtainBestPen(SC^.ViewPort.ColorMap, 0, 0, 0, [TAG_END]); // black
  if TextColor < 0 then
    TextColor := 1;
end;

destructor TFancyList.Destroy;
var
  Sc: pScreen;
begin
  LT.Terminate;
  LT.WaitFor;
  LT.Free;
  CloseMUIFont(Bigfont);
  CloseMUIFont(TinyFont);
  Sc := IntuitionBase^.ActiveScreen;
  ReleasePen(SC^.ViewPort.ColorMap, BGColor);
  ReleasePen(SC^.ViewPort.ColorMap, SelBGColor);
  ReleasePen(SC^.ViewPort.ColorMap, Titlecolor);
  ReleasePen(SC^.ViewPort.ColorMap, TextColor);
  inherited Destroy;
end;

procedure TFancyList.UpdateList;
begin
  InternUpdateList;
  Redraw;
end;

procedure TFancyList.Redraw;
begin
  if Assigned(MUIObj) and ShowMe then
    MUI_Redraw(MUIObj, MADF_DRAWOBJECT);
end;

procedure TFancyList.MakeItemVisible(Idx: Integer);
var
  ItemTop, ItemBottom, VisTop, VisBottom: Integer;
begin
  //writeln('enter make visible');
  // check if we have a list and the idx is in the range
  if not Assigned(List) or not InRange(Idx, 0, List.Count - 1) then
    Exit;
  // get some values for easier comparison
  ItemTop := Idx * FItemHeight;
  ItemBottom := ItemTop + FItemHeight;
  //
  VisTop := FScroller.First;
  VisBottom := FScroller.First + FScroller.Visible;
  //writeln('item: ', itemTop, ' - ', ItemBottom, '  Vis: ', VisTop, ' - ', VisBottom);
  //
  // First check if already visible
  if InRange(ItemTop, VisTop, VisBottom) and InRange(ItemBottom, VisTop, VisBottom) then
    Exit;
  if ItemTop < VisTop then
    FScroller.First := ItemTop
  else
    FScroller.First := ItemBottom - FScroller.Visible;
  Redraw;
  //writeln('leave make visible');
end;

end.

