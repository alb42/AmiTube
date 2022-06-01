unit searchthreadunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, filedownloadunit, resolutionselunit, XMLRead, DOM,
  amitubelocale, prefsunit, fphttpclient, SyncObjs;

type
    { TSearchThread }

  { Search informations by search terms or an ID.
  to saerch for ID supply a standard youtube URL as search string}
  TSearchThread = class(TThread)
  protected
    procedure DoOnEnd;  // called at end via synchronize
    procedure DoProgress(APercent: integer; AText: string); // send a progress report to main thread
    procedure Execute; override;  // amni search function
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property Terminated;
  public
    SearchRes: TResultEntries;     // the actual result
    GetSharedList: Boolean;        // instead of searching load shared list
    ErrMsg: string;                // something went wrong -> error message is here (only if IsError = True)
    IsError: Boolean;              // something went wrong in search
    Search: string;                // search terms or Youtube URL with id
    OnEnd: TNotifyEvent;           // event when thread finished, MUST be attached!
    OnProgress: TProgressEvent;    // progress event when searching
  end;

procedure KillSearch;
function GetFile(address: string; AStream: TStream): Boolean;

  {XML Helper, read a string attribute if exist, else the default is returned}
function GetStringAttribute(ANode: TDOMNode; AttributeName: string; default: string = ''): string; inline;

var
  HPsLock: TCriticalSection;
  HPs: TList;

implementation


{Kill all search threads}
procedure KillSearch;
var
  i: Integer;
begin
  if Assigned(HPs) then
  begin
    HPsLock.Enter;
    try
      for i := 0 to HPs.Count - 1 do
        TFPHTTPClient(HPs[i]).Terminate;
    finally
      HPsLock.Leave;
    end;
  end;
end;

{Basic Get data from URL to a Stream, this one does not support progress bar}
function GetFile(address: string; AStream: TStream): Boolean;
var
  hp: TFPHTTPClient;
begin
  Result := False;
  hp := TFPHTTPClient.Create(nil);
  try
    HPsLock.Enter;
    HPs.Add(hp);  // put to list of current download jobs to kill them with KillSearch
    HPsLock.Leave;
    hp.AllowRedirect := True; // important for redirects, like the amitube.alb42.de is
    hp.AddHeader('User-Agent', ShortVer + ' ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%}); // server will check that this is AmiTube!
    hp.Get(address, AStream); // the actual GET Method
    Result := True;
  finally
    // cleanup, most importantly remove from lsit to kill
    HPsLock.Enter;
    HPs.Remove(HP);
    HPsLock.Leave;
    hp.Free;
    hp := nil;
  end;
end;

{ call the main Gui event, this should ALWAYS called via Synchronize}
procedure TSearchThread.DoOnEnd;
begin
  if Assigned(OnEnd) then
    OnEnd(Self);
end;

{ call maingui with new progress status, no synchromize needed, the maingui will care about the threadsafety}
procedure TSearchThread.DoProgress(APercent: Integer; AText: String);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, APercent, AText);
end;

{XML Helper, read a string attribute if exist, else the default is returned}
function GetStringAttribute(ANode: TDOMNode; AttributeName: string; default: string = ''): string; inline;
var
  Node: TDOMNode;
begin
  Result := Default;
  Node := ANode.Attributes.GetNamedItem(UniCodeString(AttributeName));
  if Assigned(Node) then
    Result := string(Node.NodeValue);
end;

procedure TSearchThread.Execute;
var
  Url, SearchTerm, EncStr: string;
  i, Idx: Integer;
  Mem: TMemoryStream;
  Doc: TXMLDocument;
  Child, Node, FNode: TDOMNode;
  s: String;
  p: Integer;
  Count: Integer;
  AsID: Boolean;
  SRes: TResultEntry;
begin
  // main Search thread routine, only runs once, then the Thread is killed
  DoProgress(0, GetLocString(MSG_STATUS_PREPSEARCH)); // rather useless now, it's fast enough now
  Doc := nil;
  Mem := Nil;
  try
    if GetSharedList then // no search just get the shared list
    begin
      Url := SharedURL;
    end
    else
    begin
      // check if the user supplied a Youtube URL with ID
      // then extract the ID and only search for that particualar ID
      AsID := False;
      Search := Trim(Search);
      // First URL type https://www.youtube.com/watch?v=ID

      if (Pos('https://', LowerCase(Trim(Search))) = 1) then
      begin
        if Pos('https://youtu.be/', LowerCase(Search)) = 1 then
        begin
          Delete(Search, 1, 17);
          AsId := True;
        end
        else
        begin
          p := Pos('list=', Search); // search for "list=" then search for https://
          if p > 0 then
            p := p + 3
          else
            p := Pos('v=', Search); // search for "v=" then search for https://
          if p > 0 then
          begin
            s := Copy(Search, P + 2, Length(Search));
            p := Pos('&', s);
            if p > 1 then
              Delete(s, p, Length(s));
            Search := s;
            AsId := True;
          end;
        end;
      end;
      // convert Search string to UTF8
      SearchTerm := AnsiToUTF8(Search);
      // we can have some strange chars in there, so we convert all to Hex, no problems
      EncStr := '';
      for i := 1 to Length(SearchTerm) do
        EncStr := EncStr + '%' + IntToHex(Ord(SearchTerm[i]),2);
      // for the actual URL
      if AsId then
        Url := SearchURLID + EncStr + '&num=' + IntToStr(Prefs.NumSearch)
      else
        Url := SearchURL + EncStr + '&num=' + IntToStr(Prefs.NumSearch);
    end;
    Mem := TMemoryStream.Create;
    ErrMsg := '';
    IsError := True;
    try
      DoProgress(0, GetLocString(MSG_STATUS_SEARCH));
      //################ the actual searching happens here
      if GetFile(Url, Mem) then
      begin
        // we got a break
        if Terminated then
          Exit;
        DoProgress(0, GetLocString(MSG_STATUS_PARSESEARCH));
        // debugoutput of search result
        {Mem.Position := 0;
        With TStringList.Create do
        begin
          LoadFromStream(Mem);
          Writeln(Text);
          Free;
        end;}
        // try to read the result XML data
        try
          Mem.Position := 0;
          ReadXMLFile(Doc, Mem);
        except
          on E: Exception do
          begin
            writeln('Exception in ReadXMLFile ', E.Message); // something wrong with the XML, should not happen anymore
            Mem.Position := 0;
            With TStringList.Create do
            begin
              LoadFromStream(Mem);
              SaveToFile('PROGDIR:ErrorLog.log');
              //Writeln(Text);
              Exit;
              Free;
            end;
          end;
        end;
        // process the results in the XML
        Child := Doc.DocumentElement.FirstChild;
        Count := Doc.DocumentElement.ChildNodes.Count;
        if Count = 0 then
          Count := 1;
        i := 0;
        while Assigned(Child) do
        begin
          DoProgress(Round(((i + 1) / Count) * 100), GetLocString(MSG_STATUS_PARSESEARCH)); // slows down? ... not anymore main GUI polling instead syncronize
          Inc(i);
          if Child.NodeName <> 'result' then // only results are interesting for us right now
            Continue;
          // result entry creation!
          SRes := TResultEntry.Create;
          SearchRes.Add(SRes); // List of results, we assume nothing can go wrong from here ;)
          // get some data
          SRes.Name := GetStringAttribute(Child, 'fulltitle');
          SRes.Name := StringReplace(SRes.Name, '&amp;', '&', [rfReplaceAll]);
          SRes.Id := GetStringAttribute(Child, 'id');
          SRes.Icon := GetStringAttribute(Child, 'icon');
          SRes.Duration := StrToIntDef(GetStringAttribute(Child, 'duration'), 0); // TODO: observe, sometimes the duration is 0, seldom but happens
          // build the description directly here
          SRes.Desc := SRes.Name + #10#10;
          s := GetStringAttribute(Child, 'uploader');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Uploader: ' + s + #10;
          s := GetStringAttribute(Child, 'like_count');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Likes: ' + s + #10;
          s := GetStringAttribute(Child, 'view_count');
          if s <> '' then
            SRes.Desc := SRes.Desc + 'Views: ' + s + #10;
          s := GetStringAttribute(Child, 'license'); // hmmm
          if s <> '' then
            SRes.Desc := SRes.Desc + 'License: ' + s + #10;
          // #### new! NEW! gather formats! for direct download, later useful?
          Node := Child.FirstChild;
          while Assigned(Node) do
          begin
            // get description, at which point we convert them to Ansi?
            if Node.NodeName = 'description' then
              SRes.Desc := SRes.Desc + #10 + string(Node.TextContent);
            // it's a format desc
            if Node.NodeName = 'formats' then
            begin
              FNode := Node.FirstChild;
              while Assigned(FNode) do
              begin
                if FNode.NodeName = 'format' then
                begin
                  Idx := Length(SRes.Formats);
                  SetLength(SRes.Formats, Idx + 1);
                  SRes.Formats[idx].Title := GetStringAttribute(FNode, 'title');
                  SRes.Formats[idx].ACodec := GetStringAttribute(FNode, 'acodec');
                  SRes.Formats[idx].VCodec := GetStringAttribute(FNode, 'vcodec');
                  SRes.Formats[idx].URL := GetStringAttribute(FNode, 'url');  // careful mostly HUGE, no stripping possible, OS3.9 does not work, too long for command line
                  SRes.Formats[idx].Ext := GetStringAttribute(FNode, 'ext');  // extension, old mobile formats any use, idk?
                  SRes.Formats[idx].FormatID := GetStringAttribute(FNode, 'format_id'); // it seems they are mostly number, but maybe not always
                end;
                FNode := FNode.NextSibling;
              end;
            end;
            // get nextformats
            Node := Node.NextSibling;
          end;
          // get next result
          Child := Child.NextSibling;
        end;
        // yeah we are finished!
        DoProgress(100, GetLocString(MSG_STATUS_SEARCHDONE));
        IsError := False;
      end
      else
      begin
        ErrMsg := GetLocString(MSG_ERROR_GETURL); // cannot get the search result, maybe more error messages here needed? Todo: GetFile with more error output?
      end;
    except
      // 404 and such will end up here
      on E:Exception do
        ErrMsg := E.Message;
    end;
  finally
    Doc.Free;
    Mem.Free;
    Terminate;
    // lets tell the mai GUI we finished
    Synchronize(@DoOnEnd);
  end;
end;


constructor TSearchThread.Create;
begin
  inherited Create(True);
  SearchRes := TResultEntries.Create(False); // search results do not BELONG to the thread, must be copied out on OnEnd Event
end;

destructor TSearchThread.Destroy;
begin
  SearchRes.Free; // do not forget to remove the search entries!
  inherited Destroy;
end;

initialization
  HPsLock := TCriticalSection.Create;
  HPs := TList.Create;
finalization
end.

