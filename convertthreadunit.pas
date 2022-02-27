unit convertthreadunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, amitubelocale, filedownloadunit, downloadlistunit;

type
{ TStartConvertThread }

  {Conversation and download thread.
  Originally seperate tasks to convert and download, but now only one call needed, also also usable for
  other downloads like the update}
  TStartConvertThread = class(TThread)
  private
    LastTime: Cardinal;
    procedure DoProgress(APercent: integer; AText: string);
    procedure ProgressUpdate(Sender: TObject; Percent, Speed: Integer; ASize: Int64);
  protected
    procedure Execute; override;
    procedure DoOnEnd;
  public
    property Terminated;
  public
    DL: TDownloadEntry;
    //
    FormatID: string; // for direct download FormatID to use as direct download from the JSON list (usually it's a number), if set its a direct download via ytdownload.php
    Filename: string; // filename to save it at, with path
    //
    ID: string;        // id of the video to download
    Movies: string;    // movie directory path
    Desc: string;      // description of the video to save along in the *.txt file
    Format: Integer;   // format description (0=cdxl OCS, 1 CDXL AGA .... see list)
    OnEnd: TNotifyEvent;         // event when threadf ended, must be connected!
    OnProgress: TProgressEvent;  // progress reports to main gui
  end;

implementation


{Progress update when downloading a big file}
procedure TStartConvertThread.ProgressUpdate(Sender: TObject; Percent: integer; Speed: Integer; ASize: Int64);
var
  t1: Cardinal;
  s: string;
begin
  // do not fire too often, GUI will not update that often ;)
  t1 := GetTickCount;
  if t1 - LastTime > 500 then
  begin
    // form status text with downloaded size and overall speed
    s := GetLocString(MSG_STATUS_DOWNLOADING) + '...' + FloatToStrF(ASize/1000/1000, ffFixed, 8, 3) + ' MB';
    if Speed > 0 then
      s := s + ' @' + FloatToStrF(Speed/1000, ffFixed, 8,2) + ' kb/s';
    //
    if Percent = 0 then
      Percent := -1;  // -1 means no change
    // do the actual status bar settting
    DoProgress(Percent, s);
    //
    LastTime := GetTickCount; // update again in 500 ms
  end;
end;

procedure TStartConvertThread.DoProgress(APercent: integer; AText: string);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, APercent, AText);
end;

procedure TStartConvertThread.Execute;
var
  Url: string;
begin
  try
    // conversation thread main routine, only runs once, then the thread ends
    // if Format ID is given -> its a direct download job via ytdownload.php
    // if FormatID is empty, normal conversation
    if FormatID = '' then
    begin
      DoProgress(0, GetLocString(MSG_STATUS_CONVERT));
      Url := ConvertURL + ID + '&format=' + IntToStr(Format);
      try
        // the actual download in FileDownloadUnit, with progressbar, see there
        DownloadFile(@ProgressUpdate, URL, Filename);
        //
        if FileExists(Filename) then // successfull if file exists (it will delete it when error)
        begin
          if Terminated then // we got terminated, then delete the file, it's not complete and exit
          begin
            DeleteFile(Filename);
            Exit;
          end;
          // write the description next to the filename
          with TStringList.Create do
          begin
            Text := Desc;
            SaveToFile(ChangeFileExt(Filename, '.txt'));
            Free;
          end;
        end;
        // finished!
      except
        on e:Exception do
        begin
          writeln('Convert Thread Exception ' + E.Message); // something went horribly wrong, should never happen
        end;
      end;
    end
    else
    begin
      // starts with http -> download directly (like the Updated AmiTube :-D)
      // if not then it's a FormatID
      if Pos('http', FormatID) = 1 then
        Url := FormatID
      else
        Url := DownloadURL + ID + '&format=' + FormatID;
      // the actual download with progress see in FileDownloadUnit
      DownloadFile(@ProgressUpdate, Url, Filename);
    end;
  finally
    Terminate;
    Synchronize(@DoOnEnd); // tell main gui that we are finished the work
  end;
end;

{ call the main Gui event, this should ALWAYS called via Synchronize}
procedure TStartConvertThread.DoOnEnd;
begin
  if Assigned(OnEnd) then
    OnEnd(Self)
end;

end.

