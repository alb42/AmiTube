unit AmiTubeLocale;
{$mode objfpc}{$H+}
interface

{****************************************************

   This file was created automatically by 'FlexCat 2.18'
   from "locale/AmiTube.cd".

   Do NOT edit by hand!

****************************************************}

uses
  Exec , Locale , Utility ;

const
  MSG_MENU_PROJECT = 10000 ;
  MSG_MENU_PROJECT_STR = 'Project'#0;

  MSG_MENU_LOCAL_FILES = 10001 ;
  MSG_MENU_LOCAL_FILES_STR = 'Local Files'#0;

  MSG_MENU_LOCAL_FILES_KEY = 10002 ;
  MSG_MENU_LOCAL_FILES_KEY_STR = 'F'#0;

  MSG_MENU_SHARED = 10003 ;
  MSG_MENU_SHARED_STR = 'Remote Shared'#0;

  MSG_MENU_SHARED_KEY = 10004 ;
  MSG_MENU_SHARED_KEY_STR = 'R'#0;

  MSG_MENU_MAIN_QUIT = 10005 ;
  MSG_MENU_MAIN_QUIT_STR = 'Quit'#0;

  MSG_MENU_MAIN_QUIT_KEY = 10006 ;
  MSG_MENU_MAIN_QUIT_KEY_STR = 'Q'#0;

  MSG_MENU_SETTINGS = 11000 ;
  MSG_MENU_SETTINGS_STR = 'Settings'#0;

  MSG_MENU_PREFS = 11001 ;
  MSG_MENU_PREFS_STR = 'Prefs ...'#0;

  MSG_MENU_PREFS_KEY = 11002 ;
  MSG_MENU_PREFS_KEY_STR = 'P'#0;

  MSG_MENU_ABOUT = 12000 ;
  MSG_MENU_ABOUT_STR = 'About'#0;

  MSG_MENU_ABOUT_AMITUBE = 12001 ;
  MSG_MENU_ABOUT_AMITUBE_STR = 'About AmiTube ...'#0;

  MSG_MENU_ABOUT_MUI = 12002 ;
  MSG_MENU_ABOUT_MUI_STR = 'About MUI ...'#0;

  MSG_MENU_CHECK_UPDATES = 12003 ;
  MSG_MENU_CHECK_UPDATES_STR = 'Check for Updates ...'#0;

  MSG_MENU_SORTBY = 13000 ;
  MSG_MENU_SORTBY_STR = 'Sort by'#0;

  MSG_GUI_BREAK = 20000 ;
  MSG_GUI_BREAK_STR = 'Break'#0;

  MSG_GUI_DOWNLOAD_AS = 20001 ;
  MSG_GUI_DOWNLOAD_AS_STR = 'Get as'#0;

  MSG_GUI_LOAD_ICON = 20002 ;
  MSG_GUI_LOAD_ICON_STR = 'Load Preview'#0;

  MSG_GUI_DELETE = 20003 ;
  MSG_GUI_DELETE_STR = 'Delete Movies'#0;

  MSG_GUI_SHARE = 20004 ;
  MSG_GUI_SHARE_STR = 'Share'#0;

  MSG_GUI_PLAY = 20005 ;
  MSG_GUI_PLAY_STR = 'Play Movie'#0;

  MSG_GUI_GOTURL = 20006 ;
  MSG_GUI_GOTURL_STR = 'Got URL'#0;

  MSG_GUI_GOTURLTEXT = 20007 ;
  MSG_GUI_GOTURLTEXT_STR = 'Found YouTube URL "%s" in clipboard, search for that video?'#0;

  MSG_GUI_YES = 20008 ;
  MSG_GUI_YES_STR = 'Yes'#0;

  MSG_GUI_NO = 20009 ;
  MSG_GUI_NO_STR = 'No'#0;

  MSG_GUI_DISK_FULL = 20010 ;
  MSG_GUI_DISK_FULL_STR = 'Disk full'#0;

  MSG_GUI_FREE = 20011 ;
  MSG_GUI_FREE_STR = 'free'#0;

  MSG_GUI_SELECTFILE = 20012 ;
  MSG_GUI_SELECTFILE_STR = 'Select name/path for file.'#0;

  MSG_GUI_GETORIGINAL = 20013 ;
  MSG_GUI_GETORIGINAL_STR = 'Get Original'#0;

  MSG_GUI_DOWNLOADHD = 20014 ;
  MSG_GUI_DOWNLOADHD_STR = 'Download to HD'#0;

  MSG_GUI_DOWNLOADTOOL = 20015 ;
  MSG_GUI_DOWNLOADTOOL_STR = 'Use Download Tool'#0;

  MSG_GUI_USEURLPLAYER = 20016 ;
  MSG_GUI_USEURLPLAYER_STR = 'Use URL Player'#0;

  MSG_GUI_NOUPDATE = 20017 ;
  MSG_GUI_NOUPDATE_STR = 'No Update available'#0;

  MSG_GUI_UPDATEAVAIL = 20018 ;
  MSG_GUI_UPDATEAVAIL_STR = 'There is an Update available.\n\nYour Version: %o \n Online Version: %n \n\n Download latest Version?'#0;

  MSG_GUI_LISTNUMBER = 20019 ;
  MSG_GUI_LISTNUMBER_STR = 'Nr.'#0;

  MSG_GUI_LISTNAME = 20020 ;
  MSG_GUI_LISTNAME_STR = 'Name'#0;

  MSG_GUI_LISTDURATION = 20021 ;
  MSG_GUI_LISTDURATION_STR = 'Duration'#0;

  MSG_GUI_LISTSIZE = 20022 ;
  MSG_GUI_LISTSIZE_STR = 'Filesize'#0;

  MSG_STATUS_IDLE = 30000 ;
  MSG_STATUS_IDLE_STR = 'Idle'#0;

  MSG_STATUS_SHARED = 30001 ;
  MSG_STATUS_SHARED_STR = 'Successfully shared.'#0;

  MSG_STATUS_PREPSEARCH = 30002 ;
  MSG_STATUS_PREPSEARCH_STR = 'Prepare Search.'#0;

  MSG_STATUS_SEARCH = 30003 ;
  MSG_STATUS_SEARCH_STR = 'Search started.'#0;

  MSG_STATUS_PARSESEARCH = 30004 ;
  MSG_STATUS_PARSESEARCH_STR = 'Parse search result.'#0;

  MSG_STATUS_SEARCHDONE = 30005 ;
  MSG_STATUS_SEARCHDONE_STR = 'Search done.'#0;

  MSG_STATUS_CONVERT = 30006 ;
  MSG_STATUS_CONVERT_STR = 'Converting movie'#0;

  MSG_STATUS_DOWNLOADING = 30007 ;
  MSG_STATUS_DOWNLOADING_STR = 'Downloading'#0;

  MSG_ERROR_LOCAL = 40000 ;
  MSG_ERROR_LOCAL_STR = 'No locally saved movie files found'#0;

  MSG_ERROR_SHARE = 40001 ;
  MSG_ERROR_SHARE_STR = 'Error sharing:'#0;

  MSG_ERROR_GETURL = 40002 ;
  MSG_ERROR_GETURL_STR = 'Error get URL'#0;

  MSG_ERROR_CONVERT = 40003 ;
  MSG_ERROR_CONVERT_STR = 'Error converting:'#0;

  MSG_ERROR_LOAD_ICON = 40004 ;
  MSG_ERROR_LOAD_ICON_STR = 'Error load Icon.'#0;

  MSG_ERROR_PLAYER = 40005 ;
  MSG_ERROR_PLAYER_STR = 'Error: "%s" not found.'#0;

  MSG_ERROR_NO_SPACE = 40006 ;
  MSG_ERROR_NO_SPACE_STR = 'Not enough space to download this movie. Continue anyway?'#0;

  MSG_ERROR_UPDATE = 40007 ;
  MSG_ERROR_UPDATE_STR = 'Error checking for Update.'#0;

  MSG_ERROR_ALREADYRUN = 40008 ;
  MSG_ERROR_ALREADYRUN_STR = 'Another downloading job is running, wait for finish before starting a new one'#0;

  MSG_PREFS_WINDOW = 50000 ;
  MSG_PREFS_WINDOW_STR = 'Settings'#0;

  MSG_PREFS_STARTUP = 50100 ;
  MSG_PREFS_STARTUP_STR = 'Start up list contents'#0;

  MSG_PREFS_STARTUP1 = 50101 ;
  MSG_PREFS_STARTUP1_STR = 'Empty'#0;

  MSG_PREFS_STARTUP2 = 50102 ;
  MSG_PREFS_STARTUP2_STR = 'Local Files'#0;

  MSG_PREFS_STARTUP3 = 50103 ;
  MSG_PREFS_STARTUP3_STR = 'Remote Shared'#0;

  MSG_PREFS_FORMAT = 50200 ;
  MSG_PREFS_FORMAT_STR = 'File Format'#0;

  MSG_PREFS_FORMAT1 = 50201 ;
  MSG_PREFS_FORMAT1_STR = 'CDXL 32 Colors OCS'#0;

  MSG_PREFS_FORMAT2 = 50202 ;
  MSG_PREFS_FORMAT2_STR = 'CDXL 256 Colors AGA'#0;

  MSG_PREFS_FORMAT3 = 50203 ;
  MSG_PREFS_FORMAT3_STR = 'MPEG1'#0;

  MSG_PREFS_FORMAT4 = 50204 ;
  MSG_PREFS_FORMAT4_STR = 'CDXL 256 Colors Larger'#0;

  MSG_PREFS_SEARCH = 50300 ;
  MSG_PREFS_SEARCH_STR = 'Search Results'#0;

  MSG_PREFS_SEARCHNUM = 50301 ;
  MSG_PREFS_SEARCHNUM_STR = 'Maximum Number'#0;

  MSG_PREFS_CLIP = 50302 ;
  MSG_PREFS_CLIP_STR = 'Observe clipboard for URLs'#0;

  MSG_PREFS_PLAYER = 50400 ;
  MSG_PREFS_PLAYER_STR = 'Player'#0;

  MSG_PREFS_AUTOSTART = 50401 ;
  MSG_PREFS_AUTOSTART_STR = 'Auto start movie after download'#0;

  MSG_PREFS_CHOOSEPLAYER = 50402 ;
  MSG_PREFS_CHOOSEPLAYER_STR = 'Choose'#0;

  MSG_PREFS_PLAYERPARAM = 50403 ;
  MSG_PREFS_PLAYERPARAM_STR = 'Parameter'#0;

  MSG_PREFS_ALLFORMATS = 50404 ;
  MSG_PREFS_ALLFORMATS_STR = 'Show all Formats'#0;

  MSG_PREFS_AUTOICON = 50405 ;
  MSG_PREFS_AUTOICON_STR = 'Auto load Preview Icon'#0;

  MSG_PREFS_MAXTITLELEN = 50406 ;
  MSG_PREFS_MAXTITLELEN_STR = 'Maximum Title length'#0;

  MSG_PREFS_ASKDESTINATION = 50407 ;
  MSG_PREFS_ASKDESTINATION_STR = 'Ask for destination path'#0;


procedure CloseCatalog;
procedure OpenCatalog(Loc: PLocale);
function GetLocString(Num: LongInt): PChar;

implementation

const
  Builtinlanguage = 'english'#0;
  Version = 0 ;
  Catalog: PCatalog = NIL ;

type

  TAppString = record
     id: LongInt;
     str: string;
  end;

  TAppStringArray = array[0..76] of TAppString;

const
  AppStrings: TAppStringArray = (
    (id: MSG_MENU_PROJECT ; str: MSG_MENU_PROJECT_STR ),
    (id: MSG_MENU_LOCAL_FILES ; str: MSG_MENU_LOCAL_FILES_STR ),
    (id: MSG_MENU_LOCAL_FILES_KEY ; str: MSG_MENU_LOCAL_FILES_KEY_STR ),
    (id: MSG_MENU_SHARED ; str: MSG_MENU_SHARED_STR ),
    (id: MSG_MENU_SHARED_KEY ; str: MSG_MENU_SHARED_KEY_STR ),
    (id: MSG_MENU_MAIN_QUIT ; str: MSG_MENU_MAIN_QUIT_STR ),
    (id: MSG_MENU_MAIN_QUIT_KEY ; str: MSG_MENU_MAIN_QUIT_KEY_STR ),
    (id: MSG_MENU_SETTINGS ; str: MSG_MENU_SETTINGS_STR ),
    (id: MSG_MENU_PREFS ; str: MSG_MENU_PREFS_STR ),
    (id: MSG_MENU_PREFS_KEY ; str: MSG_MENU_PREFS_KEY_STR ),
    (id: MSG_MENU_ABOUT ; str: MSG_MENU_ABOUT_STR ),
    (id: MSG_MENU_ABOUT_AMITUBE ; str: MSG_MENU_ABOUT_AMITUBE_STR ),
    (id: MSG_MENU_ABOUT_MUI ; str: MSG_MENU_ABOUT_MUI_STR ),
    (id: MSG_MENU_CHECK_UPDATES ; str: MSG_MENU_CHECK_UPDATES_STR ),
    (id: MSG_MENU_SORTBY ; str: MSG_MENU_SORTBY_STR ),
    (id: MSG_GUI_BREAK ; str: MSG_GUI_BREAK_STR ),
    (id: MSG_GUI_DOWNLOAD_AS ; str: MSG_GUI_DOWNLOAD_AS_STR ),
    (id: MSG_GUI_LOAD_ICON ; str: MSG_GUI_LOAD_ICON_STR ),
    (id: MSG_GUI_DELETE ; str: MSG_GUI_DELETE_STR ),
    (id: MSG_GUI_SHARE ; str: MSG_GUI_SHARE_STR ),
    (id: MSG_GUI_PLAY ; str: MSG_GUI_PLAY_STR ),
    (id: MSG_GUI_GOTURL ; str: MSG_GUI_GOTURL_STR ),
    (id: MSG_GUI_GOTURLTEXT ; str: MSG_GUI_GOTURLTEXT_STR ),
    (id: MSG_GUI_YES ; str: MSG_GUI_YES_STR ),
    (id: MSG_GUI_NO ; str: MSG_GUI_NO_STR ),
    (id: MSG_GUI_DISK_FULL ; str: MSG_GUI_DISK_FULL_STR ),
    (id: MSG_GUI_FREE ; str: MSG_GUI_FREE_STR ),
    (id: MSG_GUI_SELECTFILE ; str: MSG_GUI_SELECTFILE_STR ),
    (id: MSG_GUI_GETORIGINAL ; str: MSG_GUI_GETORIGINAL_STR ),
    (id: MSG_GUI_DOWNLOADHD ; str: MSG_GUI_DOWNLOADHD_STR ),
    (id: MSG_GUI_DOWNLOADTOOL ; str: MSG_GUI_DOWNLOADTOOL_STR ),
    (id: MSG_GUI_USEURLPLAYER ; str: MSG_GUI_USEURLPLAYER_STR ),
    (id: MSG_GUI_NOUPDATE ; str: MSG_GUI_NOUPDATE_STR ),
    (id: MSG_GUI_UPDATEAVAIL ; str: MSG_GUI_UPDATEAVAIL_STR ),
    (id: MSG_GUI_LISTNUMBER ; str: MSG_GUI_LISTNUMBER_STR ),
    (id: MSG_GUI_LISTNAME ; str: MSG_GUI_LISTNAME_STR ),
    (id: MSG_GUI_LISTDURATION ; str: MSG_GUI_LISTDURATION_STR ),
    (id: MSG_GUI_LISTSIZE ; str: MSG_GUI_LISTSIZE_STR ),
    (id: MSG_STATUS_IDLE ; str: MSG_STATUS_IDLE_STR ),
    (id: MSG_STATUS_SHARED ; str: MSG_STATUS_SHARED_STR ),
    (id: MSG_STATUS_PREPSEARCH ; str: MSG_STATUS_PREPSEARCH_STR ),
    (id: MSG_STATUS_SEARCH ; str: MSG_STATUS_SEARCH_STR ),
    (id: MSG_STATUS_PARSESEARCH ; str: MSG_STATUS_PARSESEARCH_STR ),
    (id: MSG_STATUS_SEARCHDONE ; str: MSG_STATUS_SEARCHDONE_STR ),
    (id: MSG_STATUS_CONVERT ; str: MSG_STATUS_CONVERT_STR ),
    (id: MSG_STATUS_DOWNLOADING ; str: MSG_STATUS_DOWNLOADING_STR ),
    (id: MSG_ERROR_LOCAL ; str: MSG_ERROR_LOCAL_STR ),
    (id: MSG_ERROR_SHARE ; str: MSG_ERROR_SHARE_STR ),
    (id: MSG_ERROR_GETURL ; str: MSG_ERROR_GETURL_STR ),
    (id: MSG_ERROR_CONVERT ; str: MSG_ERROR_CONVERT_STR ),
    (id: MSG_ERROR_LOAD_ICON ; str: MSG_ERROR_LOAD_ICON_STR ),
    (id: MSG_ERROR_PLAYER ; str: MSG_ERROR_PLAYER_STR ),
    (id: MSG_ERROR_NO_SPACE ; str: MSG_ERROR_NO_SPACE_STR ),
    (id: MSG_ERROR_UPDATE ; str: MSG_ERROR_UPDATE_STR ),
    (id: MSG_ERROR_ALREADYRUN ; str: MSG_ERROR_ALREADYRUN_STR ),
    (id: MSG_PREFS_WINDOW ; str: MSG_PREFS_WINDOW_STR ),
    (id: MSG_PREFS_STARTUP ; str: MSG_PREFS_STARTUP_STR ),
    (id: MSG_PREFS_STARTUP1 ; str: MSG_PREFS_STARTUP1_STR ),
    (id: MSG_PREFS_STARTUP2 ; str: MSG_PREFS_STARTUP2_STR ),
    (id: MSG_PREFS_STARTUP3 ; str: MSG_PREFS_STARTUP3_STR ),
    (id: MSG_PREFS_FORMAT ; str: MSG_PREFS_FORMAT_STR ),
    (id: MSG_PREFS_FORMAT1 ; str: MSG_PREFS_FORMAT1_STR ),
    (id: MSG_PREFS_FORMAT2 ; str: MSG_PREFS_FORMAT2_STR ),
    (id: MSG_PREFS_FORMAT3 ; str: MSG_PREFS_FORMAT3_STR ),
    (id: MSG_PREFS_FORMAT4 ; str: MSG_PREFS_FORMAT4_STR ),
    (id: MSG_PREFS_SEARCH ; str: MSG_PREFS_SEARCH_STR ),
    (id: MSG_PREFS_SEARCHNUM ; str: MSG_PREFS_SEARCHNUM_STR ),
    (id: MSG_PREFS_CLIP ; str: MSG_PREFS_CLIP_STR ),
    (id: MSG_PREFS_PLAYER ; str: MSG_PREFS_PLAYER_STR ),
    (id: MSG_PREFS_AUTOSTART ; str: MSG_PREFS_AUTOSTART_STR ),
    (id: MSG_PREFS_CHOOSEPLAYER ; str: MSG_PREFS_CHOOSEPLAYER_STR ),
    (id: MSG_PREFS_PLAYERPARAM ; str: MSG_PREFS_PLAYERPARAM_STR ),
    (id: MSG_PREFS_ALLFORMATS ; str: MSG_PREFS_ALLFORMATS_STR ),
    (id: MSG_PREFS_AUTOICON ; str: MSG_PREFS_AUTOICON_STR ),
    (id: MSG_PREFS_MAXTITLELEN ; str: MSG_PREFS_MAXTITLELEN_STR ),
    (id: MSG_PREFS_ASKDESTINATION ; str: MSG_PREFS_ASKDESTINATION_STR ),
    (id: 0 ; str: '' )
    );

procedure CloseCatalog;
begin
  if Assigned(Catalog) then
  begin
    Locale.CloseCatalog(Catalog) ;
    Catalog := nil;
  end;
end;

procedure OpenCatalog(loc: PLocale);
var
   tags: array[0..7] of PtrUInt;
begin
  CloseCatalog;
  if (Catalog = nil) and (LocaleBase <> NIL) then
  begin
    tags[0] := OC_BuiltInLanguage; tags[1] := 0; //AsTag(PChar(builtinlanguage));
    tags[2] := OC_Version;         tags[3] := Version;
    tags[4] := TAG_END;
  end;
  Catalog := Locale.OpenCatalogA(loc, PChar('AmiTube.catalog'#0), @tags);
end;

function GetLocString(Num: LongInt): STRPTR;
var
  i: LongInt;
  Idx: Integer;
  Default: STRPTR;
begin
  Idx := -1;

  for i := 0 to High(Appstrings) do
  begin
    if AppStrings[i].id = Num then
    begin
      Idx := i;
      Break;
    end;
  end;

  if Idx >= 0 then
    Default := PChar(AppStrings[i].str)
  else
    Default := nil;

  if Assigned(Catalog) then
    GetLocString := Locale.GetCatalogStr(Catalog, Num, Default)
  else
    GetLocString := Default
end;

initialization
  OpenCatalog(nil);
finalization
  CloseCatalog;
end.
