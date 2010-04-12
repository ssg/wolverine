{
Wolverine types and base procedures

updates:
--------
25th Jun 96 - 13:14 - added uudecode proc...
14th Aug 96 - 17:43 - the revolution begins...
21st Aug 96 - 15:43 - added off-line config support...
23rd Aug 96 - 14:53 - multilingual adaptation...
}

unit WTypes;

interface

uses Tools,XStream,XScroll,XPrn,XColl,Dos,Objects,XTypes;

type

  TReaderColor = record
    Name       : string[30];
    Color      : byte;
  end;

  TDOSDateTime = record
    Date,Time  : word;
  end;

  TColorScheme = record
    Name       : string[40];
    Pal        : TRGBPalette;
  end;

  TSetupResult = (setupOK, setupNone, setupMismatch);
  TSetupStream = TCodedStream;

  TPakType = (pakUnknown,pakWHAM,pakBW,pakQWK,pakHudson);

  TWriteProc = procedure(s:string);

  PArea = ^TArea;
  TArea = record
    Name     : string[49];
    Number   : word;
    AreaType : byte;
    UserStat : byte;
    Flags    : word;
    Total    : word;
    Pers     : word;
    Unread   : word;
    EchoTag  : string[20];
  end;

  TFilterProc = function(P:PArea):boolean;

  TXTIRec = record
    Flags : byte;
    Marks : byte;
  end;

  TAddr = record
    Zone,Net,Node,Point : word;
  end;

  PMsg = ^TMsg;
  TMsg = record
    From     : string[35];
    Too      : string[35];
    Subj     : string[71];
    Addr     : TAddr;
    Date     : longint;
    Area     : PArea;
    Flags    : byte;
    Marks    : byte;
    case boolean of
      True : (Where  : longint;
              Length : longint;
              FTINum   : word;);
      False : (Filename:string[12];
               Netflags:word;);
  end;

  PFilter = ^TFilter;
  TFilter = record
    Desc   : FnameStr;
    Author : FnameStr;
    Cmd    : string[12];
    Batch  : boolean;
  end;

  PPerson = ^TPerson;
  TPerson = record {address book item}
    Name    : string[35];
    Netmail : TAddr;
    Tel1    : string[20];
    Tel2    : string[20];
    Notes   : array[1..4] of string[70];
  end;

  TPersonScr = record
    Name    : string[35];
    Netmail : string[20];
    Tel1    : string[20];
    Tel2    : string[20];
    Notes   : array[1..4] of string[70];
  end;

  THdrInfo = record
    From : string[35];
    Too  : string[35];
    Subj : string[71];
    Addr : TAddr;
    Date : longint;
    Netmail : boolean;
    Reply   : boolean;
  end;

  TUser = record
    Name  : string[42];
    Alias : string[42];
    SysOp : string[40];
    BBS   : string[64];
    NetFlags : word;
  end;

  PMsgText = ^TMsgText;
  TMsgText = record
    Next   : PMsgText;
    Size   : word;
    Data   : PChar;
  end;

  PPacket = ^TPacket;
  TPacket = record
    Size       : longint;
    Time       : longint;

    Total      : longint;
    Unread     : longint;
    WillRead   : longint;
    WillReply  : longint;
    Personal   : longint;
    Name       : string[12];
  end;

  PBBSinfo = ^TBBSInfo;
  TBBSInfo = record
    BBSName : string[64];
    InfName : string[12];
  end;

  TSearchScr = record
    From     : string[35];
    Too      : string[35];
    Subj     : string[71];
    Text     : string[71];
  end;

  TSetupRec = record
    DLDir     : FnameStr;
    ULDir     : FnameStr;
    EditCmd   : FnameStr;
    FilterCmd : string[255];
    QuoteStr  : FnameStr;
    Password  : string[15];
    Flags     : longint;
    Behaviour : word;
    MsgWindow : TRect;
    readerFont  : word;
    readerFontH : word;
    ColorScheme : string[40];
    RClock      : TRect;
    RCDPlayer   : TRect;
{    RPerc       : TRect;}
    lastSearch  : TSearchScr;
  end;

  TSetupScr = record
    DLDir     : FnameStr;
    ULDir     : FnameStr;
    EditCmd   : FnameStr;
    FilterCmd : string[255];
    QuoteStr  : FnameStr;
    Password  : string[15];
    AreaSort  : boolean;
    FallenAngel : boolean;
    AskPost     : boolean;
    ScrSaver    : boolean;
    PersBeep    : boolean;
    ReplySafe   : boolean;
    TaglineSort : boolean;
    PersCheckFirst : boolean;
    NoVivaLaSewage : boolean;
    UnreadReminder : boolean;
    Behaviour : word;
  end;

  {TSearchRec = record
    key      : FnameStr;
    how      : word;
  end;}

  TReplyScr = record
    Area     : string[49];
    From     : string[35];
    Too      : string[35];
    Subj     : string[71];
    Addr     : string[25];
  end;

  TInfHeader = record
    ver          : byte;
    ReaderFiles  : array[1..5] of array[1..13] of char;
    RegNum       : array[1..9] of char;
    MashType     : byte; {zero}
    LoginName    : array[1..43] of char;
    AliasName    : array[1..43] of char;
    password     : array[1..21] of char;
    Passtype     : byte;
    zone,net,node,point:word;
    SysOp        : array[1..41] of char;
    Unused1      : word;
    SystemName   : array[1..65] of char;
    MaxFReqs     : byte;
    Unused2      : array[1..6] of byte;
    UFlags       : word;
    Keywords     : array[1..10] of array[1..21] of char;
    Filters      : array[1..10] of array[1..21] of char;
    Macros       : array[1..3] of array[1..80] of char;
    NetmailFlags : word;
    Credits      : word;
    Debits       : word;
    Can_Forward  : boolean;
    HeaderLen    : word;
    Areainfo_Len : word;
    MixLen       : word;
    FtiLen       : word;
    UsesUPL      : boolean;
    FromToLen    : byte; {Fix to 35 even if zero}
    SubjLen      : byte; {Fix 71 vice versa}
    PacketId     : array[1..9] of char;
    Reserved     : array[1..234] of byte;
  end;

  TWHAMInfo = record
    Sign     : longint;
    Flags    : longint;
    Netflags : word;
    System   : string[39];
    SysOp    : string[39];
    Addr     : TAddr;
    Owner    : string[39];
    Alias    : string[39];
    ProgName : string[39];
    Pad      : array[1..294] of byte; {512 bytes}
  end;

  PTaglineCollection = ^TTaglineCollection;
  TTaglineCollection = object(TStringCollection)
    function Compare(k1,k2:pointer):integer;virtual;
  end;

  PBBSInfoColl = ^TBBSInfoColl;
  TBBSInfoColl = object(TSortedCollection)
    function Compare(k1,k2:pointer):integer;virtual;
    procedure FreeItem(item:pointer);virtual;
  end;

  PPersonColl = ^TPersonColl;
  TPersonColl = object(TSortedCollection)
    function Compare(k1,k2:pointer):integer;virtual;
    procedure FreeItem(item:pointer);virtual;
  end;

  PAreaColl = ^TAreaColl;
  TAreaColl = object(TSortedCollection)
    procedure FreeItem(item:pointer);virtual;
    function  Compare(k1,k2:pointer):integer;virtual;
  end;

  PPacketColl = ^TPacketColl;
  TPacketColl = object(TSortedCollection)
    function Compare(k1,k2:pointer):integer;virtual;
    procedure FreeItem(item:pointer);virtual;
  end;

  PMsgPacket = ^TMsgPacket;
  TMsgPacket = object(TObject)
    InfoArea : PArea;
    areaList : PAreaColl;
    msgList  : PSizedCollection;
    Where    : FnameStr;
    PacketId : string[8];
    Config   : word;
    constructor Init(awhere,apacketid:FnameStr);
    destructor Done;virtual;
    function   Load:boolean;virtual;
    function   ReadMsgText(amsg:PMsg):PMsgText;virtual;
    function   GetType:TPakType;virtual;
    procedure  PostMsg(amsg:PMsg; afile:FnameStr);virtual;
    procedure  OLC(aarealist:PAreaColl);virtual;
    procedure  UpdateMsgFlags(amsg:PMsg);virtual;
    procedure  WriteMsgHeader(amsg:Pmsg; anew:boolean);virtual;
    procedure  MsgToFile(amsg:Pmsg; afile:FnameStr);virtual;
    procedure  FileToMsg(afile:FnameStr; amsg:PMsg);virtual;
    procedure  DeleteMsg(amsg:Pmsg);virtual;
    procedure  AddInfo(fn,sbj:FnameStr);
    procedure  Clear;
    procedure  CacheTo(towhere:FnameStr);virtual;
    function   GetPacketid(filespec:FnameStr):FnameStr;
    function   GetSpecificCount(func:pointer):longint;
    function   FindSpecificItems(func:pointer; internal:boolean):PDumbCollection;
  end;

  TNetScr = record
    Crash : boolean;
    KSent : boolean;
    Hold  : boolean;
    Immed : boolean;
    Direct : boolean;
  end;

var

  user : TUser;

const

  maxreadercolors = 6;

  lineChar        = 'Ä';

  runFaast        : boolean = false;

  {mail packet config}
  mpcCanPost      = 1;  {can post msgs}
  mpcCanDelete    = 2;  {can delete msgs}
  mpcCanOLC       = 4;  {can establish offline config}
  mpcInfo         = 8;  {info areas allowed}
  mpcCanCache     = 16; {packet cacheable?}

  VivaLaSewagePercent = 80; { %80 quote limit }

  rcBackground = 1;
  rcNormalText = 2;
  rcTagline    = 3;
  rcQuote      = 4;
  rcTearline   = 5;
  rcOrigin     = 6;

  watLocal     = 0;
  watEcho      = 1;
  watNetmail   = 2;
  watEMail     = 3;
  watInfo      = 4;
  watArchive   = 5;

  wusNone      = 0;
  wusPers      = 1;
  wusPAll      = 2;
  wusAll       = 3;

  wafAlias     = 1;
  wafPost      = 2;

  wmfRead      = 1;
  wmfReplied   = 2;
  wmfPersonal  = 4;
  wmfFileLink  = 16;
  wmfDeleted   = 128;

  wmmSave      = 1;
  wmmReply     = 2; {bluewave compatible}
  wmmRead      = 8;

  wnfCrash     = 1;
  wnfKill      = 2;
  wnfHold      = 4;
  wnfDirect    = 8;
  wnfImmed     = 16;

  ufNoAreaSort      = 1;    { do not sort area list }
  ufFallenAngel     = 2;    { fallenangel enabled }
  ufAskPost         = 4;    { ask before posting msg }
  ufScrSaver        = 8;    { screen saver enabled }
  ufPersBeep        = 16;   { beep on pers }
  ufReplySafe       = 32;   { update reply packet after each msg post }
  ufNoConflictCheck = 64;   { warn when opening non-context packet }
  ufPersCheckFirst  = 128;  { personal mail check each time a packet has been opened }
  ufNoTaglineSort   = 256;  { do not sort tagline lists }
  ufNoVivaLaSewage  = 512;  { use viva-la-sewage quote guard }
  ufUnreadReminder  = 1024; { warn for unread msgs before closing packet }

  tbNone        = 1; {tagline behaviour}
  tbAsk         = 2;
  tbAuto        = 3;

  runTaglineManager = 1;
  runAddressBook    = 2;
  runColors         = 4;
  runPentagon       = 8;
  runOLC            = 16;

  wkdir      : PString = NIL;
  stdpakdir  : PString = NIL;
  stdrepDir  : PString = NIL;
  infDir     : PString = NIL;
  tempDir    : PString = NIL;
  fltDir     : PString = NIL;
  txtDir     : PString = NIL;

  currentPacketName : FnameStr = '';

  menuList   : PTextCollection  = NIL;
  filterList : PSizedCollection = NIL;

  Packet      : PMsgPacket = NIL;
  ReplyPacket : PMsgPacket = NIL;

  nVersion   = 2; {major version}
  rVersion   : string[11] = char(byte('0')+nVersion)+'.13';
  appName    : string[9] = 'Wolverine';

  packetOpen : boolean = false;

  idBackground = 100;
  idW          = 203;
  idAddr       = 204;
  idTagline    = 205;
  idMagni      = 226;
  idSetup      = 200;
  idPrompt     = 201;
  idExit       = 202;
  idAreaFull   = $cf;
  idAreaEmpty  = $d1;
  idAreaLink   = $e6;

  fallLimit    = 5; {5 strokes}
  fallenAngelCounter : byte = 0;
  mins = 1080;
  saverDelay   = 15*mins;

  wrapLimit = 78; {wrap at}

  StatusLine : PDynamicLabel = NIL;

  {$IFNDEF DPMI}
  ovrFile    : string[12] = 'W.Ovr';
  {$ENDIF}
  resFile    : string[12] = 'W.Rif';
  lanFile    : string[12] = 'W.Lan';
  tagFile    : string[12] = 'W.Tag';
  setupFile  : string[12] = 'W.Cfg';
  helpFile   : string[12] = 'W.Hlp';
  menuFile   : string[12] = 'W.Mnu';
  keyFile    : string[12] = 'W.Key';
  colFile    : string[12] = 'W.Col';
  addrFile   : string[12] = 'W.Adr';
  cacheFile  : string[12] = 'W.Pkt';
  arcFile    : string[12] = 'W.Ari';
  infFile    : string[12] = 'W.Inf';
  tempFile   : string[12] = 'msgtemp.txt';
  filterFile : string[12] = 'filter.out';

  defaultSchemeName = 'Wolverine';

  Interpreter : PString = NIL;

  MinViewerWindowSize : TPoint = (X:320;Y:165);

  BatchExtensions  = '.BAT.BTM';
  BinaryExtensions = '.EXE.COM';
  ExecutableExtensions = BatchExtensions + BinaryExtensions;

  setupFileSize = SizeOf(TSetupRec) + SizeOf(prnFile);

  scrollerXSize=sbButtonSize;

  {search modes}
  smFrom = 1;
  smTo   = 2;
  smSubj = 3;
  smText = 4;

  fntProp      = 2;
  fntPropSmall = 3;
  fntPropBig   = 4;
  fntPropTimes = 5;
  fntFixedBold = 6;
  fntFixedNorm = 7;

  wfForward   = 1;
  wfQuote     = 2;
  wfNetmail   = 4;
  wfCrosspost = 8;

  dtcAlarm  = 1;

  acNormal   = 0; {no hideself and nonetmail}
  acNetmail  = 1; {list only netmail areas}
  acHideSelf = 2; {hide self area}

  cmIsThatYou            = 50400;
  cmDecode               = 50401;
  cmReply                = 50402;
  cmAreaChange           = 50403;
  cmNetReply             = 50404;
  cmForward              = 50405;
  cmRandom               = 50406;
  cmWhatever             = 50407;
  cmOpen                 = 50408;
  cmWriteMsg             = 50409;
  cmSearch               = 50410;
  cmSetup                = 50411;
  cmManual               = 50412;
  cmPersonal             = 50413;
  cmOwnerNext            = 50414;
  cmOwnerPrev            = 50415;
  cmManualEntry          = 50416;
  cmShowMsg              = 50417;
  cmRead                 = 50418;
  cmDelete               = 50419;
  cmRefreshReplies       = 50420;
  cmReplyNext            = 50421;
  cmReplyPrev            = 50422;
  cmAreYouReplier        = 50423;
  cmNewPacket            = 50424;
  cmDosShell             = 50425;
  cmPrintSetup           = 50426;
  cmEditRep              = 50427;
  cmOwnerDel             = 50428;
  cmNetmailReply         = 50429;
  cmUpdatePacketList     = 50430;
  cmUnread               = 50431;
  cmCloseActivePacket    = 50432;
  cmAreYouPers           = 50433;
  cmAreYouUnread         = 50434;
  cmAdoptTagline         = 50435;
  cmEditHeader           = 50436;
  cmArchiveReplies       = 50437;
  cmAddressBook          = 50438;
  cmTaglines             = 50439;
  cmAdd                  = 50440;
  cmEdit                 = 50441;
  cmForgottenRealms      = 50442;
  cmAreYouForgotten      = 50443;
  cmColors               = 50444;
  cmAreYouMsgList        = 50445;
  cmAddressBookChanged   = 50446;
  cmAreYouAddrBook       = 50447;
  cmAddressLookup        = 50448;
  cmActivateFull         = 50450;
  cmActivateEmpty        = 50451;
  cmActivateNoDL         = 50452;
  cmMarkMsg              = 50453;
  cmUUDecode             = 50454;
  cmWillRead             = 50455;
  cmWillReply            = 50456;
  cmRefresh              = 50457;
  cmNetmailFlags         = 50458;
  cmReplyOriginal        = 50459;
  cmMagni                = 50460;
  cmInterest             = 50461;

  cmCDRewind             = 50462;
  cmCDStop               = 50463;
  cmCDPlay               = 50464;
  cmCDPause              = 50465;
  cmCDForward            = 50466;
  cmCDEject              = 50467;

  cmCrosspost            = 50468;
  cmReplyReadMode        = 50469;
  cmEditFile             = 50470;
  cmArchivers            = 50471; {cms}
  cmColorCategoryChanged = 50472;
  cmUnreadTotalChanged   = 50473;
  cmAddRequest           = 50474;
  cmFilterLookup         = 50475;
  cmDelRequest           = 50476;
  cmAddrLookup           = 50477;
  cmShowReplies          = 50478;

  idCDRewind           = 305;
  idCDStop             = 301;
  idCDPlay             = 300;
  idCDPause            = 302;
  idCDForward          = 304;
  idCDEject            = 303;

  cmMenuBase       = 65000;

  xfFull        = 0;
  xfHideOutput  = 1;
  xfDirect      = 2;
  xfPause       = 4;

  grp_AreaFull  = 1;
  grp_AreaEmpty = 2;

  PersXSize = 100; {percbar sizes}
  PersYSize = 9;

{  Persia : PSimplePerc = NIL; {alexander the great}

  defaultSetupFlags = ufFallenAngel or
                      ufScrSaver or
                      ufReplySafe or
                      ufUnreadReminder;

  setup : TSetupRec = (DLDir:'';
                       ULDir:'';
                       EditCmd:'edit';
                       FilterCmd:'';
                       QuoteStr:'';
                       PassWord:'';
                       Flags:defaultSetupFlags;
                       Behaviour:tbAsk;
                       MsgWindow : (A:(X:0;Y:0);B:(X:0;Y:0));
                       ReaderFont: fntFixedNorm;
                       ReaderFontH: 8;
                       ColorScheme : '';
                       RClock      : (A:(X:0;Y:0);B:(X:0;Y:0));
                       RCDPlayer   : (A:(X:0;Y:0);B:(X:0;Y:0))
                       {RPerc       : (A:(X:0;Y:0);B:(X:0;Y:0))}

                       );

type

  TReaderColors = array[1..maxreadercolors] of TReaderColor;

var

  readerColors : TReaderColors;

function CompareDate(u1,u2:DateTime):integer;
procedure InitReaderColors;

implementation

uses WLan,XLan,XDebug,XIO,XBuf;

procedure InitReaderColors;
const
  colors:array[1..maxreadercolors] of byte =
    (cLightGray,cBlack,cMagenta,cCyan,cBlue,cGreen);
var
  l:longint;
begin
  for l:=1 to maxreadercolors do with ReaderColors[l] do begin
    Color := colors[l];
    Name  := gtid(msReaderColors1+(l-1));
  end;
end;

function CompareDate(u1,u2:DateTime):integer;
begin
  if u1.Year > u2.Year then CompareDate := 1 else
  if u1.Year < u2.Year then CompareDate := -1 else
  if u1.Month > u2.Month then CompareDate := 1 else
  if u1.Month < u2.Month then CompareDate := -1 else
  if u1.Day > u2.Day then CompareDate := 1 else
  if u1.Day < u2.Day then CompareDate := -1 else
  if u1.Hour > u2.Hour then CompareDate := 1 else
  if u1.Hour < u2.Hour then CompareDate := -1 else
  if u1.Min > u2.Min then CompareDate := 1 else
  if u1.Min < u2.Min then CompareDate := -1 else CompareDate := 0;
end;

{- TPersonColl -}

function TPersonColl.Compare(k1,k2:pointer):integer;
begin
  if PPerson(k1)^.Name > PPerson(k2)^.Name then Compare := 1 else
  if PPerson(k2)^.Name > PPerson(k1)^.Name then Compare := -1 else
  Compare := 0;
end;

procedure TPersonColl.FreeItem(item:pointer);
begin
  Dispose(PPerson(item));
end;

{- TBBSInfoColl -}

function TBBSInfoColl.Compare(k1,k2:pointer):integer;
begin
  if PBBSInfo(k1)^.BBSName < PBBSInfo(k2)^.BBSName then Compare := -1 else Compare := 1;
end;

procedure TBBSInfoColl.FreeItem(item:pointer);
begin
  Dispose(PBBSInfo(item));
end;

{- TPacketColl -}

function TPacketColl.Compare;
var
  t1,t2:longint;
  u1,u2:DateTime;
  i:integer;
begin
  t1 := PPacket(k1)^.Time;
  t2 := PPacket(k2)^.Time;
  UnPackTime(t1,u1);
  UnPackTime(t2,u2);
  i := CompareDate(u1,u2);
  if i = 0 then i := -1 else i := -i;
  Compare := i;
end;

procedure TPacketColl.FreeItem;
begin
  Dispose(PPacket(item));
end;

{- TAreaColl -}

procedure TAreaColl.FreeItem;
begin
  Dispose(PArea(item));
end;

function TAreaColl.Compare;
var
  p1,p2:PArea;
  wha:integer;
  function lev(p:PArea):integer;
  begin
    case P^.AreaType of
      watEcho : lev := 2;
      watNetmail : lev := 3;
      watLocal : lev := 1;
      watEMail : lev := 4;
      watInfo  : lev := 5;
    end; {Case}
  end;
begin
  if setup.Flags and ufNoAreaSort > 0 then begin
    Compare := -1;
    exit;
  end;
  p1 := k1;
  p2 := k2;
  if lev(p1) > lev(p2) then Compare := 1 else
    if lev(p1) < lev(p2) then Compare := -1 else begin
    if p1^.name > p2^.Name then Compare := 1 else
      if p1^.Name < p2^.Name then Compare := -1 else Compare := 0;
  end;
end;

{- TMsgPacket -}

constructor TMsgPacket.Init;
begin
  inherited Init;
  XMakeDirStr(awhere,true);
  Where := awhere;
  Packetid := apacketid;
  New(areaList,Init(20,40));
  New(msgList,Init(40,40,SizeOf(TMsg)));
end;

destructor TMsgPacket.Done;
begin
  Dispose(msgList,Done);
  Dispose(areaList,Done);
  inherited Done;
end;

function TMsgPacket.FindSpecificItems(func:pointer; internal:boolean):PDumbCollection;
var
  ftilist:PDumbCollection;
  Pm:PMsg;
  n:integer;
  b:boolean;
begin
  New(ftilist,Init(20,40));
  FindSpecificItems := ftilist;
  if msgList^.Count = 0 then exit;
  for n:=0 to msgList^.Count-1 do begin
    Pm := msgList^.At(n);
    if Pm^.Area^.AreaType <> watInfo then begin
      asm
        les  di,Pm
        push es
        push di
{        cmp  internal,0
        je   @skip}
{        mov  ax,[bp]
        push ax}
      @skip:
        call dword ptr func
        mov  b,al
      end;
      if b then ftiList^.Insert(Pm);
    end;
  end;
  FindSpecificItems := ftiList;
end;

function TMsgPacket.GetSpecificCount(func:pointer):longint;
var
  Pm : PMsg;
  count:longint;
  n:integer;
  b:boolean;
begin
  count := 0;
  for n:=0 to msgList^.Count-1 do begin
    Pm := msgList^.At(n);
    if Pm^.Area^.AreaType <> watInfo then begin
      asm
        les  di,Pm
        push es
        push di
        call dword ptr func
        mov  b,al
      end;
      if b then inc(count);
    end;
  end;
  GetSpecificCount := count;
end;

function TMsgPacket.Load;
begin
end;

function TMsgPacket.ReadMsgText(amsg:PMsg):PMsgText;
begin
end;

procedure TMsgPacket.PostMsg;
begin
end;

procedure TMsgPacket.MsgToFile;
begin
end;

procedure TMsgPacket.FileToMsg;
begin
end;

procedure TMsgPacket.DeleteMsg;
begin
end;

procedure TMsgPacket.UpdateMsgFlags;
begin
end;

procedure TMsgPacket.AddInfo;
var
  Pm:PMsg;
  procedure CreateInfoArea;
  begin
    New(InfoArea);
    ClearBuf(InfoArea^,SizeOf(InfoArea^));
    with InfoArea^ do begin
      Name     := appName+' - '+gtid(msNewsAreaName);
      Number   := 65535;
      AreaType := watInfo;
      UserStat := wusAll;
      EchoTag  := '65535';
    end;
    arealist^.Insert(InfoArea);
  end;

begin
  if infoArea = NIL then CreateInfoArea;
  New(Pm);
  ClearBuf(Pm^,SizeOf(Pm^));
  with Pm^ do begin
    From := user.SysOp;
    Too  := 'All';
    Subj := sbj;
    Date := XGetFileDate(Self.Where+fn);
    Area := infoArea;
    Flags := wmfFileLink;
    Filename := fn;
  end;
  Debug('importing news: '+sbj);
  msglist^.Insert(Pm);
  inc(infoArea^.Total);
  inc(infoArea^.Unread);
end;

function TMsgPacket.GetType;
begin
  GetType := pakUnknown;
end;

procedure TMsgPacket.OLC;
begin
end;

procedure TMsgPacket.WriteMsgHeader;
begin
end;

procedure TMsgPacket.Clear;
begin
  areaList^.FreeAll;
  msgList^.FreeAll;
end;

procedure TMsgPacket.CacheTo;
begin
end;

function TMsgPacket.GetPacketid;
var
  dirinfo:searchrec;
  b:byte;
begin
  FindFirst(filespec,ReadOnly+Archive,dirinfo);
  if DosError = 0 then begin
    b := pos('.',dirinfo.name);
    if b = 0 then GetPacketid := dirinfo.name
             else GetPacketid := copy(dirinfo.name,1,b-1);
  end else GetPacketid := '';
end;

{ TTaglineCollection }
function TTaglineCollection.Compare;
begin
  if setup.Flags and ufNoTaglineSort = 0 then Compare := inherited Compare(k1,k2)
                                         else Compare := -1;
end;

end.