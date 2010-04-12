{
Wolverine BlueWave packet extensions

updates:
--------
20st Jun 96 - 23:46 - Made code more fault-tolerant...
 7th Aug 96 - 23:51 - adapted to WHAM...
21st Aug 96 - 01:58 - fixes...
22nd Aug 96 - 17:34 - olc support is perfect...
27th Aug 96 - 13:43 - fixes...
10th Nov 96 - 11:27 - perfected olc...
}

{$C MOVEABLE DEMANDLOAD DISCARDABLE}
{$O+}

unit WBlue;

interface

uses Objects,WTypes;

const

  infCanCrash  = 1; {netmail flags}
  infCanAttach = 2;
  infCanKSent  = 4;
  infCanHold   = 8;
  infCanImm    = 16;
  infCanFReq   = 32;
  infCanDirect = 64;

  iafScanning  = 1;     {user reads this area}
  iafAlias     = 2;     {aliases allowed}
  iafAnyName   = 4;     {allow any name to be entered}
  iafEcho      = 8;     {if set hitnet area, else local area}
  iafNetmail   = 16;    {Netmail area}
  iafPost      = 32;    {User can post}
  iafNoPrivate = 64;    {No private messages can be entered}
  iafNoPublic  = 128;   {No public messages allowed}
  iafNoTagline = 256;   {No taglines allowed}
  iaf7bit      = 512;   {No high bits}
  iafNoEcho    = 1024;  {user can prevent messages go to net - NIMP}
  iafHasFile   = 2048;  {file attach operations welcome}
  iafPersOnly  = 4096;  {user downloads only personal msgs in this area}
  iafPAll      = 8192;  {user downloads P+All messages in this area}

  iufHotKeys    = 1;
  iufXPert      = 2;
  iufGraphics   = 8;  {ansi color in door}
  iufNotMyMail  = 16; {do not bundle msg from user}
  iufExtInfo    = 32; {download extended info (v3)}
  iufNumericExt = 64;

  icfNoOLC      = 1;  {no offline config support (v3)}
  icfNoFREQ     = 2;  {no file request (v3)}

  iflNone       = 0;  {no new files list}
  iflText       = 1;  {plain text}
  iflANSI       = 2;  {ansi}

  ptDoor       = 1;
  ptReader     = 2;

  intFidonet   = 1;  {network types}
  intQwkNet    = 2;
  intInternet  = 3;

  ftiPrivate   = 1;
  ftiCrash     = 2;
  ftiRead      = 4;
  ftiSent      = 8;
  ftiFile      = 16;
  ftiForward   = 32;
  ftiOrphan    = 64;
  ftiKill      = 128;
  ftiLocal     = 256;
  ftiHold      = 512;
  ftiImmediate = 1024;
  ftiFReq      = 2048;
  ftiDirect    = 4096;
  ftiUReq      = 32768;

  xtfHasRead    = 1;
  xtfHasReplied = 2;
  xtfIsPersonal = 4;
  xtfIsTagged   = 8;
  xtfHasSaved   = 16;
  xtfHasPrinted = 32;

  xtmSave       = 1;
  xtmReply      = 2;
  xtmPrint      = 4;
  xtmDelete     = 8;
  xtmReRead     = xtmDelete; {wolverine specific}

  umfInactive   = 1;
  umfPrivate    = 2;
  umfNoEcho     = 4;
  umfHasFile    = 8;
  umfNetmail    = 16;

  unfCrash      = 2;
  unfFile       = 16; {file attach}
  unfKill       = 128;
  unfLocal      = 256;
  unfHold       = 512;
  unfImmediate  = 1024;
  unfFReq       = 2048;
  unfDirect     = 4096;
  unfUReq       = 32768;

  pdqHotkeys    = 1;
  pdqXPert      = 2;
  pdqAreaChange = 4;
  pdq8bit       = 8;
  pdqNotMyMail  = 16;

  tsmfNetmail   = 1;
  tsmfCrash     = 2;
  tsmfRead      = 4;
  tsmfFile      = 8;
  tsmfHidden    = 16;

  upfWasQWK      = 1;
  upfUPIRequired = 2;

type

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
    CtrlFlags    : word;
    SystemName   : array[1..65] of char;
    MaxFReqs     : byte;
    isQWK        : word;
    Unused2      : array[1..4] of byte;
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
    FileListType : byte;
    AutoMacro    : array[1..3] of boolean;
    MaxPakSize   : word;
    Reserved     : array[1..228] of byte;
  end;

  TOLCInfo = record
    UFlags       : word;
    FileListType : byte;
    MaxPakSize   : word;
  end;

  TInfRec = record
    AreaNum : array[1..6] of char;
    echoTag : array[1..21] of char;
    Title   : array[1..50] of char;
    Flags   : word;
    NetType : byte;
  end;

  TMixRec = record
    Areanum   : array[1..6] of char;
    Total     : word;
    Pers      : word;
    FTIStart  : longint;
  end;

  TFTIRec = record
    mFrom     : array[1..36] of char;
    mTo       : array[1..36] of char;
    Subject   : array[1..72] of char;
    Date      : array[1..20] of char;
    MsgNum    : word;
    ReplyTo   : word;
    ReplyAt   : word;
    Where     : longint;
    MsgLength : longint;
    Flags     : word;
    Orig_Zone : word;
    Orig_Net  : word;
    Orig_Node : word;
  end;

  TUPLHeader = record
    RegNum     : array[1..10] of char;
    Version    : array[1..20] of char; {encoded}
    MajVer     : byte;
    MinVer     : byte;
    RdrName    : array[1..80] of char;
    HdrLen     : word;
    RecLen     : word;
    LoginName  : array[1..44] of char;
    AliasName  : array[1..44] of char;
    TearLine   : array[1..16] of char;
    Compress   : byte;
    Flags      : byte;
    NR         : boolean;
    Pad        : array[1..33] of byte; {zeroed}
  end;

  TUPLRec = record
    mFrom     : array[1..36] of char;
    mTo       : array[1..36] of char;
    Subject   : array[1..72] of char;
    dZone,dNet,dNode,dPoint:word;
    MsgAttr   : word;
    NetAttr   : word;
    Date      : longint;
    ReplyTo   : longint;
    Filename  : array[1..13] of char;
    EchoTag   : array[1..21] of char;
    AreaFlags : word;
    Attach    : array[1..13] of char;
    UserArea  : array[1..6] of byte;
    NetType   : byte;
    Dest      : array[1..100] of char;
  end;

  TREQRec = record
    Filename : array[1..13] of char;
  end;

  TPDQHeader = record
    Keywords : array[1..10] of array[1..21] of char;
    Filters  : array[1..10] of array[1..21] of char;
    Macros   : array[1..3] of array[1..78] of char;
    Password : array[1..21] of char;
    PassType : byte;
    Flags    : word;
  end;

  TPDQRec = record
    echoTag : array[1..21] of char;
  end;

  PBWPacket = ^TBWPacket;
  TBWPacket = object(TMsgPacket)
    OLCInfo : TOLCInfo;
    constructor Init(awhere,apacketid:FnamEStr);
    function   Load:boolean;virtual;
    function   ReadMsgText(amsg:PMsg):PMsgText;virtual;
    function   GetType:TPakType;virtual;
    procedure  UpdateMsgFlags(amsg:PMsg);virtual;
    procedure  ReadMsgFlags(amsg:Pmsg);virtual;
    procedure  CacheTo(towhere:FnameStr);virtual;
  end;

  PBWReplyPacket = ^TBWReplyPacket;
  TBWReplyPacket = object(TMsgPacket)
    constructor Init(awhere,apacketid:FnamEStr);
    function   Load:boolean;virtual;
    function   ReadMsgText(amsg:PMsg):PMsgText;virtual;
    function   GetType:TPakType;virtual;
    procedure  PostMsg(amsg:PMsg; afile:FnameStr);virtual;
    procedure  OLC(aarealist:PAreaColl);virtual;
    procedure  WriteMsgHeader(amsg:Pmsg; anew:boolean);virtual;
    procedure  MsgToFile(amsg:Pmsg; afile:FnameStr);virtual;
    procedure  FileToMsg(afile:FnameStr; amsg:PMsg);virtual;
    procedure  DeleteMsg(amsg:Pmsg);virtual;
  end;

procedure BWEncode(var buf; count:word);
procedure BWDecode(var buf; count:word);

implementation

uses

  XTypes,XStream,WProcs,XDebug,XUnix,Debris,Dos,Strings,XStr,XIO,XGfx,
  XBuf;

const

  newsFile : string[12] = 'NewFiles.BW';

function StrBWDateToUnix(s:FnameStr):longint;
var
  T:DateTime;
  l:longint;
  b:byte;
begin
  T.Day  := s2l(copy(s,1,2));
  T.year := (s2l(copy(s,8,2)))+1900;
  T.Hour := s2l(copy(s,12,2));
  T.Min  := s2l(copy(s,15,2));
  s := lower(copy(s,4,3));
  b := 0;
  for b:=1 to 12 do if s = ESNamesOfMonths[b] then T.Month := b;
  PackTime(t,l);
  StrBWDateToUnix := l;
end;

function XtiFti2Msg(var xti:TXTIRec; var fti:TFTIRec; aftinum:longint):PMsg;
var
  Pm:PMsg;
begin
  New(Pm);
  with Pm^ do begin
    From   := StrPas(@fti.mFrom);
    Too    := StrPas(@fti.mTo);
    if (upper(Too) = upper(user.Name)) or (upper(Too) = upper(user.Alias)) then xti.Flags := xti.Flags or xtfIsPersonal;
    Subj   := StrPas(@fti.Subject);
    Strip(From);
    Strip(Too);
    Strip(Subj);
    Translate(Subj,WinTransDest,TrTransDest);
    Date   := StrBWDateToUnix(StrPas(@fti.Date));
    Where  := fti.Where;
    Length := fti.MsgLength;
    Flags  := (byte(xti.Flags and xtfHasRead > 0)*wmfRead) or
              (byte(xti.Flags and xtfHasReplied > 0)*wmfReplied) or
              (byte(xti.Flags and xtfIsPersonal > 0)*wmfPersonal);
    Marks  := xti.Marks;
    FTINum := aftinum;
    FillChar(Addr,SizeOf(Addr),0);
    Move(fti.Orig_Zone,Addr,6);
  end;
  XtiFti2Msg := Pm;
end;

const

  MAXFACTOR = 10;

procedure BWEncode(var buf; count:word);assembler;
asm
  cld
  les  di,buf
  mov  cx,count
@loop:
  mov  al,es:[di]
  or   al,al
  je   @contiyugard
  add  al,MAXFACTOR
  mov  es:[di],al
@contiyugard:
  inc  di
  loop @loop
end;

{for n=1 to buffer_size do buffer[n] = buffer[n] + MAXFACTOR}

procedure BWDecode(var buf; count:word);assembler;
asm
  cld
  les  di,buf
  mov  cx,count
@loop:
  mov  al,es:[di]
  or   al,al
  je   @contiyugard
  sub  al,MAXFACTOR
  mov  es:[di],al
@contiyugard:
  inc  di
  loop @loop
end;

{for n=1 to buffer_size do buffer[n] = buffer[n] - MAXFACTOR}

procedure FilterBWText(P:PMsgText);
var
  tempBuf:PChar;
  tempbufSize:word;
  Pt:PMsgText;
  w,w1:word;
  c:char;
begin
  Pt := P;
  while Pt <> NIL do begin
    tempbufSize := Pt^.Size-GetByteCount(Pt^.Data^,Pt^.Size,10);
    if tempBufSize <> Pt^.Size then begin
      GetMem(tempBuf,tempBufSize);
      w1 := 0;
      for w:=0 to Pt^.Size-1 do begin
        c := Pt^.Data[w];
        if c <> #10 then begin
          tempBuf[w1] := c;
          inc(w1);
        end;
      end;
      FreeMem(Pt^.Data,Pt^.Size);
      Pt^.Size := tempBufSize;
      Pt^.Data := tempBuf;
    end;
    Pt := Pt^.Next;
  end;
end;

{- TBWPacket -}

constructor TBWPacket.Init;
begin
  inherited Init(aWhere,apacketid);
  Config := mpcInfo or mpcCanCache;
  if packetid = '' then Packetid := GetPacketid(Where+'*.INF');
end;

function TBWPacket.Load;
var
  mT,mX,M,T:TBufStream;
  h:TInfHeader;
  rec:TInfRec;
  mix:TMixRec;
  xti:TXTIRec;
  dirinfo:SearchRec;
  s:string;
  ftioffs:longint;
  desiredarea:word;
  desiredstat:byte;
  P:PArea;
  b:byte;
  xtiok:boolean;
  realpacket:boolean;
  function LoadArea(aarea:PArea):boolean;
  var
    fti:TFTIRec;
    Pm:PMsg;
    ftinum:longint;
    subtotal:longint;
  begin
    LoadArea := false;
    ftinum := FTIOffs div SizeOf(TFTIRec);
    subtotal := 0;
    mT.Seek(FTIOffs);
    mX.Seek(ftinum*SizeOf(TXTIRec));
    while subtotal < aarea^.Total do begin
      mT.Read(fti,SizeOf(fti));
      if mT.Status <> stOK then exit;
      if xtiOK then mX.Read(xti,SizeOf(xti)) else ClearBuf(xti,SizeOf(xti));
      Pm := XtiFti2Msg(xti,fti,ftinum);
      if not xtiOK then mX.Write(xti,SizeOf(xti));
      Pm^.Area := aarea;
      Cycle;
      MsgList^.Insert(Pm);
      inc(ftinum);
      inc(subtotal);
      if Pm^.Flags and wmfRead = 0 then inc(aarea^.Unread);
    end;
    LoadArea := true;
  end;

  procedure GetAreaTotal(var arec:TMIXRec);
  begin
    ClearBuf(arec,SizeOf(arec));
    M.Seek(0);
    while M.GetPos < M.GetSize do begin
      M.Read(arec,SizeOf(arec));
      if bufcmp(arec.areanum,rec.areanum,5) then exit;
    end;
  end;

  procedure CloseAllStreams;
  begin
    M.Done;
    T.Done;
    mX.Done;
    if realPacket then mT.Done;
  end;

  function GetIt(P:PChar):string;
  var
    s:string;
  begin
    s := StrPas(P);
    Strip(s);
    GetIt := s;
  end;

begin
  Load := false;
  T.Init(Where+Packetid+'.INF',stOpenRead,8192);
  if T.Status <> stOK then begin
    Debug('stream open error');
    exit;
  end;
  T.Read(h,SizeOf(h));
  with user do begin
    Name  := GetIt(@h.LoginName);
    Alias := GetIt(@h.AliasName);
    SysOp := GetIt(@h.SysOp);
    BBS   := GetIt(@h.SystemName);
    NetFlags := h.NetmailFlags;
  end;
  with OLCInfo do begin
    UFlags := h.UFlags;
    FileListType := h.FileListType;
    MaxPakSize   := h.MaxPakSize;
  end;
  Debug(user.BBS+' loading');

  M.Init(Where+Packetid+'.MIX',stOpenRead,8192);
  realPacket := M.Status = stOK;
  if realPacket then begin
    mT.Init(Where+packetid+'.FTI',stOpenRead,8192);
    if mT.Status <> stOK then begin
      M.Done;
      mT.Done;
      T.Done;
      Debug('mT init error');
      exit;
    end;
  end;
  xtiok := true;
  mX.Init(Where+Packetid+'.XTI',stOpenRead,8192);
  if mX.Status <> stOK then begin
    mX.Done;
    mX.Init(Where+Packetid+'.XTI',stCreate,8192);
    if mX.Status <> stOK then begin
      CloseAllStreams;
      Debug('mX create error');
      exit;
    end;
    ClearBuf(xti,SizeOf(xti));
    xtiok := false;
  end;

  while T.GetPos < T.GetSize do begin
    T.Read(rec,SizeOf(rec));
    if T.Status <> stOK then begin
      CloseAllStreams;
      Debug('stream read error');
      exit;
    end;
    New(P);
    Cycle;
    ClearBuf(P^,SizeOf(P^));
    with P^ do begin
      Number  := s2l(StrPas(@rec.areanum)); {!!!}
      EchoTag := StrPas(@rec.echotag);
      Name    := StrPas(@rec.Title);
      Flags   := 0;
      Unread  := 0;
      if rec.Flags and iafAnyname > 0 then Flags := Flags or wafAlias;
      if rec.Flags and iafPost > 0 then Flags := Flags or wafPost;

      if rec.Flags and iafScanning > 0 then begin
        if rec.Flags and iafPersOnly > 0 then UserStat := wusPers else
        if rec.Flags and iafPAll > 0 then UserStat := wusPAll else UserStat := wusAll;
      end else UserStat := wusNone;

      if rec.NetType = intInternet then AreaType := watEMail else
      if rec.Flags and iafNetmail > 0 then AreaType := watNetmail else
      if rec.Flags and iafEcho > 0 then AreaType := watEcho else AreaType := watLocal;
      if UserStat <> wusNone then begin
        GetAreaTotal(mix);
        Total   := mix.Total;
        Pers    := mix.Pers;
        FTIOffs := mix.FTIStart;
      end;
    end;
    AreaList^.Insert(P);
    if realPacket and (P^.UserStat <> wusNone) then if not LoadArea(P) then begin
      Debug('loadarea fail');
      CloseAllStreams;
      exit;
    end;
  end;
  CloseAllStreams;
  infoArea := NIL;
  for b:=1 to 5 do begin
    s := StrPas(@h.ReaderFiles[b]);
    if s <> '' then if XFileExists(Where+s) then AddInfo(s,'Haberler ('+s+')');
  end;
  if XFileExists(Where+newsFile) then AddInfo(newsFile,user.BBS+' - yeni dosyalar');
  Debug('Load successful');
  Load := true;
end;

function TBWPacket.ReadMsgText(amsg:PMsg):PMsgText;
var
  T:TDosStream;
  Pt:PMsgText;
begin
  Pt := NIL;
  if amsg^.Flags and wmfFileLink = 0 then begin
    T.Init(Where+Packetid+'.DAT',stOpenRead);
    T.Seek(amsg^.Where);
    if T.Status = stOK then begin
      Pt := stream2Text(T,amsg^.Length);
      if Pt <> NIL then FilterBWText(Pt);
    end else Debug('nofilelink: stream error');
  end else begin
    T.Init(Where+amsg^.Filename,stOPenRead);
    if T.Status = stOK then begin
      Pt := stream2Text(T,T.GetSize);
      if Pt <> NIL then FilterBWText(Pt);
    end else Debug('filelink: stream error');
  end;
  T.Done;
  ReadMsgText := Pt;
end;

var
  desiredArea:longint;

function TBWReplyPacket.Load;
var
  T:TDosStream;
  rec:TUPLRec;
  P:PMsg;
  D:DateTime;
  EchoTag : string[20];
  function Test(aarea:PArea):boolean;far;
  begin
    Test := aarea^.EchoTag = EchoTag;
  end;

  procedure ClearArea(par:PArea);far;
  begin
    par^.UserStat := wusNone;
  end;

  function SearchArea(par:Parea):boolean;far;
  begin
    SearchArea := par^.Number = desiredarea;
  end;

  procedure LookOLC;
  var
    Tex:TDosStream;
    P:PArea;
    s:string;
    b:byte;
    desiredstat:byte;
  begin
    Tex.Init(Where+Packetid+'.OLC',stOpenRead);
    if Tex.Status = stOK then begin
      Debug('utilizing OLC');
      Packet^.areaList^.ForEach(@ClearArea);
      while Tex.GetPos < Tex.GetSize do begin
        SReadln(Tex,s);
        Strip(s);
        if s[1] = '[' then begin
          s := copy(s,2,length(s)-2);
          desiredarea := s2l(s);
          if desiredarea > 0 then while true do begin
            SReadln(Tex,s);
            Strip(s);
            FastUpper(s);
            if pos('SCAN',s) > 0 then begin
              b := pos('=',s);
              if b > 0 then begin
                s := copy(s,b+1,255);
                Strip(s);
                if s = 'PERSONLY' then desiredstat := wusPers else
                if s = 'PERS+ALL' then desiredstat := wusPAll else
                if s = 'ALL' then desiredstat := wusAll else desiredstat := wusNone;
                P := Packet^.areaList^.FirstThat(@SearchArea);
                if P <> NIL then P^.UserStat := desiredstat;
                break;
              end; {if}
            end; {if * while true}
          end; {if}
        end; {if}
      end; {while}
    end; {if}
    Tex.Done;
  end;

begin
  Load := false;
  T.Init(Where+packetid+'.UPL',stOpenRead);
  if T.Status <> stOK then begin
    T.Done;
    exit;
  end;
  Debug('loading replies');
  T.Seek(SizeOf(TUPLHeader));
  while T.GetPos < T.GetSize do begin
    T.Read(rec,SizeOf(rec));
    if T.Status <> stOK then begin
      Debug('read error');
      break;
    end;
    Cycle;
    New(P);
    with P^ do begin
      From := rec.mFrom;
      Too  := rec.mTo;
      Subj := rec.Subject;
      Strip(From);
      Strip(Too);
      Strip(Subj);
      Addr.Zone := rec.dZone;
      Addr.Net  := rec.dNet;
      Addr.Node := rec.dNode;
      Addr.Point := rec.dPoint;
      Unix2DOS(rec.Date,D);
      PackTime(D,Date);
      Filename := StrPas(@rec.Filename);
      EchoTag := StrPas(@rec.EchoTag);
      Area := Packet^.areaList^.FirstThat(@Test);
      if Area = NIL then begin
        Area := Packet^.areaList^.At(0);
        Debug('WARNING: '+EchoTag+' matched to '+Area^.EchoTag);
      end;
    end;
    msgList^.Insert(P);
  end;
  T.Done;
  LookOLC;
  Debug(l2s(msglist^.Count)+' replies read');
  Load := true;
end;

procedure TBWReplyPacket.PostMsg(amsg:PMsg; afile:FnameStr);
var
  I,O:TDosStream;
  fn:FnameStr;
begin
  Debug('PostMsg: copying streams');
  I.Init(afile,stOpenRead);
  fn := XGetUniqueName(Where,'.MSG');
  O.Init(Where+fn,stCreate);
  CopyStream(I,O,I.getSize);
  I.Done;
  O.Done;
  Debug('PostMsg: writing header');
  amsg^.Filename := fn;
  WriteMsgHeader(amsg,true);
end;

procedure GetUPLHeader(var h:TUPLHeader);
var
  ver:FnameStr;
begin
  Debug('preparing UPL header');
  ClearBuf(h,SizeOf(h));
  h.MajVer := 0;
  h.MinVer := 3;
  StrPCopy(@h.RdrName,appName);
  ver := rVersion;
  if regged then ver := ver + '.';
  StrPCopy(@h.Version,ver);
  BWDecode(h.Version,20);
  h.HdrLen := SizeOf(TUPLHeader);
  h.RecLen := SizeOf(TUPLRec);
  StrPCopy(@h.LoginName,user.Name);
  StrPCopy(@h.AliasName,user.Alias);
  StrPCopy(@h.Tearline,appName+' ');
end;

procedure TBWReplyPacket.WriteMsgHeader;
var
  T:TDosStream;
  h:TUPLHeader;
  rec:TUPLRec;
  l:longint;
  tempDate:DateTime;
begin
  Debug('writing '+GetBool(anew,'new','old')+' msg header');
  if not XFileExists(Where+packetid+'.UPL') then begin
    if not anew then Abort('TBWReplyPacket.WriteMsgHeader','Inconsistency failure');
    Debug('creating: '+Where+Packetid+'.UPL');
    T.Init(Where+packetid+'.UPL',stCreate);
    if T.Status <> stOK then begin
      Debug('create error');
      T.Done;
      exit;
    end;
  end else T.init(Where+packetid+'.UPL',stOpen);
  GetUPLHeader(h);
  T.Write(h,SizeOf(h));
  if anew then T.Seek(T.GetSize) else begin
    Debug('seeking for old header');
    while T.GetPos < T.GetSize do begin
      T.Read(rec,SizeOf(rec));
      if StrPas(@rec.Filename) = amsg^.Filename then begin
        T.Seek(T.GetPos-SizeOf(rec));
        break;
      end;
    end;
  end;
  ClearBuf(rec,SizeOf(rec));
  StrPCopy(@rec.mFrom,amsg^.From);
  StrPCopy(@rec.mTo,amsg^.Too);
  StrPCopy(@rec.Subject,amsg^.Subj);
  StrPCopy(@rec.EchoTag,amsg^.area^.EchoTag);
  StrPCopy(@rec.Filename,amsg^.Filename);
  rec.AreaFlags := amsg^.area^.Flags;
  rec.NetAttr   := amsg^.Netflags;
  LongRec(l).Lo := GetSysTime;
  LongRec(l).Hi := GetSysDate;
  UnPackTime(l,tempDate);
  rec.Date := Dos2Unix(tempDate);
  with amsg^.Addr do begin
    rec.dZone := Zone;
    rec.dNet  := Net;
    rec.dNode := Node;
    rec.dPoint := Point;
  end;
  if amsg^.area^.AreaType = watNetmail then rec.MsgAttr := umfNetmail + umfPrivate
                                       else rec.MsgAttr := 0;
  T.Write(rec,SizeOf(rec));
  T.Done;
  Debug('msg header written');
end;

function TBWReplyPacket.ReadMsgText;
var
  T:TDosStream;
  Pt:PMsgText;
begin
  T.Init(Where+amsg^.Filename,stOpenRead);
  Pt := Stream2Text(T,T.GetSize);
  T.Done;
  if Pt <> NIL then FilterBWText(Pt);
  ReadMsgText := Pt;
end;

procedure TBWReplyPacket.MsgToFile;
begin
  CopyFile(Where+amsg^.Filename,afile);
end;

procedure TBWReplyPacket.FileToMsg;
begin
  CopyFile(afile,Where+amsg^.Filename);
end;

procedure TBWReplyPacket.DeleteMsg;
var
  rec:TUPLRec;
  h:TUPLHeader;
  I,O:TDosStream;
begin
  I.Init(Where+packetid+'.UPL',stOpenRead);
  O.init(Where+'hebe.tmp',stCreate);
  I.Read(h,SizeOf(h));
  O.write(h,SizeOf(h));
  while I.GetPos < I.GetSize do begin
    I.Read(rec,SizeOf(rec));
    if StrPas(@rec.Filename) <> amsg^.Filename then O.Write(rec,SizeOf(rec));
  end;
  I.Done;
  O.Done;
  XDeleteAnyway(Where+amsg^.Filename);
  XRenameAnyway(Where+'hebe.tmp',Where+packetid+'.UPL');
end;

procedure TBWPacket.UpdateMsgFlags;
var
  T:TDosStream;
  fn:FnameStr;
  rec:TXTIRec;
begin
  fn := Where+packetid+'.XTI';
  T.Init(fn,stOpen);
  if T.Status <> stOK then begin
    T.Init(fn,stCreate);
    if T.Status <> stOK then begin
      T.Done;
      Debug('creation error');
      exit;
    end;
  end;
  T.Seek(amsg^.FTINum*SizeOf(TXTIRec));
  rec.Marks := amsg^.Marks;
  rec.Flags := amsg^.Flags;
  T.Write(rec,SizeOf(rec));
  T.Done;
end;

procedure TBWPacket.ReadMsgFlags;
var
  T:TDosStream;
  fn:FNameStr;
  rec:TXTIRec;
begin
  amsg^.Flags := 0;
  amsg^.Marks := 0;
  fn := Where+packetid+'.XTI';
  T.Init(fn,stOpenRead);
  if T.Status <> stOK then begin
    T.Done;
    Debug('XTI open error');
    exit;
  end;
  T.Seek(amsg^.FTINum*SizeOf(TXTIRec));
  T.Read(rec,SizeOf(rec));
  T.Done;
  amsg^.Flags := rec.Flags;
  amsg^.Marks := rec.Marks;
end;

procedure TBWReplyPacket.OLC;
const
  modeass:array[1..3] of string[8]=('PERSONLY','PERS+ALL','ALL');
var
  n:integer;
  P:PArea;
  T:TDosStream;
  procedure WriteHeader;
  type
    TOLCFlagRec = record
      Keyword : PChar;
      Flag    : word;
    end;
  const
    MaxOLCFlags = 6;
    OLCFlags : array[1..MaxOLCFlags] of TOLCFlagRec = (
      (Keyword:'MenuHotKeys';Flag:iufHotKeys),
      (Keyword:'ExpertMenus';Flag:iufXPert),
      (Keyword:'SkipUserMsgs';Flag:iufNotMyMail),
      (Keyword:'ExtendedInfo';Flag:iufExtInfo),
      (Keyword:'NumericExtensions';Flag:iufNumericExt),
      (Keyword:'DoorGraphics';Flag:iufGraphics));
  var
    oi:TOLCInfo;
    subloop:byte;
    procedure WriteOLCFlag(var aflagrec:TOLCFlagRec);
    begin
      SWriteln(T,StrPas(aflagrec.Keyword)+'='+GetBool(oi.UFlags and aflagrec.Flag > 0,'ON','OFF'));
    end;
    function getFileListStyle(astyle:byte):string;
    begin
      case astyle of
        iflText : getFileListStyle := 'TEXT';
        iflANSI : getFileListStyle := 'ANSI';
        else getFileListStyle := 'OFF';
      end; {case}
    end;
  begin
    SWriteln(T,'[Global Mail Host Configuration]');
    SWriteln(T,'AreaChanges=ON');
    oi := PBWPacket(Packet)^.OLCInfo;
    for subloop := 1 to MaxOLCFlags do WriteOLCFlag(OLCFlags[subloop]);
    SWriteln(T,'NewFileList='+getFileListStyle(oi.FileListType));
    SWriteln(T,'MaxPacketSize='+l2s(oi.MaxPakSize)+'K');
  end;
begin
  T.Init(Where+packetid+'.OLC',stCreate);
  if T.Status = stOK then begin
    writeHeader;
    for n:=0 to aarealist^.Count-1 do begin
      P := aarealist^.At(n);
      if (P^.AreaType in [watLocal,watEcho,watNetmail,watEmail]) and
         (P^.UserStat in [1..3]) then begin
        SWriteln(T,#13#10+'['+l2s(P^.Number)+']');
        SWriteln(T,'Scan = '+modeass[P^.UserStat]+#13#10);
      end;
    end;
  end else Debug('create error: '+Where+packetid+'.OLC');
  T.Done;
end;

constructor TBWReplyPacket.init;
begin
  inherited Init(awhere,apacketid);
  if Packetid = '' then Packetid := GetPacketid(Where+'*.UPL');
  Config := mpccanPost or mpcCanOLC or mpcCanDelete;
end;

procedure TBWPacket.CacheTo;
begin
  CopyFile(Where+Packetid+'.INF',towhere+Packetid+'.INF');
end;

function TBWPacket.GetType;
begin
  GetType := pakBW;
end;

function TBWReplyPacket.GetType;
begin
  GetType := pakBW;
end;

end.