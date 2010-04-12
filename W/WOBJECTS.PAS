{
Wolverine dialogs
}

unit WObjects;

interface

uses

Graph,XIO,XGfx,WHelp,XScroll,Drivers,WTypes,WProcs,GView,Tools,Objects,
XSetup,XCDPlay,WArch,Dos,XColl,XDebug,Debris,XTypes;

type

  PMsgHeader = ^TMsgHeader;
  TMsgHeader = object(TView)
    Font     : word;
    FontH    : word;
    constructor Init(var abounds:TRect; afont:word);
    procedure   Paint;virtual;
    procedure GetMsg(var info:THdrInfo);virtual;
  end;

  PReplyHeader = ^TReplyHeader;
  TReplyHeader = object(TMsgHeader)
    procedure GetMsg(var info:THdrInfo);virtual;
  end;

  PMsgStatus = ^TMsgStatus;
  TMsgStatus = object(TView)
    constructor Init(var abounds:TRect);
    procedure   Paint;virtual;
  end;

  PTextViewer = ^TTextViewer;
  TTextViewer = object(TView)
    Text      : PMsgText;
    ScrTop    : longint;
    ScrollBar : PScrollBar;
    EndY      : longint;
    Keyword   : PString;
    Keywhere  : longint;
    OrigY     : integer;
    constructor Init(var abounds:TRect; atext:PMsgText; ascrollbar:PScrollBar);
    destructor  Done;virtual;
    procedure   NewText(atext:PMsgText);
    procedure   Paint;virtual;
    procedure   UpdateScroller;
    procedure   HandleEvent(var Event:TEvent);virtual;
    procedure   ChangeBounds(var R:TRect);virtual;
    procedure   DoSearch(s:string);
    procedure   Go(where:integer);
{    procedure   GoSmooth(where:integer);}
  end;

  PMsgWindow = ^TMsgWindow;
  TMsgWindow = object(TWindow)
    Msg      : PMsg;
    Viewer   : PTextViewer;
    Status   : PMsgStatus;
    Text     : PMsgText;
    constructor Init(amsg:PMsg);
    destructor  Done;virtual;
    procedure   NewMsg(amsg:PMsg);
    procedure   MakeItRead;
    procedure   HandleEvent(var Event:Tevent);virtual;
    procedure   ChangeBounds(var R:TRect);virtual;
  end;

  PReplyWindow = ^TReplyWindow;
  TReplyWindow = object(TWindow)
    Reply      : PMsg;
    Viewer     : PTextViewer;
    Text       : PMsgText;
    constructor Init(areply:PMsg);
    procedure   NewMsg(amsg:PMsg);virtual;
    procedure   HandleEvent(var Event:TEvent);virtual;
  end;

  PFlexDialog = ^TFlexDialog;
  TFlexDialog = object(TDialog)
    constructor Init(var r:trect; ahdr:FnameStr);
  end;

  PAreaLister = ^TAreaLister;
  TAreaLister = object(TFormattedLister)
    constructor Init(x,y:integer; rowcount:byte);
    function GetColor(item:longint):byte;virtual;
    function GetText(item:longint):string;virtual;
    procedure ItemDoubleClicked(item:longint);virtual;
  end;

  PAreaChoiceLister = ^TAreaChoiceLister;
  TAreaChoiceLister = object(TAreaLister)
    function GetColor(item:longint):byte;virtual;
    procedure ItemDoubleClicked(item:longint);virtual;
  end;

  PAreaChangeDialog = ^TAreaChangeDialog;
  TAreaChangeDialog = object(TDialog)
    Lister      : PAreaChoiceLister;
    constructor Init(alist:PAreaColl; acurrent:PArea);
  end;

  PReplyLister = ^TReplyLister;
  TReplyLister = object(TFormattedLister)
    function   GetText(item:longint):string;virtual;
    procedure ItemDoubleClicked(item:longint);virtual;
  end;

  PReplyListWindow = ^TReplyListWindow;
  TReplyListWindow = object(TFlexDialog)
    Lister : PReplyLister;
    constructor Init;
    procedure HandleEvent(var Event:TEvent);virtual;
  end;

  PAreaListWindow = ^TAreaListWindow;
  TAreaListWindow = object(TFlexDialog)
    FullList  : PAreaLister;
    EmptyList : PAreaLister;
    constructor Init(atitle:FnameStr);
    procedure HandleEvent(var Event:TEvent);virtual;
    function Valid(acmd:word):boolean;virtual;
  end;

  PMsgLister = ^TMsgLister;
  TMsgLister = object(TFormattedLister)
    constructor Init(amax:word);
    function GetText(item:longint):string;virtual;
    function GetColor(item:longint):byte;virtual;
    procedure ItemDoubleClicked(item:longint);virtual;
  end;

  PMsgListWindow = ^TMsgListWindow;
  TMsgListWindow = object(TFlexDialog)
    Lister       : PMsgLister;
    IdCmd        : word;
    constructor Init(atitle:FnameStr; alist:PCollection; aidcmd:word);
    procedure   HandleEvent(var Event:TEvent);virtual;
  end;

  PMsgDialog = ^TMsgDialog;
  TMsgDialog = object(TDialog)
    Addr   : PInputLine;
    N      : PButton;
    Area   : PArea;
    Msg    : PMsg;
    Filter : string;
    constructor Init(ahdr:FnameStr; anetmail:boolean);
    procedure   HandleEvent(var Event:TEvent);virtual;
    procedure   Update(aarea:PArea; amsg:PMsg);
    procedure   GetInfo;
  end;

  PTaglineLister = ^TTaglineLister;
  TTaglineLister = object(TStringViewer)
    procedure ItemDoubleClicked(item:longint);virtual;
  end;

  PTaglineInputDialog = ^TTaglineInputDialog;
  TTaglineInputDialog = object(TDialog)
    constructor Init(savebox:boolean);
  end;

  PTaglineSelectDialog = ^TTaglineSelectDialog;
  TTaglineSelectDialog = object(TDialog)
    Lister : PTaglineLister;
    constructor Init(alist:PTaglineCollection);
    procedure HandleEvent(var Event:TEvent);virtual;
  end;

  PMarkDialog = ^TMarkDialog;
  TMarkDialog = object(TDialog)
    constructor Init;
  end;

  PPersonInputDialog = ^TPersonInputDialog;
  TPersonInputDialog = object(TDialog)
    constructor Init(ahdr:FnameStr);
    procedure GetData(var rec);virtual;
    procedure SetData(var rec);virtual;
  end;

  PPersonLister = ^TPersonLister;
  TPersonLister = object(TFormattedLister)
    function GetText(item:longint):string;virtual;
    procedure ItemDoubleClicked(item:longint);virtual;
  end;

  PPersonDialog = ^TPersonDialog;
  TPersonDialog = object(TDialog)
    Lister      : PPersonLister;
    constructor Init(ahdr:FnameStr);
    function Valid(acmd:word):boolean;virtual;
    procedure HandleEvent(var Event:TEvent);virtual;
  end;

  PSearchDialog = ^TSearchDialog;
  TSearchDialog = object(TDialog)
    constructor Init(ahdr:FnameStr);
  end;

  PAreaAdjustDialog = ^TAreaAdjustDialog;
  TAreaAdjustDialog = object(TDialog)
    constructor Init(ahdr:FnameStr);
    procedure SetData(var rec);virtual;
    procedure GetData(var rec);virtual;
  end;

  PNetFlagChangeDialog = ^TNetFlagChangeDialog;
  TNetFlagChangeDialog = object(TDialog)
    constructor Init;
    procedure SetData(var rec);virtual;
    procedure GetData(var rec);virtual;
  end;

  PFilterLister = ^TFilterLister;
  TFilterLister = object(TFormattedLister)
    constructor Init(x,y:integer);
    function GetText(item:longint):string;virtual;
    procedure ItemDoubleClicked(item:longint);virtual;
  end;

  PFilterSelLister = ^TFilterSelLister;
  TFilterSelLister = object(TStringViewer)
    procedure ItemDoubleClicked(item:longint);virtual;
  end;

  PFilterLookupDialog = ^TFilterLookupDialog;
  TFilterLookupDialog = object(TDialog)
    LLister           : PFilterSelLister;
    RLister           : PFilterLister;
    constructor Init(ahdr:string);
    procedure HandleEvent(var Event:Tevent);virtual;
    procedure GetData(var rec);virtual;
    procedure SetData(var rec);virtual;
    function  DataSize:word;virtual;
  end;

  TMoment = record
    Year  : word;
    Month : byte;
    Day   : byte;
    Hour  : byte;
    Min   : byte;
  end;

  PDateTimeViewer = ^TDateTimeViewer;
  TDateTimeViewer = object(TView)
    Moment        : TMoment;
    Alarm         : TMoment;
    Font          : word;
    constructor Init(x,y:integer; afont:word);
    procedure Paint;virtual;
    procedure Backprocess;virtual;
    procedure GetMoment(var amoment:TMoment);
    procedure HandleEvent(var Event:TEvent);virtual;
  end;

const

  CDPlayer       : PSimpleCDPlayer = NIL;
  DateTimeViewer : PDateTimeViewer = NIL;

function  AreaChangeDialog(aarea:PArea; acflags:word):PArea;
function  EditMsg(fn:FnameStr; filtercmd:string; amsg:PMsg; addtag:boolean):boolean;
function  TaglineSelector:FnameStr;
function  NonEmptyFilter(P:PArea):boolean;
function  EmptyFilter(P:PArea):boolean;
function  FindSpecific(windowheader,notfoundmsg:FNameStr; func:Pointer; warn:boolean):PView;
procedure WriteMsg(who,subj,ahdr:FnameStr; destarea:PArea; quote:PMsg;
                   text:PMsgText;writeflags:word);
procedure AddTagline(fn:FnameStr);
procedure OpenArea(P:PArea);
procedure SaveExtraPacketInfo;
procedure FindPersonal(warn:boolean);
procedure FindForgotten(warn:boolean);
procedure WriteSetup;
function  ReadSetup:TSetupResult;
procedure CheckReplyPacketConflict;
function  OpenPacket(apakname,apakdir,arepdir:FnameStr; unpackreplies,fakepacket:boolean):boolean;
procedure ClosePacket;
function CompareMsg(Pm:PMsg):boolean;

var

  srchRec : TSearchScr;

implementation

uses

  XDev,XPrn,XBuf,XStream,XLan,WLan,XStr;

{- OpenArea -}
procedure OpenArea(P:PArea);
var
  ftiList:PDumbCollection;
  Pm:PMsg;
  n:integer;
  Pv:PView;

  function Test(av:PView):boolean;far;
  begin
    Test := false;
    if (av^.viewType = vtWindow) and (PWindow(av)^.Header^=P^.Name) then Test := true;
  end;
begin
  if P^.Total = 0 then exit;
  EventWait;
  Pv := GSystem^.FirstThat(@Test);
  if Pv <> NIL then begin
    Pv^.Select;
    exit;
  end;
  New(ftilist,init(10,10));
  for n:=0 to Packet^.msgList^.Count-1 do begin
    Pm := Packet^.msgList^.At(n);
    if Pm^.Area = P then ftiList^.Insert(Packet^.msgList^.At(n));
  end;
  GSystem^.Insert(new(PMsgListWindow,Init(P^.Name,ftiList,cmAreYouMsgList)));
end;

{- Find procs -}
function IsToBeReplied(P:PMsg):boolean;far;
begin
  IsToBeReplied := P^.Marks and wmmReply > 0;
end;

procedure FindWillReply;
begin
  FindSpecific(gtid(msWillReplyHeader),gtid(msWillReplyNotFoundMsg),@IsToBeReplied,true);
end;

function IsToBeRead(P:Pmsg):boolean;far;
begin
  IsToBeRead := P^.Marks and wmmRead > 0;
end;

procedure FindWillRead;
begin
  FindSpecific(gtid(msWillreadHeader),gtid(mswillReadNotFoundMsg),@IsToBeRead,true);
end;

function IsPersonal(P:Pmsg):boolean;far;
begin
  IsPersonal := P^.Flags and wmfPersonal > 0;
end;

procedure FindPersonal;
begin
  FindSpecific(gtid(msPersonalMsg),gtid(msPersonalNotFound),@IsPersonal,warn);
end;

function IsUnread(P:PMsg):boolean;far;
begin
  IsUnread := P^.Flags and wmfRead = 0;
end;

procedure FindUnread;
begin
  FindSpecific(gtid(msUnreadHeader),gtid(msUnreadNotFoundmsg),@IsUnread,true);
end;

function IsForgotten(P:PMsg):boolean;far;
begin
  IsForgotten := (P^.Flags and (wmfPersonal+wmfReplied)) = wmfPersonal;
end;

procedure FindForgotten;
begin
  FindSpecific(gtid(msForgottenHeader),gtid(msForgottenNotFoundMsg),@IsForgotten,warn);
end;

function FindSpecific;
var
  ftilist:PDumbCollection;
  Pw:PMsgListWindow;
  peewee:PView;
  internalCall:boolean;
begin
  FindSpecific := NIL;
  EventWait;
  internalCall := (notfoundmsg = '');
  Pw := Message(GSystem,evBroadcast,cmAreYouUnread,NIL);
  if Pw <> NIL then Message(Pw,evCommand,cmClose,NIL);
  ftiList := Packet^.FindSpecificItems(func,internalCall);
  if ftiList^.Count > 0 then begin
    peewee := New(PMsgListWindow,Init(windowheader+' ('+l2s(ftiList^.Count)+' adet)',ftiList,cmAreYouUnread));
    GSystem^.Insert(peewee);
    FindSpecific := peewee;
  end else begin
    Dispose(FtiList,Done);
    if (not internalCall) and warn then ExecBox(^C+notfoundmsg,windowheader,0,GetBlock(0,0,mnfHorizontal,
      NewButton(gtid(msNotFoundOKButton),cmOK,NIL)));
  end;
end;

procedure SaveExtraPacketInfo;
var
  rec:TPacket;
  temp:TPacket;
  T:TDosStream;
  fn:FnameStr;
  pos:longint;
  fullpak:String[12];
begin
  fullpak := XGetFileName(currentPacketName);
  fn := stdPakDir^+FullPak;
  rec.Name := Lower(FullPak);
  rec.Size := XGetFileSize(fn);
  rec.Time := XGetFileDate(fn);
  rec.Total := Packet^.msgList^.Count;
  rec.Unread := Packet^.GetSpecificCount(@IsUNread);
  rec.WillRead := Packet^.GetSpecificCount(@IsToBeRead);
  rec.WillReply := Packet^.GetSpecificCount(@IsToBeReplied);
  rec.Personal := Packet^.GetSpecificCount(@IsPersonal);
  T.Init(wkdir^+cacheFile,stOpen);
  if T.status <> stOK then begin
    T.Init(wkdir^+cacheFile,stCreate);
    if T.Status <> stOK then begin
      Debug('SaveExtraPacketInfo: File creation error');
      T.Done;
      exit;
    end;
  end;
  while T.GetPos < T.GetSize do begin
    pos := T.GetPos;
    T.Read(temp,SizeOf(temp));
    if T.Status <> stOK then begin
      T.Done;
      T.Init(wkdir^+cacheFile,stCreate);
      break;
    end;
    if temp.Name = rec.Name then begin
      T.Seek(pos);
      break;
    end;
  end;
  T.Write(rec,SizeOf(rec));
  T.Done;
end;

{- Msg filter procs -}
function NonEmptyFilter(P:PArea):boolean;
begin
  NonEmptyFilter := P^.Total > 0;
end;

function EmptyFilter(P:PArea):boolean;
begin
  EmptyFilter := (P^.Total = 0);
end;

{- FilterAreaList -}
function FilterAreaList(aproc:TFilterProc):PAreaColl;
var
  P:PAreaColl;
  n:integer;
  Pa:PArea;
begin
  New(P,Init(10,10));
  for n:=0 to Packet^.areaList^.Count-1 do begin
    Pa := Packet^.areaList^.At(n);
    if aproc(Pa) then P^.Insert(Pa);
  end;
  FilterAreaList := P;
end;

{- TaglineSelector -}
function TaglineSelector:FnameStr;
var
  tagList:PTaglineCollection;
  Pt:PTaglineSelectDialog;
  code:word;
  i:integer;
begin
  TaglineSelector := '';
  if setup.Behaviour = tbNone then exit;
  tagList := getTaglineList;
  if tagList^.Count=0 then begin
    Dispose(tagList,Done);
    exit;
  end;
  if setup.Behaviour = tbAuto then begin
    i := random(tagList^.Count);
    TaglineSelector := PString(tagList^.At(i))^;
    Dispose(tagList,Done);
  end else begin
    New(Pt,Init(tagList));
    code := GSystem^.ExecView(Pt);
    if code = cmOK then TaglineSelector := PString(tagList^.At(Pt^.Lister^.FocusedItem))^;
    Dispose(Pt,Done);
  end;
end;

{- AddTagline -}
procedure AddTagline(fn:FnameStr);
var
  T:TDosStream;
  tagline:FnameStr;
begin
  if setup.Behaviour <> tbNone then begin
    tagline := TaglineSelector;
    if tagline <> '' then begin
      EventWait;
      T.Init(fn,stOpen);
      T.Seek(T.GetSize);
      SWriteln(T,#13#10+'... '+tagline);
      T.Done;
    end;
  end;
end;

function VivaLaSewage(fn:FnameStr):boolean;
var
  T:TDosStream;
  lines:longint;
  quote:longint;
  perc:longint;
  s:string;
begin
  T.Init(fn,stOpenRead);
  lines := 0;
  quote := 0;
  while T.GetPos < T.getSize do begin
    SReadln(T,s);
    Strip(s);
    inc(lines,length(s));
    if pos('> ',s) in [1..6] then inc(quote,length(s));
  end;
  T.Done;
  perc := (quote*100) div lines;
  Debug('viva-la-sewage percent: '+l2s(perc));
  VivaLaSewage := perc > VivaLaSewagePercent;
end;

{- EditMsg -}
function EditMsg(fn:FnameStr; filtercmd:string; amsg:PMsg; addtag:boolean):boolean;
var
  ouf:FnameStr;
begin
  EditMsg := false;
  repeat {whilce portacio}
    if not EditFile(fn) then begin
      XDeleteFile(fn);
      exit;
    end;
    if setup.Flags and ufAskPost > 0 then
      if ExecBox(gtid(msAskPostTxt),
                 gtid(msAskPostHdr),hcAskPost,GetBlock(0,0,mnfHorizontal,
                 NewButton(gtid(msAskPostYes),cmOK,
                 NewButton(gtid(msAskPostNo),cmCancel,
                 NewButton(gtid(msAskPostHelp),cmHelp,
                 NIL))))) <> cmOK then begin
      XDeleteFile(fn);
      exit;
    end;
    EventWait;
    amsg^.Date := (GetSysDate shl 16) or GetSysTime;
    if addtag then begin
      if filtercmd <> '' then DoFilter(fn,filtercmd);
      AddTagline(fn);
    end;
    if Setup.Flags and ufNoVivaLaSewage = 0 then begin {cryin' / aerosmith}
      if VivaLaSewage(fn) then begin
        if ExecBox( gtid(msVivaLaSewageMsg), gtid(msVivaLaSewageHdr), hcVivaLaSewage, GetBlock(0,0,mnfHorizontal,
                               NewButton(gtid(msVivaLaSewageOK),cmOK,
                               NewButton(gtid(msVivaLaSewageCancel),cmCancel,
                               NewButton(gtid(msVivaLaSewageHelp),cmHelp,
                               NIL))))) <> cmOK then exit;
      end else break;
    end else break;
  until false;
  PostMsg(fn,amsg);
  XDeleteFile(fn);
  EditMsg := true;
end;

{- WriteMsg -}
procedure WriteMsg(who,subj,ahdr:FnameStr; destarea:PArea; quote:PMsg;
                   text:PMsgText;writeflags:word);
const
  aliasFlags = wafAlias;
var
  msgrec:TMsg;
  P:PMsgDialog;
  code:word;
  activeFilter:string;
  function thebox:word;
  begin
    thebox := ExecBox(gtid(msAskForwardTxt),
                                                      gtid(msAskForwardHdr),0,
      GetBlock(0,0,mnfHorizontal,
        NewButton(gtid(msAskForwardNo),cmNo,
        NewButton(gtid(msAskForwardYes),cmYes,
        NewButton(gtid(msAskForwardCancel),cmCancel,
        NIL)))));
  end;
  function JumpingJack:boolean;
  begin
    JumpingJack := false;
    msgrec.Area := destarea;
    if (writeFlags and (wfForward or wfCrosspost)) > 0 then case thebox of
      cmNo : begin
               PostMsg(wkdir^+tempFile,@msgrec);
               exit;
             end;
      cmYes : ;
      else begin
        XDeleteFile(wkdir^+tempFile);
        exit;
      end;
    end; {case}
    JumpingJack := EditMsg(wkdir^+tempFile,activeFilter,@msgrec,true);
  end;

  function PrepareReply:boolean;
  var
    T:TDosStream;
    line:string;
    lastword:string;
    myinits,yourinits:string;
    kukluxklan:FnameStr;
    w:word;
    c:char;
    b:byte;
    Pt:PMsgText;
    procedure putAreaStr(id:word);
    begin
      kukluxklan := #13#10+gtid(id)+#13#10;
      Replace(kukluxklan,'%A',quote^.Area^.Name);
      SWriteln(T,kukluxklan);
    end;
    procedure outline;
    var
      temp:string;
    begin
      temp := line;
      Strip(temp);
      if temp[1] <> #1 then begin
        if quote <> NIL then
          if writeFlags and wfQuote > 0 then
            if temp <> '' then
              if not (pos('> ',temp) in [1..6]) then line := yourinits+temp;
        SWriteln(T,line);
      end;
      line := '';
      if w < Text^.Size-1 then if Text^.Data[w+1] = #10 then inc(w);
    end;
  begin
    EventWait;
    PrepareReply := false;
    T.Init(wkdir^+tempFile,stCreate);
    if T.Status <> stOK then begin
      T.Done;
      MessageBox(gtid(msTempFileCreateErrorMsg),hcFileError,mfError);
      exit;
    end;

    if writeflags and wfQuote > 0 then begin
      SWriteln(T,GetQuoteStr(quote)+#13#10);
      yourinits := getinitials(quote^.From);
    end;

    if quote <> NIL then begin
      if writeFlags and wfCrosspost > 0 then putAreaStr(msCrosspostString)
      else if writeFlags and wfForward > 0 then begin
        putAreaStr(msForwardString);
        WriteMsgHeaderToTextStream(quote,T);
      end else if quote^.Area <> destArea then putAreaStr(msReplyAnotherAreaHeader);
    end;
    Pt := Text;
    line := '';
    lastword := '';
    while Pt <> NIL do begin
      for w:=0 to Text^.Size-1 do case Text^.Data[w] of
        #32 : if length(line+lastword) > wrapLimit then begin
                OutLine;
                line := lastword + #32;
                lastword := '';
              end else begin
                line := line + lastword + #32;
                lastword := '';
              end;
        #13 : if length(line+lastword) > wrapLimit then begin
                OutLine;
                line := lastword + #32;
                lastword := '';
              end else begin
                line := line + lastword;
                OutLine;
                lastword := '';
              end;
        else lastword := lastword + Text^.Data[w];
      end; {case & for}
      line := line+lastword;
      outline;
      Pt := Pt^.Next;
    end;
    T.Done;
    PrepareReply := true;
  end;
begin
  if destarea^.Flags and wafPost = 0 then begin
    ExecBox(gtid(msNoPostTxt),gtid(msNoPostHdr),
    hcNoWrite,GetBlock(0,0,mnfHorizontal,
      NewButton(gtid(msNoPostOK),cmOK,
      NewButton(gtid(msNoPostHelp),cmHelp,
      NIL))));
    exit;
  end;
  ClearBuf(msgrec,SizeOf(msgrec));
  activeFilter := setup.FilterCmd;
  if (destarea^.Flags and aliasFlags) = aliasFlags then msgrec.From := user.Alias
                                                   else msgrec.From := user.name;
  msgrec.Too := who;
  msgrec.Area := destarea;
  if quote <> NIL then begin
    if quote^.Addr.Zone <> 0 then msgrec.Addr := quote^.Addr
                             else GetOrigin(text,msgrec.Addr);
  end;
  msgrec.Subj  := subj;
  New(P,Init(ahdr,writeflags and wfNetmail > 0));
  P^.Update(destarea,@msgrec);
  P^.Filter := activeFilter;
  code := GSystem^.ExecView(P);
  if code = cmOK then begin
    P^.GetInfo;
    activeFilter := P^.Filter;
    destarea := P^.Area;
  end;
  Dispose(P,Done);
  if code <> cmOK then exit;

  if PrepareReply then if JumpingJack then if quote <> NIL then begin
    if writeFlags and wfQuote = 0 then exit;
    if quote^.Flags and wmfReplied > 0 then exit;
    quote^.Flags := quote^.Flags or wmfReplied;
    Packet^.UpdateMsgFlags(quote);
  end;
end;

{- AreaChangeDialog -}
function AreaChangeDialog(aarea:PArea; acflags:word):PArea;
var
  P:PAreaChangeDialog;
  Pac:PAreaColl;
  Pa:PArea;
  n:integer;
  code:word;
begin
  EventWait;
  AreaChangeDialog := NIL;
  New(Pac,Init(10,10));
  for n:=0 to Packet^.AreaList^.Count-1 do begin
    Pa := Packet^.AreaList^.At(n);
    if (acflags and acHideSelf > 0) and (Pa = aarea) then continue;
    if (acflags and acNetmail > 0) and (Pa^.AreaType <> watNetmail) then continue;
    if Pa^.Flags and wafPost > 0 then Pac^.Insert(Pa);
  end;
  if Pac^.Count = 0 then begin
    Dispose(Pac,Done);
    exit;
  end;
  if Pac^.Count = 1 then begin
    AreaChangeDialog := Pac^.At(0);
    Pac^.Count := 0;
    Dispose(Pac,Done);
    exit;
  end;
  New(P,Init(Pac,aArea));
  code := GSystem^.ExecView(P);
  if code = cmOK then begin
    Pa := Pac^.At(P^.Lister^.FocusedItem);
    AreaChangeDialog := Pa;
  end;
  Pac^.Count := 0;
  Dispose(P,Done);
end;

{- TFlexDialog -}
constructor TFlexDialog.Init;
begin
  inherited Init(R,ahdr);
  Options := Options or Ocf_ReSize or Ocf_Centered;
end;

{- TAreaLister -}
constructor TAreaLister.Init;
begin
  inherited Init(x,y,fntProp,rowcount,
    NewColumn(gtid(msAreaListArea),216,0,
    NewColumn(gtid(msAreaListType),60,0,
    NewColumn(gtid(msAreaListDL),60,0,
    NewColumn(gtid(msAreaListTotal),34,cofRJust,
    NewColumn(gtid(msAreaListPers),34,cofRJust,
    NewColumn(gtid(msAreaListUnread),34,cofRJust,
    NIL)))))));
  SetConfig(Lvc_KeepList,True);
end;

function TAreaLister.GetColor;
var
  Pa:PArea;
begin
  Pa := ItemList^.At(Item);
  if (Pa^.Total = 0) or (Pa^.Unread=0) then GetColor := cDarkgray
                                       else GetColor := cBlack;
end;

procedure TareaLister.ItemDoubleClicked;
begin
  Message(Owner,evCommand,cmOpen,NIL);
end;

function TAreaLister.GetText;
var
  P:PArea;
  wha:string;
  how:string;
  s:string;
begin
  GetText := '';
  P := ItemList^.At(item);
  wha := gtid(P^.AreaType+msAreaTypeLocal);
  if P^.UserStat <> wusNone then how := gtid(P^.UserStat+msUserStatNone)
                            else how := '';
  s := P^.Name+'|'+wha+'|'+how+'|';
  if P^.Total > 0 then s := s + l2s(P^.Total);
  s := s + '|';
  if (P^.Pers = P^.Total) and (P^.Total > 0) then s := s + 'tumu' else
    if P^.Pers > 0 then s := s + l2s(P^.Pers);
  s := s + '|';
  if P^.Unread > 0 then s := s + l2s(P^.Unread);
  GetText := s;
end;

{- TAreaChoiceLister -}
function TAreaChoiceLister.GetColor;
begin
  GetColor := cBlack;
end;

procedure TAreaChoiceLister.ItemDoubleClicked;
begin
  Message(Owner,evCommand,cmOK,@Self);
end;

{- TReplyLister -}
function TReplyLister.GetText;
var
  Pr:PMsg;
begin
  Pr := ItemList^.At(item);
  GetText := Pr^.From+'|'+Pr^.Too+'|'+Pr^.Subj+'|'+Pr^.Area^.Name;
end;

procedure TReplyLister.ItemDoubleClicked;
begin
  Message(Owner,evCommand,cmRead,NIL);
end;

{- TReplyListWindow -}
constructor TReplyListWindow.Init;
var
  R:TRect;
  Ps:PScrollBar;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(msReplyListWindowHeader));
  Options := (Options and not (Ocf_Close+Ocf_CenterY));
  New(Lister,Init(5,5,fntProp,7,
    NewColumn(gtid(msReplyListFrom),120,0,
    NewColumn(gtid(msReplyListTo),120,0,
    NewColumn(gtid(msReplyListSubj),120,0,
    NewColumn(gtid(msReplyListArea),120,0,
    NIL))))));
  Lister^.SetConfig(Lvc_KeepList,True);
  Lister^.NewList(replyPacket^.msgList);
  Lister^.GetBounds(R);
  r.a.x := r.b.x+3;
  r.b.x := r.a.x+scrollerXSize;
  New(Ps,Init(R));
  Insert(Ps);
  Insert(Lister);
  Lister^.AssignScroller(Ps);
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msReplyListRead),cmRead,
    NewButton(gtid(msReplyListDelete),cmDelete,
    NewButton(gtid(msReplyListNewPacket),cmNewPacket,
    NewButton(gtid(msReplyListHelp),cmHelp,
    NIL))))));
  HelpContext := hcReplyListWindow;
  FitBounds;
  GetBounds(R);
  r.a.y := ScreenY-Size.Y-18;
  r.b.y := r.a.y+Size.Y-1;
  ChangeBounds(R);
  MaxSize := Size;
end;

procedure TReplyListWindow.HandleEvent;
  procedure ReadRep;
  var
    Pr:PReplyWindow;
    P:PMsg;
  begin
    if Lister^.ItemList^.Count = 0 then exit;
    P := Lister^.ItemList^.At(Lister^.FocusedItem);
    Pr := Message(Owner,evBroadcast,cmAreYouReplier,NIL);
    if Pr <> NIL then begin
      Pr^.NewMsg(P);
      Pr^.Select;
    end else begin
      New(Pr,Init(P));
      GSystem^.Insert(Pr);
    end;
  end;

  procedure DeleteRep;
  var
    P:PMsg;
    Pr:PReplyWindow;
  begin
    if Lister^.itemList^.Count = 0 then exit;
    if ExecBox(gtid(msReplyDeleteAskTxt),gtid(msReplyDeleteAskHdr),0,GetBlock(0,0,mnfHorizontal,
      NewButton(gtid(msReplyDeleteOK),cmOK,
      NewButton(gtid(msReplyDeleteCancel),cmCancel,
      NIL)))) <> cmOK then exit;
    EventWait;
    P := Lister^.ItemList^.At(Lister^.FocusedItem);
    Pr := Message(Owner,evBroadcast,cmAreYouReplier,NIL);
    ReplyPacket^.DeleteMsg(P);
    Lister^.DeleteItem(Lister^.FocusedItem);
    if Pr <> NIL then begin
      if Lister^.ItemList^.Count = 0 then Message(Pr,evCommand,cmClose,Pr)
        else Pr^.NewMsg(Lister^.ItemList^.At(Lister^.FocusedItem));
    end;

  end;

  function GoGo(aitem:integer):boolean;
  var
    Pr:PReplyWindow;
  begin
    GoGo := false;
    Pr := Event.infoPtr;
    if Pr^.Reply <> Lister^.ItemList^.At(Lister^.FocusedItem) then exit;
    Lister^.FocusItem(aitem);
    if Lister^.FocusedItem = aitem then Pr^.NewMsg(Lister^.ItemList^.At(aitem))
                                   else Message(Pr,evCommand,cmClose,Pr);
    GoGo := true;
  end;

  procedure RefreshReplies(last:boolean);
  var
    oldfocused:integer;
    Pr:PReplyWindow;
  begin
    EventWait;
    Pr := Message(Owner,evBroadcast,cmAreYouReplier,NIL);
    ReplyPacket^.Clear;
    ReplyPacket^.Load;
    oldfocused := Lister^.FocusedItem;
    Lister^.ItemList := NIL;
    Lister^.NewList(replyPacket^.msgList);
    Lister^.PaintView;
    if replyPacket^.MsgList^.Count > 0 then
      if last then Lister^.FocusItem(Lister^.ItemList^.Count-1)
              else Lister^.FocusItem(oldfocused);
    if Pr <> NIL then Pr^.NewMsg(Lister^.ItemList^.At(Lister^.FocusedItem));
  end;

  procedure ClearReplyPacket;
  begin
    if Lister^.ItemList^.Count = 0 then exit;
    if ExecBox(gtid(msNewPacketAskTxt),
               gtid(msNewPacketAskHdr),hcNewPacket,GetBlock(0,0,mnfHorizontal,
               NewButton(gtid(msNewPacketAskyes),cmYes,
               NewButton(gtid(msNewPacketAskno),cmNo,
               NewButton(gtid(msNewPacketAskhelp),cmHelp,
               NIL))))) = cmYes then begin
      EventWait;
      XDeleteWild(stdrepDir^+'*.*');
      XDeleteAnyway(ReplyName(packet));
      RefreshReplies(false);
    end;
  end;

  procedure ArchiveReplies;
  begin

  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand : case Event.Command of
                  cmRead : ReadRep;
                  cmDelete : DeleteRep;
                  cmNewPacket : ClearReplyPacket;
                  cmArchiveReplies : ArchiveReplies;
                  else exit;
                end; {case}
    evBroadcast : case Event.Command of
                    cmIsThatYou : if Event.InfoPtr <> NIL then exit;
                    cmRefreshReplies : RefreshReplies(Event.InfoPtr <> NIL);
                    cmOwnerDel : DeleteRep;
{                    cmShowReplies : Select;}
                    cmReplyReadMode : ReadRep;
                    cmReplyPrev : if not GoGo(Lister^.FocusedItem-1) then exit;
                    cmReplyNext : if not GoGo(Lister^.FocusedItem+1) then exit;
                    else exit;
                  end; {case}
    evKeyDown : case Event.KeyCode of
                  kbDel : DeleteRep;
                  else exit;
                end; {case}
    else exit;
  end; {case}
  ClearEvent(Event);
end;

{- TMsgHeader -}
constructor TMsgHeader.Init;
begin
  abounds.b.y := abounds.a.y+(3*(GetFontHeight(afont)+2))+2;
  inherited Init(abounds);
  EventMask := 0;
  Font      := afont;
  FontH     := GetFontHeight(Font);
  GrowMode  := gmFixedLoX+gmFixedHiX;
end;

{- TMsgHeader -}
procedure TMsgHeader.GetMsg;
begin
  with PMsgWindow(Owner)^.Msg^ do begin
    info.From := From;
    info.Too  := Too;
    info.Subj := Subj;
    info.Addr := Addr;
    info.Date := Date;
    info.Netmail := Area^.AreaType = watNetmail;
    info.Reply := false;
  end;
end;

procedure TMsgHeader.Paint;
var
  R:TRect;
  gap:integer;
  x,y:integer;
  temp:string;
  info:THdrInfo;
  hdrwidth:word;
  procedure PutThis(s:FnameStr);
  var
    width:word;
  begin
    width := GetStringSize(Font,s);
    if width > (Size.X-x) then width := Size.X-x;
    XPrintStr(x,y,width,Font,s);
    inc(y,gap);
  end;

  procedure PutThat(s:FnameStr);
  var
    width:word;
  begin
    width := GetStringSize(Font,s);
    XPrintStr(Size.X-width-3,y,width,Font,s);
    inc(y,gap);
  end;

  procedure PutShit(s:FnameStr);
  begin
    XPrintStr(x,y,hdrwidth,Font,s);
    inc(y,gap);
  end;

begin
  if Owner <> NIL then GetMsg(info);
  FastPaintBegin;
    GetExtent(R);
    SetFillStyle(SolidFill,Col_Back);
    ShadowBox(R,False);
    R.Grow(-1,-1);
    XBox(R,True);
    R.Grow(-3,-1);
    SetTextColor(cBlue,Col_Back);
    gap := FontH+2;
    x := r.a.x;
    y := r.a.y;
    hdrwidth := GetStringSize(Font,gtid(msMsgHeaderFrom))+4;
    putshit(gtid(msMsgHeaderFrom));
    putshit(gtid(msMsgHeaderTo));
    putshit(gtid(msMsgHeaderSubj));
    y := r.a.y;
    inc(x,hdrwidth);
    SetTextColor(cLightBlue,Col_Back);
    if not info.Reply then begin
      temp := info.From;
      if info.Netmail then temp := temp+' ('+Addr2Str(info.Addr)+')';
      putthis(temp);
      putthis(info.Too);
    end else begin
      putthis(info.From);
      temp := info.Too;
      if info.Netmail then temp := temp+' ('+Addr2Str(info.Addr)+')';
      putthis(temp);
    end;
    putthis(info.Subj);
    y := r.a.y;
    SetTextColor(cBlack,Col_Back);
    putthat(Date2Str(LongRec(info.Date).Hi,true));
    putthat(Time2Str(LongRec(info.Date).Lo));
  PaintEnd;
end;

{- TReplyHeader -}
procedure TReplyHeader.GetMsg;
begin
  with PReplyWindow(Owner)^.Reply^ do begin
    info.From := From;
    info.Too  := Too;
    info.Subj := Subj;
    info.Addr := Addr;
    info.Date := Date;
    info.Netmail := Area^.AreaType = watNetmail;
    info.Reply := true;
  end;
end;

{- TMsgStatus -}
constructor TMsgStatus.Init;
begin
  inherited Init(abounds);
  EventMask := 0;
end;

procedure TMsgStatus.Paint;
var
  R:TRect;
  Msg:PMsg;
  fh:integer;
  xsize:word;
  procedure Putit(amsg:FnameStr; isit:boolean; high,low:byte);
  var
    bc:byte;
  begin
    if isit then bc := high else bc := low;
    SetTextColor(Black,bc);
    XPrintStr(r.a.x,r.a.y,xsize,fntPropSmall,amsg);
    inc(r.a.y,fh);
    SetColor(cBlack);
    XLine(1,r.a.y,Size.X-2,r.a.y);
    inc(r.a.y);
  end;
begin
  Msg := PMsgWindow(Owner)^.Msg;
  fh  := GetFontHeight(fntPropSmall);
  xsize := (Size.X-4) div 2;
  FastPaintBegin;
    GetExtent(R);
    SetColor(cBlack);
    ShadowBox(R,False);
    R.Grow(-1,-1);
    SetColor(cBlack);
    XBox(R,False);
    R.Grow(-1,-1);
    putit(gtid(msMsgStatusRead),Msg^.Flags and wmfRead <> 0,cYellow,cBrown);
    putit(gtid(msMsgStatusReplied),Msg^.Flags and wmfReplied <> 0,cLightGreen,cGreen);
    putit(gtid(msMsgStatusPersonal),Msg^.Flags and wmfPersonal <> 0,cLightRed,cRed);
    r.a.y := 2;
    inc(r.a.x,xsize);
    XLine(r.a.x,1,r.a.x,Size.Y-2);
    inc(r.a.x);
    putit(gtid(msMsgStatusWillRead),Msg^.Marks and wmmRead <> 0,cLightBlue,cBlue);
    putit(gtid(msMsgStatusWillReply),Msg^.Marks and wmmReply <> 0,cLightBlue,cBlue);
    putit(gtid(msMsgStatusBWSave),Msg^.Marks and wmmSave <> 0,cLightBlue,cBlue);
  PaintEnd;
end;

{- TTextViewer -}
constructor TTextViewer.Init;
begin
  inherited Init(abounds);
  EventMask := evMouse+evBroadCast+evCommand+evKeyboard;
  GrowMode  := gmFixedAll;
  ScrollBar := AScrollBar;
  OrigY     := -1;
  Options   := Options or Ocf_PreProcess or Ocf_PaintFast;
  NewText(atext);
end;

procedure TTextViewer.NewText;
var
  w:word;
  b:byte;
begin
  Text := atext;
  ScrTop := 0;
  EndY   := 0;
  if Keyword <> NIL then begin
    DisposeStr(Keyword);
    Keyword := NIL;
  end;
  Paint;
  UpdateScroller;
end;

procedure TTextViewer.UpdateScroller;
begin
  ScrollBar^.Update(ScrTop,EndY,setup.readerFontH*2,Size.Y,False);
  ScrollBar^.PaintView;
end;

procedure TTextViewer.ChangeBounds;
begin
  inherited ChangeBounds(R);
  Go(ScrTop);
  updateScroller;
end;

procedure TTextViewer.Go;
begin
  if EndY-where < Size.Y then where := EndY-Size.Y;
  if where < 0 then where := 0;
  if where = ScrTop then exit;
  ScrTop := where;
  PaintView;
end;

procedure TTextViewer.DoSearch;
begin
  if Keyword <> NIL then DisposeStr(Keyword);
  Keyword := NewStr(Upper(s));
  inc(Owner^.LockCount);
  Paint;
  dec(Owner^.LockCount);
  if keywhere < endy then Go(keywhere);
end;

destructor TTextViewer.Done;
begin
  if Keyword <> NIL then DisposeStr(Keyword);
  inherited Done;
end;

procedure TTextViewer.HandleEvent(var Event:Tevent);
  procedure kgo(where:integer);
  begin
    ClearEvent(Event);
    go(where);
    updateScroller;
  end;
begin
  inherited HandleEvent(Event);
  case Event.What of
    evKeydown : case Event.KeyCode of
                  kbDown : kGo(ScrTop+setup.readerFontH*2);
                  KbUP   : kGo(ScrTop-(setup.readerFontH*2));
                  kbPgUp : kGo(ScrTop-Size.Y);
                  kbPgDn : kGo(ScrTop+Size.Y);
                  kbHome : kGo(0);
                  kbEnd  : kGo(EndY);
                end;
    evBroadcast : if (Event.Command = Brc_ScrollBarChanged) and
                     (Event.InfoPtr = ScrollBar) then begin
                     Go(ScrollBar^.Value);
                     ClearEvent(Event);
                  end;
    evMouseMove : if Event.Buttons > 0 then begin
                    if OrigY = -1 then OrigY := Event.Where.Y else begin
                      Go(ScrTop-(Event.Where.Y-OrigY));
                      OrigY := Event.Where.Y;
                      ClearEvent(Event);
                      updateScroller;
                    end;
                    ClearEvent(Event);
                  end else origY := -1;
    evMouseUp   : origY := -1;
  end;
end;

procedure TTextViewer.Paint;
var
  line:string;
  curword:string;
  R:TRect;
  Pt:PMsgText;
  gap:integer;
  curline:longint;
  xs:integer;
  y:longint;
  prevendy:longint;
  c:char;
  lastcolor:byte;

  procedure PaintANSI;
  var
    escstr:FNameStr;
    x:integer;
    w:word;
    ox:integer;
    fontw:integer;
    escing:boolean;
    hebe:byte;
    code:byte;
    fc,bc:byte;
    oldbc:byte;
    bold:boolean;
    blink:boolean;
    newfc,newbc:byte;
    procedure sfs(b:byte);
    begin
      setFillStyle(solidFill,b);
    end;

    procedure ANSIOutLine;
    var
      xsz:word;
    begin
      if (y+gap >= 0) and (y <= Size.Y) then begin
        xsz := GetStringSize(setup.readerFont,line);
        XPrintStr(x,y,xsz,setup.readerFont,line);
        inc(x,xsz);
      end;
      line := '';
    end;

    procedure ANSICRLF;
    begin
      sfs(cblack);
      XBar(x,y,Size.X,y+gap-1);
      sfs(bc);
      inc(curline);
      x := 0;
      inc(y,gap);
      line := '';
    end;

    procedure setcolor(afc,abc:byte);
    begin
      fc := afc;
      bc := abc;
      SetTextColor(fc,bc);
      sfs(bc);
    end;

    procedure endesc;
    begin
      escstr := '';
      escing := false;
    end;

  begin
    setcolor(7,0);
    escstr := '';
    curline := 1;
    fontw := GetStringSize(setup.readerFont,'A');
    escing := false;
    bold := false;
    blink := false;
    x := 0;
    while Pt <> NIL do begin
      for w:=0 to Pt^.Size-1 do begin
        c := Pt^.Data[w];
        if escing then case c of
          #27 : escstr := '';
          '[' : ;
          'C' : begin
                  ansioutline;
                  sfs(cblack);
                  ox := (s2l(escstr)*fontw);
                  XBar(x,y,x+ox,y+gap-1);
                  sfs(bc);
                  inc(x,ox);
                  endesc;
                end;
          'm' : begin
                  ansioutline;
                  line := escstr;
                  newfc := fc;
                  newbc := bc;
                  {newbc := 0;}
                  for hebe := 1 to GetParseCount(line,';') do begin
                    code := s2l(getparse(line,';',hebe));
                    case code of
                      0 : begin
                            bold := false;
                            blink := false;
                            newfc := 7;
                            newbc := 0;
                          end;
                      1 : bold := true;
                      5 : blink := true;
                      22 : bold := false;
                      25 : blink := false;
                      30..37 : newfc := code-30;
                      40..47 : newbc := code-40;
                      else Debug('unknown color code: '+l2s(code));
                    end;
                  end;
                  if bold then newfc := newfc or 8
                          else newfc := newfc and not 8;
                  if blink then newbc := newbc or 8
                           else newbc := newbc and not 8;
                  setcolor(newfc,newbc);
                  line := '';
                  endesc;
                end;
          'B' : begin
                  ox := x;
                  ansioutline;
                  ansicrlf;
                  for hebe := 1 to s2l(escstr)-2 do ansicrlf;
                  sfs(cBlack);
                  XBar(0,y,ox,y+gap-1);
                  sfs(bc);
                  x := ox;
                  endesc;
                end;
          'H' : begin
                  ansioutline;
                  ansicrlf;
                  line := escstr;
                  ox := (s2l(getparse(line,';',2))-1)*GetStringSize(setup.readerFont,'A');
                  for hebe := curline to s2l(getparse(line,';',1))-1 do ansicrlf;
                  sfs(cBlack);
                  XBar(0,y,ox,y+gap-1);
                  sfs(bc);
                  x := ox;
                  line := '';
                  endesc;
                end;
          'p','l','h','K','J','f','A','D','s','u' : endesc;
          else escstr := escstr+c;
        end else case c of
          #27 : escing := true;
          #13,#10 : begin
                  ansioutline;
                  ansicrlf;
                end;
          #0..#31 :;
          else line := line + c;
        end; {case}
      end;
      Pt := Pt^.Next;
    end; {while}
    ansioutline;
    ansicrlf;
  end;

  procedure OutLine;
  var
    fc:byte;
    b:byte;
    x:integer;
    xsize:integer;
    temp:string;
    procedure doPrint;
    begin
      XPrintStr(0,y+2,Size.X+1,setup.readerFont,line);
    end;
  begin
    if pos(#1,line) > 0 then line := '';
    if lastcolor <> 255 then fc := lastcolor else begin
      temp := copy(line,1,6);
      if pos('... ',temp) = 1 then fc := readercolors[rcTagline].Color else
        if pos('>',temp) > 0 then fc := readercolors[rcQuote].Color else
          if (pos('--- ',temp) = 1) or (pos('~~~ ',line)=1) then fc := readercolors[rcTearline].Color else
            if pos(' * Orig',line) = 1 then fc := readercolors[rcOrigin].Color else fc := readercolors[rcNormalText].Color;
    end;
    if (y+gap >= 0) and (y <= Size.Y) then begin
      XBar(0,y,Size.X,y+1);
      {Strip(line);}
      SetTextColor(fc,readerColors[rcBackground].Color);
      lastcolor := fc;
      if line = '' then begin
        SetFillStyle(SolidFill,readerColors[rcBackground].Color);
        XBar(0,y+2,Size.X,y+2+setup.readerFontH);
      end else if Keyword = NIL then doPrint else begin
        b := pos(keyword^,upper(line));
        if b = 0 then doPrint else begin
          temp := copy(line,1,b-1);
          xsize := GetStringSize(setup.readerFont,temp);
          x    := 0;
          XPrintStr(x,y+2,xsize,setup.readerFont,temp);
          inc(x,xsize);
          SetTextColor(cYellow,cBlack);
          temp := copy(line,b,length(keyword^));
          xsize := GetStringSize(setup.readerFont,temp);
          XPrintStr(x,y+2,xsize,setup.readerFont,temp);
          inc(x,xsize);
          SetTextColor(fc,readerColors[rcBackground].Color);
          temp := copy(line,b+length(keyword^),255);
          XPrintStr(x,y+2,Size.X-x,setup.readerFont,temp);
          if keywhere = maxint then keywhere := y+2;
        end;
      end;
    end else if Keyword <> NIL then if keywhere = maxint then begin
      b := pos(keyword^,upper(line));
      if b > 0 then keywhere := y+2;
    end;
    inc(y,gap);
    line := '';
  end;

  procedure PaintNormal;
  var
    w:word;
  begin
    inc(gap,2);
    while Pt <> NIL do begin
      for w:=0 to Pt^.Size-1 do begin
        c := Pt^.Data[w];
        case c of
          #32,#13 : begin
                   xs := GetStringSize(setup.readerFont,line+curword);
                   if xs > Size.X-1 then begin
                     OutLine;
                     line := curword+#32;
                     curword := '';
                     if c = #13 then begin
                       OutLine;
                       lastcolor := 255;
                     end;
                   end else begin
                     line := line + curword + #32;
                     if c = #13 then begin
                       OutLine;
                       lastcolor := 255;
                     end;
                     curword := '';
                   end;
                 end;
          #9 : curword := curword + Duplicate(#32,length(curword) mod 8);
          #1,#33..#255 : begin
            inc(byte(curword[0]));
            curword[length(curword)] := c;
          end;
        end; {case}
      end; {for}
      Pt := Pt^.Next;
    end; {while}
    line := line + curword;
    OutLine;
  end;

begin
  lastcolor := 255;
  PaintBegin;
  prevendy:=endy;
  SetFillStyle(solidFill,readercolors[rcBackground].Color);
  Keywhere := maxint;
  gap := setup.readerFontH;
  curline := 0;
  line    := '';
  curword := '';
  y := -scrtop;
  Pt := Text;
  if Pt = NIL then PaintNormal else if Pt^.Data[0] = #27 then PaintANSI else PaintNormal;
  if y <= Size.Y then XBar(0,y,Size.X,Size.Y);
  EndY := y+ScrTop;
  PaintEnd;
  if prevendy = 0 then updateScroller;
end;

{- TMsgWindow -}
constructor TMsgWindow.Init;
const
  defbuttonsize : word = 60;
  willdisabled  : boolean = false;
var
  R:TRect;
  r1:trect;
  P:PView;
  a:PArea;
  butx,buty:integer;

  procedure putbut(amsg:FnameStr; acmd:word);
  var
    P:PButton;
    SubR:TRect;
  begin
    if butx + defbuttonsize > 500 then begin
      inc(buty,Top^.Size.Y+1);
      r.a.y := buty;
      butx := 5;
    end;
    SubR.Assign(0,0,defbuttonsize,21);
    SubR.Move(butx,buty);
    New(P,SizedInit(SubR,amsg,acmd));
    P^.Options := P^.Options and not Ocf_Selectable;
    P^.SetState(scf_Disabled,willdisabled);
    Insert(P);
    inc(butx,P^.Size.X+1);
  end;
begin
  R.Assign(0,0,500,300);
  a := amsg^.Area;
  if a = NIL then Abort('TMsgWindow.Init','AREANIL!!');
  if not setup.msgWindow.Empty then R := setup.msgWindow;
  inherited Init(R,a^.name);
  Options := (Options or Ocf_ReSize);
  if setup.MsgWindow.Empty then Options := Options or Ocf_Centered;
  NewMsg(amsg);
  GetVisibleBounds(R1);
  R1.Move(-r1.a.x,-r1.a.y);
  R1.Grow(-5,-5);
  R := R1;
  r.b.x := r.a.x + 100;
  r.b.y := r.a.y + 38;
  New(Status,Init(R));
  Insert(Status);

  R := R1;
  inc(r.a.x,Top^.Size.X+5);

  P := New(PMsgHeader,Init(R,fntProp));
  P^.GetBounds(R);
  Insert(P);

  r.b.x := r1.b.x;
  r.a.y := r.b.y+5;
  Insert(New(PAccelerator,Init(
    NewAcc(kbLeft,cmOwnerPrev,
    NewAcc(kbRight,cmOwnerNext,
    NewAcc(kbEnter,cmOwnerNext,
    NIL))))));

  butx := 5;
  buty := r.a.y;

  defButtonSize := 29;
  putbut('<<',cmOwnerPrev);
  defButtonSize := 30;
  putbut('>>',cmOwnerNext);
  defButtonSize := 60;
  {used keys: U,C,N,Y,K,F,T,I,P,Z,H,R,S,A}
  putbut(gtid(msMsgWindowReply),cmReply);
  putbut(gtid(msMsgWindowNetmail),cmNetmailReply);
  putbut(gtid(msMsgWindowOther),cmReplyOriginal);
  putbut(gtid(msMsgWindowNew),cmWriteMsg);
  putbut(gtid(msMsgWindowSave),cmSave);
  putbut(gtid(msMsgWindowForward),cmForward);
  putbut(gtid(msMsgWindowAdopt),cmAdoptTagline);
  putbut(gtid(msMsgWindowPentagon),cmAddressLookup);
  willdisabled := packet^.GetType = pakQWK;
  putbut(gtid(msMsgWindowLater),cmMarkMsg);
  willdisabled := false;
  putbut(gtid(msMsgWindowPrint),cmPrint);
  putbut(gtid(msMsgWindowUUDecode),cmUUDecode);
  putbut(gtid(msMsgWindowSearch),cmSearch);
  putbut(gtid(msMsgWindowReplies),cmShowReplies);
  putbut(gtid(msMsgWindowHelp),cmHelp);

  inc(r.a.y,Top^.Size.Y+5);
  r.b.y := r1.b.y;
  r.a.x := r.b.x-scrollerXSize;
  P := New(PScrollBar,Init(R));
  P^.GrowMode := gmFixedAll and not gmFixedLoX;
  Insert(P);
  r.b.x := r.a.x-5;
  r.a.x := 5;
  Viewer := New(PTextViewer,Init(R,Text,PScrollBar(P)));
  Insert(Viewer);
  MinSize := MinViewerWindowSize;
  HelpContext := hcMsgWindow;
end;

procedure TMsgWindow.NewMsg;
begin
  Lock;
  Msg := amsg;
  if Text <> NIL then DisposeText(Text);
  Text := Packet^.ReadMsgText(msg);
  if Viewer <> NIL then Viewer^.NewText(Text);
  if setup.Flags and ufPersBeep > 0 then if msg^.Flags and wmfPersonal > 0 then CikCik;
  if GetState(Scf_Exposed) then begin
    if Msg^.Area^.Name <> Header^ then begin
      DisposeStr(Header);
      Header := NewStr(Msg^.Area^.Name);
      Paint;
    end else PaintSubViews;
  end;
  UnLock;
end;

procedure TMsgWindow.MakeItRead;
begin
  if Msg^.Flags and wmfRead > 0 then exit;
  Msg^.Flags := Msg^.Flags or wmfRead;
  Packet^.UpdateMsgFlags(Msg);
  dec(Msg^.Area^.Unread);
  Message(Owner,evBroadcast,cmUnreadTotalChanged,Msg^.Area);
end;

destructor TMsgWindow.Done;
begin
  MakeItRead;
  DisposeText(Text);
  inherited Done;
end;

procedure TMsgWindow.ChangeBounds;
var
  newrect:boolean;
begin
  newrect := not R.Equals(setup.msgWindow);
  inherited ChangeBounds(R);
  if newrect then GetBounds(setup.msgWindow);
end;

procedure TMsgWindow.HandleEvent;
  procedure GoNext(anext:boolean);
  var
    acmd:word;
  begin
    EventWait;
    ClearEvent(Event);
    MakeItRead;
    if anext then acmd := cmOwnerNext else acmd := cmOwnerPrev;
    Message(Owner,evBroadcast,acmd,@Self);
  end;

  procedure AdoptTagline;
  var
    w:word;
    line:string;
    Pt:PmsgText;
  begin
    EventWait;
    line := '';
    Pt := Text;
    while Pt <> NIL do begin
      for w:=0 to Pt^.Size-1 do begin
        if Pt^.Data[w] = #13 then begin
          Strip(line);
          if pos('... ',line) = 1 then begin
            Adopt(line);
            exit;
          end else line := '';
        end else line := line + Pt^.Data[w];
      end;
      Pt := Pt^.Next;
    end;
    if pos('... ',line) = 1 then Adopt(line) else MessageBox(gtid(mstaglineNotFoundMsg),0,mfInfo);
  end;

  procedure ForwardMsg;
  var
    Pa:PArea;
  begin
    Pa := AreaChangeDialog(Msg^.Area,acNormal);
    if Pa = NIL then exit;
    WriteMsg('All',Msg^.Subj,'Forward',Pa,Msg,Viewer^.Text,wfForward);
  end;

  procedure PrintMsg;
  var
    line:string;
    w:word;
    c:char;
  begin
    BeginPrint;
    PrintMsgHeader(Msg);
    line := '';
    for w := 0 to Viewer^.Text^.Size-1 do begin
      c := Text^.Data[w];
      if c = #13 then begin
        WritePrn(line);
        line := '';
      end else line := line +c;
    end;
    if line <> '' then WritePrn(line);
    EndPrint;
  end;

  procedure ReplyIt(replyinanother:boolean);
  var
    aarea:PArea;
  begin
    if replyinanother then begin
      aarea := AreaChangeDialog(Msg^.Area,acHideSelf);
      if aarea = NIL then exit;
    end else aarea := Msg^.Area;
    WriteMsg(Msg^.From,Msg^.Subj,gtid(msReply),aarea,Msg,Viewer^.Text,wfQuote);
    PaintSubViews;
  end;

  procedure NetmailReply;
  var
    desiredArea:PArea;
  begin
    desiredArea := AreaChangeDialog(Msg^.Area,acNetmail);
    if desiredArea = NIL then exit;
    WriteMsg(Msg^.From,Msg^.Subj,'Netmail',desiredArea,Msg,Viewer^.Text,wfQuote+wfNetmail);
    PaintSubViews;
  end;

  procedure MarkMsg;
  var
    P:PMarkDialog;
    code:word;
    flag:word;
  begin
    New(P,Init);
    code := GSystem^.ExecView(P);
    Dispose(P,Done);
    flag := 0;
    case code of
      cmYes : flag := wmmReply;
      cmNo  : flag := wmmRead;
      cmOK  : flag := wmmSave;
      else exit;
    end;
    Msg^.Marks := Msg^.Marks xor flag;
    EventWait;
    Packet^.UpdateMsgFlags(Msg);
    Status^.PaintView;
  end;

  procedure Pentagon;
  var
    P:PPersonColl;
    Pp:PPerson;
    n:integer;
    code:word;
    found:boolean;
    function EditMe:boolean;
    var
      Pd:PPersonInputDialog;
    begin
      EditMe := false;
      New(Pd,Init('Pentagon'));
      Pd^.SetData(Pp^);
      code := GSystem^.ExecView(pd);
      if code = cmOK then begin
        EditMe := true;
        Pd^.GetData(Pp^);
      end;
      Dispose(Pd,Done);
    end;

    procedure NotifyOthers;
    begin
      Message(Owner,evBroadcast,cmAddressBookChanged,NIL);
    end;

  begin
    EventWait;
    StartJob(gtid(msPentagonSearchTxt));
    P := GetPersonList;
    found := false;
    for n:=0 to P^.Count-1 do begin
      Pp := P^.At(n);
      if Upper(Pp^.Name) = Upper(Msg^.From) then begin
        found := true;
        break;
      end;
    end;
    EndJob;
    if found then begin
      if EditMe then begin
        EventWait;
        WritePersonList(P);
      end;
    end else begin
      if ExecBox(gtid(msPentagonNotFoundMsg),
                 gtid(msPentagonNotFoundHdr),hcPersonNotFound,GetBlock(0,0,mnfHorizontal,
                 NewButton(gtid(msPentagonNotFoundYes),cmYes,
                 NewButton(gtid(msPentagonNotFoundNo),cmNo,
                 NIL)))) = cmYes then begin
        New(Pp);
        ClearBuf(Pp^,SizeOf(TPerson));
        Pp^.Name := Msg^.From;
        if Msg^.Area^.AreaType = watNetmail then Pp^.Netmail := Msg^.Addr
                                            else GetOrigin(Viewer^.Text,Pp^.Netmail);
        if EditMe then begin
          EventWait;
          P^.Insert(Pp);
          WritePersonList(P);
        end else Dispose(Pp);
      end;
    end;
    Dispose(P,Done);
    NotifyOthers;
  end;

  procedure UUDecode;
  var
    xtable      : array[0..255] of byte;
    outbuf      : array[0..255] of byte;
    line        : string;
    fn          : FnameStr;
    hebe        : FnameStr;
    T           : TDosStream;
    Pt          : PMsgText;
    Pc          : PChar;
    w           : word;
    tableBuilt  : boolean;
    foundHeader : boolean;
    len         : byte;
    n           : byte;
    out         : byte;
    decodeMode  : (UU,XX,MIME);
    function GetLine:boolean;
    var
      c:char;
      procedure NextCheck;
      begin
        if w = Pt^.Size then begin
          Pt := Pt^.Next;
          w := 0;
        end;
      end;
    begin
      GetLine := false;
      line := '';
      if Pt = NIL then exit;
      while (Pt <> NIL) do begin
        c := Pt^.Data[w];
        if c = #13 then begin
          inc(w);
          GetLine := true;
          NextCheck;
          exit;
        end else line := line + c;
        inc(w);
        NextCheck;
      end;
      GetLine := true;
    end;

    procedure BuildMIMETable;
    var
      b:byte;
    begin

    end;

    procedure BuildUUTable;
    var
      b:byte;
    begin
      Debug('building uuencode xtable');
      ClearBuf(xtable,SizeOf(xtable));
      for b:=32 to 32+64 do xtable[b] := b-32;
      xtable[byte('`')] := xtable[byte(' ')];
      xtable[byte('~')] := xtable[byte('^')];
    end;

    procedure BuildXXTable;
    var
      b:byte;
    begin
      Debug('building xxencode xtable');
      ClearBuf(xtable,SizeOf(xtable));
      xtable[byte('+')] := 0;
      xtable[byte('-')] := 1;
      for b:=byte('0') to byte('9') do xtable[b] := (b-byte('0'))+2;
      for b:=byte('A') to byte('Z') do xtable[b] := (b-byte('A'))+12;
      for b:=byte('a') to byte('z') do xtable[b] := (b-byte('a'))+38;
    end;

{    procedure BuildBASE64Table;
    var
      b:byte;
    begin
      Debug('building base64 xtable');
      ClearBuf(xtable,SizeOf(xtable));
      xtable[byte('=')] := 0;
      for b:=byte('A') to byte('Z') do xtable[b] := (b-byte('A'));
      for b:=byte('a') to byte('z') do xtable[b] := (b-byte('a'))+26;
      for b:=byte('0') to byte('9') do xtable[b] := (b-byte('0'))+52;
      xtable[byte('+')] := 62;
      xtable[byte('/')] := 63;
    end;}

    function ReadTable:boolean;
    var
      subloop:byte;
      b:byte;
    begin
      Debug('reading extra translation table');
      ReadTable := true;
      ClearBuf(xtable,SizeOf(xtable));
      for subloop := 0 to 1 do if GetLine then begin
        for b:=1 to length(line) do xtable[byte(line[b])] := (b-1)+(32*subloop);
      end else begin
        Debug('xtable read error');
        readTable := false;
        exit;
      end;
    end;

    procedure CheckXX;
    var
      b:byte;
    begin
{      if decodeMode = MIME then BuildBASE64Table else begin}
      for b:=1 to length(line) do if line[b] in ['a'..'z'] then begin
        Debug('xxencode detected');
        decodeMode := XX;
        BuildXXTable;
        exit;
      end;
      Debug('default mode');
      BuildUUTable;
{      end;}
    end;

  begin
    if Msg^.Area^.AreaType = watInfo then exit;
    EventWait;
    Debug('decode begin');
    decodeMode := UU;
    w := 0;
    Pt := Viewer^.Text;
    FoundHeader := false;
    tableBuilt  := false;
    while GetLine do begin
      Strip(line);
      if pos('table',line) = 1 then tableBuilt := ReadTable;
      if pos('begin ',line) = 1 then begin
        foundHeader := true;
        break;
      end;
{      if pos('CONTENT-TRANSFER-ENCODING',upper(line)) = 1 then begin
        if pos('BASE64',upper(line)) = 0 then continue;
        Debug('MIME detected');
        decodeMode := MIME;
        foundHeader := true;
        break;
      end;}
    end;
    if not foundHeader then begin
      MessageBox(gtid(msUUDecodeNotFoundMsg),0,0);
      Debug('header not found');
      exit;
    end;
    fn := XGetFileName(GetParse(line,#32,3));
    Strip(fn);
    if fn = '' then begin
      Debug('invalid file name');
      exit;
    end;
    fn := wkdir^ + fn;
    while true do begin
      if not InputBox('UUDecode','Dosya adi',hcUUDecodePrompt,fntPropSmall,fn,79) then exit;
      hebe := gtid(msUUDecodeExistsTxt);
      Replace(hebe,'%F',fn);
      if XFileExists(fn) then case ExecBox(hebe,gtid(msUUDecodeExistsHdr),0,GetBlock(0,0,mnfHorizontal,
                                         NewButton(gtid(msUUDecodeExistsYes),cmYes,
                                         NewButton(gtid(msUUDecodeExistsNewName),cmNo,
                                         NewButton(gtid(msUUDecodeExistsCancel),cmCancel,
                                         NIL))))) of
        cmYes : break;
        cmNo  : continue;
        else exit;
      end else break;
    end; {while}
    Debug('decoding '+fn);
    T.Init(fn,stCreate);
    if T.Status <> stoK then begin
      T.Done;
      Debug('creation error');
      exit;
    end;
    GetLine;
    if not tableBuilt then CheckXX;
    repeat
      if line = '' then continue;
      if line = 'end' then begin
        Debug('legal end of file');
        break;
      end;
      Pc := @line;
      inc(Pc);
      len := xtable[byte(Pc[0])];
      inc(word(Pc));
      n   := len;
      out := 0;
      while n > 0 do begin
        outbuf[out] := (xtable[byte(Pc[0])] shl 2) or (xtable[byte(Pc[1])] shr 4);
        inc(out);
        dec(n);
        if n > 0 then begin
          outbuf[out] := (xtable[byte(Pc[1])] shl 4) or (xtable[byte(Pc[2])] shr 2);
          inc(out);
          dec(n);
        end;
        if n > 0 then begin
          outbuf[out] := (xtable[byte(Pc[2])] shl 6) or (xtable[byte(Pc[3])]);
          inc(out);
          dec(n);
        end;
        inc(word(Pc),4);
      end;
      T.Write(outbuf,out);
    until not getline;
    T.Done;
  end;

  procedure SearchBakalim;
  var
    s:string;
  begin
    s := '';
    if InputBox(gtid(msInSearchhdr),gtid(msInSearchPrompt),hcTextSearchDialog,fntPropSmall,s,255) then Viewer^.DoSearch(s);
  end;

var
  temp:TMsg;
begin
  if Event.What = evKeyDown then if Event.KeyCode = kbAltR then begin
    ClearEvent(Event);
    ReplyIt(true);
    exit;
  end;
  inherited HandleEvent(Event);
  case Event.What of
    evBroadcast : if Event.Command = cmIsThatYou then if (Event.InfoPtr = Msg^.Area) or (Event.InfoPtr = NIL) then begin
                    ClearEvent(Event);
                    exit;
                  end;
    evKeyDown : begin
                  case Event.KeyCode of
                    kbEsc : begin
                      ClearEvent(Event);
                      Message(@Self,evCommand,cmClose,@Self);
                      exit;
                    end;
                    kbAltN : Message(@Self,evCommand,cmNetmailReply,NIL);
                    else case upcase(Event.CharCode) of
                      'R' : Message(@Self,evCommand,cmReply,NIL);
                      'E' : Message(@Self,evCommand,cmWriteMsg,NIL);
                      'S' : Message(@Self,evCommand,cmSave,NIL);
                      else exit;
                    end; {case}
                  end; {case}
                  ClearEvent(Event);
                end;
    evCommand : begin
                  case Event.Command of
                    cmReply : ReplyIt(false);
                    cmReplyOriginal : begin
                      Move(Msg^,temp,SizeOf(TMsg));
                      temp.From := temp.Too;
                      WriteMsg(Msg^.Too,Msg^.Subj,gtid(msReplyOriginal),Msg^.Area,@temp,Viewer^.Text,wfQuote);
                      PaintSubViews;
                    end;
                    cmWriteMsg : begin
                      WriteMsg('All','',gtid(msNewMsg),Msg^.Area,NIL,NIL,0);
                      PaintSubViews;
                    end;
                    cmMarkMsg : MarkMsg;
                    cmNetmailReply : NetmailReply;
                    cmForward : ForwardMsg;
                    cmSearch : SearchBakalim;
                    cmOwnerNext : begin
                      GoNext(True);
                      exit;
                    end;
                    cmOwnerPrev : begin
                      GoNext(False);
                      exit;
                    end;
                    cmAdoptTagline : AdoptTagline;
                    cmAddressLookup : Pentagon;
                    cmUUDecode : UUDecode;
                    cmShowReplies : Message(Owner,evBroadcast,cmReplyReadMode,NIL);
                    cmSave : SaveMsg(Msg,Viewer^.Text);
                    cmPrint : PrintMsg;
                    else exit;
                  end; {case}
                  ClearEvent(Event);
                end;
  end;
end;

{- TareaChangeDialog -}
constructor TAreaChangeDialog.Init;
var
  R:TRect;
  Ps:PScrollBar;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(msAreaChangehdr));
  Options := Options or Ocf_Centered;
  New(Lister,Init(5,5,13));
  Lister^.NewList(alist);
  Lister^.GetBounds(R);
  Insert(Lister);
  if alist^.Count > 10 then begin
    r.a.x := r.b.x+2;
    r.b.x := r.a.x+scrollerXSize;
    New(Ps,Init(R));
    Insert(Ps);
    Lister^.AssignScroller(Ps);
  end;
  Lister^.FocusItem(alist^.IndexOf(acurrent));
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msAreaChangeOK),cmOK,
    NewButton(gtid(msAreaChangeCancel),cmCancel,
    NewButton(gtid(msAreaChangeHelp),cmHelp,
    NIL)))));
  HelpContext := hcAreaChange;
  FitBounds;
end;

{- TReplyWindow -}
constructor TReplyWindow.Init;
const
  defbuttonsize : word = 60;
var
  R:TRect;
  r1:trect;
  P:PView;
  a:PArea;
  butx,buty:integer;
  procedure putbut(amsg:FnameStr; acmd:word);
  var
    P:PButton;
    SubR:TRect;
  begin
    if butx + defbuttonsize > 500 then begin
      inc(buty,Top^.Size.Y+1);
      r.a.y := buty;
      butx := 5;
    end;
    SubR.Assign(0,0,defbuttonsize,21);
    SubR.Move(butx,buty);
    New(P,SizedInit(SubR,amsg,acmd));
    P^.Options := P^.Options and not Ocf_Selectable;
    Insert(P);
    inc(butx,P^.Size.X+1);
  end;
begin
  R.Assign(0,0,500,300);
  a := areply^.Area;
  if a = NIL then Abort('TReplyWindow.Init','Viva la sewage! area is NIL');
  inherited Init(R,a^.Name);
  Options := (Options or Ocf_ReSize or Ocf_Centered);
  NewMsg(areply);
  GetVisibleBounds(R1);
  R1.Move(-r1.a.x,-r1.a.y);
  R1.Grow(-5,-5);
  R := R1;
  P := New(PReplyHeader,Init(R,fntProp));
  P^.GetBounds(R);
  Insert(P);

  r.b.x := r1.b.x;
  r.a.y := r.b.y+5;
  Insert(New(PAccelerator,Init(
    NewAcc(kbLeft,cmReplyPrev,
    NewAcc(kbRight,cmReplyNext,
    NewAcc(kbEnter,cmReplyNext,
    NewAcc(kbDel,cmOwnerDel,
    NIL)))))));
  butx := 5;
  buty := r.a.y;
  defbuttonsize := 30;
  putbut('<<',cmReplyPrev);
  putbut('>>',cmReplyNext);
  defbuttonsize := 60;
  putbut(gtid(msRepWindowSave),cmSave);
  putbut(gtid(msRepWindowCrosspost),cmCrosspost);
  putbut(gtid(msRepWindowEditHeader),cmEditHeader);
  putbut(gtid(msRepWindowEditMsg),cmEditRep);
  putbut(gtid(msRepWindowDelete),cmOwnerDel);
  putbut(gtid(msRepWindowPrint),cmPrint);
  defbuttonsize := 30;
  putbut(gtid(msRepWindowHelp),cmHelp);

  inc(r.a.y,Top^.Size.Y+5);
  r.b.y := r1.b.y;
  r.a.x := r.b.x-scrollerXSize;
  P := New(PScrollBar,Init(R));
  P^.GrowMode := gmFixedAll and not gmFixedLoX;
  Insert(P);
  r.b.x := r.a.x-5;
  r.a.x := 5;
  Viewer := New(PTextViewer,Init(R,Text,PScrollBar(P)));
  Insert(Viewer);
  MinSize := MinViewerWindowSize;
  HelpContext := hcReplyWindow;
end;

procedure TReplyWindow.NewMsg;
begin
  Lock;
  Reply := amsg;
  if Text <> NIL then DisposeText(Text);
  Text := ReplyPacket^.ReadMsgText(reply);
  if Viewer <> NIL then Viewer^.NewText(Text);
  if GetState(Scf_Exposed) then begin
    if Reply^.Area^.name <> Header^ then begin
      DisposeStr(Header);
      Header := NewStr(Reply^.Area^.name );
      Paint;
    end else PaintSubViews;
  end;
  UnLock;
end;

procedure TReplyWindow.HandleEvent;
  procedure GoNext(anext:boolean);
  var
    acmd:word;
  begin
    EventWait;
    if anext then acmd := cmReplyNext else acmd := cmReplyPrev;
    Message(Owner,evBroadcast,acmd,@Self);
  end;

  procedure EditHeader;
  var
    P:PMsgDialog;
    msgrec:TMsg;
    code:word;
  begin
    Move(Reply^,msgrec,SizeOf(TMsg));
    New(P,Init(gtid(msRepEditHdr),false));
    P^.Update(Reply^.area,PMsg(Reply));
    code := GSystem^.ExecView(P);
    if code = cmOK then begin
      EventWait;
      P^.GetInfo;
      Reply^.area := P^.Area;
      Reply^.From := P^.Msg^.From;
      Reply^.Too  := P^.Msg^.Too;
      Reply^.Subj := P^.Msg^.Subj;
      Reply^.Addr := P^.Msg^.Addr;
      DisposeStr(Header);
      Header := NewStr(Reply^.Area^.Name);
      ReplyPacket^.WriteMsgHeader(Reply,false);
      Message(Owner,evBroadcast,cmRefreshReplies,NIL);
      PaintView;
    end else Move(msgrec,Reply^,SizeOf(msgrec));
    Dispose(P,Done);
  end;

  procedure EditRep;
  var
    fn:FNameStr;
  begin
    fn := wkdir^+tempFile;
    ReplyPacket^.MsgToFile(Reply,fn);
    if EditFile(wkdir^+tempFile) then begin
      AddTearline(fn);
      ReplyPacket^.FileToMsg(fn,Reply);
      Message(Owner,evBroadcast,cmRefreshReplies,NIL);
    end else XDeleteFile(fn);
  end;

  procedure PrintMsg;
  var
    T:TDosStream;
    line:string;
  begin
    EventWait;
    Packet^.MsgToFile(Reply,wkdir^+tempFile);
    T.Init(wkdir^+tempFile,stOpenRead);
    if T.Status <> stOK then begin
      T.Done;
      FileErrorBox(wkdir^+tempFile);
      exit;
    end;
    BeginPrint;
    PrintMsgHeader(PMsg(Reply));
    while T.GetPos < T.GetSize do begin
      if PrnAbort then break;
      SReadln(T,line);
      WritePrn(line);
    end;
    T.Done;
  end;

  procedure SaveRep;
  begin
    SaveMsg(PMsg(Reply),Viewer^.Text);
  end;

  procedure Crosspost;
  var
    P:PArea;
  begin
    P := AreaChangeDialog(Reply^.Area,acHideSelf);
    if P = NIL then exit;
    WriteMsg(Reply^.Too,Reply^.Subj,'Crosspost',P,PMsg(Reply),Text,wfCrosspost);
  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand : case Event.Command of
                  cmReplyNext,cmReplyPrev : begin
                    ClearEvent(Event);
                    Message(Owner,evBroadcast,Event.Command,@Self);
                    exit;
                  end;
                  cmOwnerDel : Message(Owner,evBroadcast,cmOwnerDel,@Self);
                  cmEditRep  : EditRep;
                  cmEditHeader : EditHeader;
                  cmPrint    : PrintMsg;
                  cmCrosspost : CrossPost;
                  cmSave     : SaveRep;
                  else exit;
                end;
    evKeyDown : case Event.CharCode of
                  'S' : SaveRep;
                  'E' : EditRep;
                  else case Event.KeyCode of
                    kbEsc : begin
                      ClearEvent(Event);
                      Message(@Self,evCommand,cmClose,NIL);
                      exit;
                    end;
                    else exit;
                  end; {case}
                end; {case}
    evBroadcast : case Event.Command of
                    cmAreYouReplier : ;
                    cmIsThatYou : if Event.InfoPtr <> NIL then exit;
                    else exit;
                  end;
    else exit;
  end; {case}
  ClearEvent(Event);
end;

{- TareaListWindow -}
constructor TAreaListWindow.Init;
var
  R:TRect;
  s:string;
  stax:integer;
  hb:PVIFButton;
  function GetFullList:PAreaColl;
  begin
    GetFullList := FilterAreaList(NonEmptyFilter);
  end;

  function GetEmptyList:PAreaColl;
  begin
    GetEmptyList := FilterAreaList(EmptyFilter);
  end;

  procedure PutIt(amsg:FnameStr; acmd:word; realonly:boolean);
  var
    pb:PButton;
  begin
    New(Pb,SizedInit(R,amsg,acmd));
    Pb^.Options := Pb^.Options and not Ocf_Selectable;
    Pb^.GetBounds(R);
    R.Move(0,Pb^.Size.Y+3);
    if realonly and (Packet^.msgList^.Count=0) then Pb^.SetState(scf_Disabled,True);
    Insert(Pb);
  end;

  procedure PutLabel(amsg:FnameStr; agid:word);
  var
    P:PLabel;
  begin
    New(P,FullInit(stax,r.a.y+4,amsg,cRed,COl_Back,fntPropSmall));
    P^.SetGroupId(agid);
    inc(r.a.y,P^.Size.Y+11);
    Insert(P);
  end;

  procedure PutScrollerFor(alist:PAreaLister; agid:word);
  var
    ps:PScrollBar;
  begin
    alist^.GetBounds(R);
    alist^.GetBounds(R);
    R.A.X := R.B.X+2;
    R.B.X := R.A.X + scrollerXSize;
    ps := New(PScrollBar,Init(R));
    Insert(alist);
    PS^.SetGroupId(agid);
    Ps^.GetBounds(R);
    Insert(ps);
    alist^.AssignScroller(ps);
  end;

  procedure MyLittleProc(P:PView);far;
  begin
    P^.SetState(Scf_Visible,False);
  end;

  procedure donot(peewee:pview);
  begin
    peewee^.Options := peewee^.Options and not Ocf_Selectable;
  end;

const
  areaListRowCount = 15;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,atitle);
  Lock;
  Options := Options and not Ocf_CenterY;
  r.a.x := 5;
  r.a.y := 5;
  hb := New(PVIFButton,Init(5,5,idAreaFull,cmActivateEmpty));
  donot(hb);
  hb^.SetGroupId(grp_AreaFull);
  Insert(hb);
  hb := New(PVIFButton,Init(5,5,idAreaEmpty,cmActivateFull));
  donot(hb);
  hb^.SetGroupId(grp_AreaEmpty);
  Insert(hb);
  hb := New(PVIFButton,Init(5+hb^.Size.X+5,5,idAreaLink,cmInterest));
  donot(hb);
  hb^.GetBounds(R);
  Insert(hb);
  stax := r.b.x+5;

  PutLabel(gtid(msFullAreas),grp_AreaFull);
  New(FullList,Init(5,r.a.y,AreaListRowCount));
  FullList^.NewList(GetFullList);
  FullList^.SetGroupId(grp_AreaFull);
  PutScrollerFor(FullList,grp_AreaFull);

  r.a.y := 5;

  PutLabel(gtid(msEmptyAreas),grp_AreaEmpty);
  New(EmptyList,Init(5,r.a.y,AreaListRowCount));
  EmptyList^.newList(getEmptyList);
  EmptyList^.SetGroupId(grp_AreaEmpty);
  PutScrollerFor(EmptyList,grp_AreaEmpty);

  GrpForEach(byte(Packet^.msgList^.Count > 0)+1,@MyLittleProc);

  r.a.x := r.b.x + 5;
  r.b.x := r.a.x+100;
  r.b.y := r.a.y+GetFontHeight(fntPropSmall)+10;
  r.Move(0,-14);
  PutIt(gtid(msALWPersonal),cmPersonal,true);   {m}
  PutIt(gtid(msALWUnread),cmUnread,true);         {o}
  PutIt(gtid(msALWForgotten),cmForgottenRealms,true); {u}
  PutIt(gtid(msALWWilLReply),cmWillReply,true);   {c}
  PutIt(gtid(msALWWillread),cmWillRead,true);        {r}
  PutIt(gtid(msALWWriteMsg),cmWriteMsg,false);         {y}
  PutIt(gtid(msALWSearchMsg),cmSearch,true);            {a}
  PutIt(gtid(msALWHelp),cmHelp,false);            {n}
  PutIt(gtid(msALWClose),cmClose,false);          {k}
  HelpContext := hcAreaListWindow;
  FitBounds;
  GetBounds(R);
  r.a.y := 20;
  r.b.y := r.a.y+Size.Y-1;
  ChangeBounds(R);
  MaxSize := Size;
  UnLock;
end;

function CompareMsg(Pm:PMsg):boolean;
var
  Pt,PPt:PMsgText;
  w:word;
  temp:word;
begin
  CompareMsg := false;
  with srchRec do if IsThatMsg(Pm^,srchRec) then begin
    if Text <> '' then begin
      Pt := Packet^.ReadMsgText(Pm);
      PPt := Pt;
      while PPt <> NIL do begin
        for w:=0 to PPt^.Size-1 do PPt^.Data[w] := upcase(PPt^.Data[w]);
        if SearchBuf(PPt^.Data^,Text[1],PPt^.Size,length(Text),temp) then begin
          CompareMsg := true;
          break;
        end;
        PPt := PPt^.Next;
      end;
      DisposeText(Pt);
    end else CompareMsg := true;
  end; {case}
end;

procedure TAreaListWindow.HandleEvent;
  procedure Search;
  var
    P:PSearchDialog;
    code:word;
    bufSize:word;
    Pt,PPt:PMsgText;
    n:integer;
    temp:word;
    w:word;
    Pm:PMsg;
    Pw:PMsgListWindow;
    ftiList:PDumbCollection;
    addthis:boolean;
{    procedure Check(ass:fnameStr);
    begin
      addthis := pos(rec.Pattern,upper(ass)) > 0;
    end;}
  begin
    New(P,Init('Mesaj Ara'));
    P^.SetData(setup.lastSearch);
    code := GSystem^.ExecView(P);
    if code = cmOK then P^.GetData(srchRec);
    Dispose(P,Done);
    if (code <> cmOK) then exit;
    EventWait;
    New(ftiList,Init(10,10));
    for n:=0 to Packet^.msgList^.Count-1 do begin
      Pm := packet^.msgList^.At(n);
      if CompareMsg(Pm) then ftilist^.Insert(Pm);
    end;
    if ftiList^.Count = 0 then begin
      Dispose(ftiList,Done);
      ExecBox(gtid(msMsgNotFoundTxt),gtid(msMsgNotFoundHdr),0,GetBlock(0,0,mnfHorizontal,
              NewButton(gtid(msMsgnotFoundBtn),cmOK,NIL)));
    end else GSystem^.Insert(New(PMsgListWindow,Init(gtid(msMsgFoundhDr),ftiList,cmAreYoumsgList)));
  end;

  function AdjustArea(aarea:PArea):boolean;
  var
    P:PAreaAdjustDialog;
    code:word;
  begin
    AdjustArea := false;
    if (aarea^.areaType in [watInfo,watArchive]) then exit;
    if ReplyPacket^.Config and mpcCanOLC > 0 then begin
      New(P,Init(aarea^.Name));
      P^.SetData(aarea^.UserStat);
      code := GSystem^.ExecView(P);
      if code = cmOK then begin
        if Regged then begin
          P^.GetData(aarea^.UserStat);
          EventWait;
          ReplyPacket^.OLC(Packet^.areaList);
        end else UnregDialog;
      end;
      Dispose(P,Done);
      AdjustArea := true;
    end else ExecBox(gtid(msNoOLCSupportTxt), gtid(msNoOLCSupportHdr),hcNoOLCSupport,
                     GetBlock(0,0,mnfHorizontal,
                     NewButton(gtid(msNoOLCSupportOK),cmOK,
                     NewButton(gtid(msNoOLCSupportHelp),cmHelp,
                     NIL))));
  end;

  function GetFocusedLister:PAreaLister;
  begin
    if FullList^.GetState(Scf_Focused) then GetFocusedLister := FullList else GetFocusedLister := EmptyList;
  end;

  procedure GetInterested;
  var
    Pl:PAreaLister;
  begin
    Pl := GetFocusedLister;
    if Pl^.ItemList^.Count = 0 then exit;
    if AdjustArea(Pl^.ItemList^.At(Pl^.FocusedItem)) then Pl^.Paint;
  end;

  procedure DoWrite;
  var
    Pl:PAreaLister;
    P:PArea;
  begin
    Pl := GetFocusedLister;
    if Pl^.ItemList^.Count = 0 then exit;
    P := Pl^.ItemList^.At(Pl^.FocusedItem);
    if P^.UserStat = wusNone then case ExecBox(^C'Bu alani normalde okumuyorsunuz'#13+
              ^C'Mesaj yazmakta kararli misiniz?',P^.Name,hcNoDLAreaWarn,GetBlock(0,0,mnfHorizontal,
              NewButton('~Kararliyim',cmOK,
              NewButton('~Bu alani okumak istiyorum',cmYes,
              NewButton('~Kem kum?',cmCancel,
              NIL))))) of
      cmOK : ;
      cmYes : if not AdjustArea(P) then exit else Pl^.Paint;
      else exit;
    end; {case}
    WriteMsg('All','',gtid(msNewMsg),P,NIL,NIL,0);
  end;

  procedure OpenBakiim;
  var
    P:PArea;
  begin
    if FullList^.GetState(Scf_Visible) then begin
      if FullList^.ItemList^.Count = 0 then exit;
      P := FullList^.ItemList^.At(FullList^.FocusedItem);
      OpenArea(P);
    end else DoWrite;
  end;

  procedure Illusion(willshow,willhide:word);
    procedure ShowProc(P:PView);far;
    begin
      P^.SetState(Scf_Visible,True);
    end;
    procedure HideProc(P:PView);far;
    begin
      P^.SetState(Scf_Visible,False);
    end;
  begin
    GrpForEach(willshow,@ShowProc);
    GrpForEach(willhide,@HideProc);
  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand : case Event.Command of
                  cmOpen : OpenBakiim;
                  cmSearch : Search;
                  cmWriteMsg : DoWrite;
                  cmPersonal : FindPersonal(true);
                  cmUnread   : FindUnread;
                  cmForgottenRealms : FindForgotten(true);
                  cmActivateFull : Illusion(grp_AreaFull,grp_AreaEmpty);
                  cmActivateEmpty : Illusion(grp_AreaEmpty,grp_AreaFull);
                  cmWillReply : FindWillReply;
                  cmWillRead  : FindWillRead;
                  cmInterest  : GetInterested;
                  else exit;
                end; {case}
    evBroadcast : case Event.Command of
                    cmCloseActivePacket : begin
                      ClearEvent(Event);
                      Message(@Self,evCommand,cmClose,NIL);
                      exit;
                    end;
                    cmUnreadTotalChanged : if FullList^.GetState(Scf_Visible) then FullList^.PaintView;
                    else exit;
                  end;
    evKeyDown : case Event.KeyCode of
                  kbTab,kbLeft,kbRight,kbShiftTab : if FullList^.GetState(Scf_Focused)
                    then Illusion(grp_areaempty,grp_areafull)
                    else Illusion(grp_areafull,grp_areaempty);
                  kbSpace : GetInterested;
                  else case Event.CharCode of
                    '1' : Illusion(grp_areafull,grp_areaempty);
                    '2' : Illusion(grp_areaempty,grp_areafull);
                    else exit;
                  end; {case}
                end; {Case}
    else exit;
  end;
  ClearEvent(Event);
end;

function TAreaListWindow.Valid;

  procedure UpdateMarks;
  var
    n:integer;
    Pm:PMsg;
  begin
    EventWait;
    for n:=0 to Packet^.msgList^.Count-1 do begin
      Pm := Packet^.msgList^.At(n);
      if Pm^.Flags and wmfRead = 0 then begin
        Pm^.Flags := Pm^.Flags or wmfRead;
        Packet^.UpdateMsgFlags(Pm);
      end;
    end;
  end;

  procedure NormalClosePacket;
  var
    dirinfo:SearchRec;
    P:PMsgListWindow;
    unread:integer;
    olddate:longint;
    warnmsg:string;
    procedure SmartRename;
    var
      ext:extstr;
      dest:FnameStr;
      counter:word;
      function isnum:boolean;
      var
        b:byte;
      begin
        isnum := ext[2] in ['0'..'9'];
      end;
    begin
      ext := XGetFileExt(currentPacketName);
      if not isnum then begin
        counter := 0;
        while XFileExists(ReplaceExt(currentPacketName,'.'+z2s(counter,3))) do begin
          inc(counter);
          if counter = 0 then begin
            Debug('uh oh... no more exts');
            exit;
          end;
        end;
        dest := ReplaceExt(currentPacketName,'.'+z2s(counter,3));
        XRenameFile(currentPacketName,dest);
        currentPacketName := dest;
        Debug('smart renaming: '+dest);
      end;
    end;
  begin
    EventWait;
    if setup.Flags and ufUnreadReminder > 0 then if Packet^.msgList^.Count > 0 then begin
      unread := Packet^.GetSpecificCount(@IsUnread);
      warnmsg := gtid(msUnreadWarnMsg);
      Replace(warnmsg,'%U',l2s(unread));
      if unread > 0 then case ExecBox(warnmsg,gtid(msUnreadWarnHdr),hcUnreadDialog,GetBlock(0,0,mnfHorizontal,
        NewButton(gtid(msUnreadWarnCancel),cmClose,
        NewButton(gtid(msUnreadWarnOK),cmYes,
        NewButton(gtid(msUnreadWarnIgnore),cmNo,
        NewButton(gtid(msUnreadWarnMark),cmOK,
        NewButton(gtid(msUnreadWarnHelp),cmHelp,NIL))))))) of
        cmYes : begin
                  StartJob(gtid(msUnreadFindMsg));
                  FindUnread;
                  EndJob;
                  Valid := false;
                  exit;
                end;
        cmOK : UpdateMarks;
        cmClose,cmCancel : begin
                    Valid := false;
                    exit;
                  end;
        cmNo : if unread > 300 then StartJob(gtid(msUnreadLiarMsg));
      end;
    end;
    repeat
      P := Message(Owner,evBroadcast,cmIsThatYou,NIL);
      if P <> NIL then Dispose(P,Done);
    until P = NIL;
    EventWait;
    SaveExtraPacketInfo;
    if (Packet^.msgList^.Count > 0) then begin
      olddate := XGetFileDate(currentpacketName);
      with Packet^ do PackArchive(currentPacketName,Where+Packetid+'.XTI');
      XSetFileDate(currentpacketName,olddate);
      SmartRename;
    end;
    FindFirst(ReplyPacket^.Where+'*.*',archive,dirinfo);
    if DosError = 0 then PackReplies;
    Lock;
    Dispose(EmptyList,Done);
    Dispose(FullList,Done);
    ClosePacket;
    Message(Owner,evBroadcast,cmUpdatePacketList,NIL);
    EndJob;
  end;

begin
  Valid := true;
  if (acmd = cmQuit) or (acmd=cmClose) then NormalClosePacket;
end;

{- TMsgLister -}
constructor TMsgLister.Init;
begin
  if amax > 20 then amax := 20;
  inherited Init(5,5,fntProp,amax,
    NewColumn(gtid(msMsgListFrom),120,0,
    NewColumn(gtid(msMsgListTo),120,0,
    NewColumn(gtid(msMsgListSubj),160,0,
    NIL))));
end;

function TMsgLister.GetColor;
var
  P:PMsg;
begin
  P := ItemList^.At(item);
  if P^.Flags and wmfRead > 0 then GetColor := cDarkGray else
    if P^.Flags and wmfPersonal > 0 then GetColor := cRed else
      GetColor := cBlack;
end;

function TMsgLister.GetText;
var
  P:PMsg;
begin
  P := ItemList^.At(item);
  GetText := P^.From+'|'+P^.Too+'|'+P^.Subj;
end;

procedure TMsgLister.ItemDoubleClicked;
begin
  Message(Owner,evCommand,cmShowMsg,NIL);
end;

{- TMsgListWindow -}
constructor TMsgListWindow.Init;
var
  R:TRect;
  ps:PScrollBar;
begin
  R.Assign(0,0,0,0);
  inherited Init(r,atitle);
  Options := Options or Ocf_Centered;
  New(Lister,Init(alist^.Count));
  Lister^.NewList(alist);
  if alist^.count > 20 then begin
    Lister^.GetBounds(R);
    R.A.X := r.b.x+2;
    R.b.x := r.a.x+scrollerXSize;
    New(Ps,Init(R));
    Insert(Ps);
    Lister^.AssignScroller(ps);
  end;
  Insert(Lister);
  FitBounds;
  MaxSize := Size;
end;

procedure TMsgListWindow.HandleEvent;
  procedure ShowMsg(aitem:integer);
  var
    Pm:PMsgWindow;
    Msg:PMsg;
  begin
    Msg := Lister^.ItemList^.At(aitem);
    if Msg = NIL then exit;
    Pm := Message(Owner,evBroadcast,cmIsThatYou,Msg^.Area);
    if Pm = NIL then begin
      New(Pm,init(Msg));
      GSystem^.Insert(Pm);
    end else begin
      if Pm^.Msg <> Msg then Pm^.NewMsg(Msg);
      Pm^.Select;
    end;
  end;

  function GoGo(aitem:integer):boolean;
  var
    Pm:PMsgWindow;
  begin
    GoGo := false;
    Pm := Event.infoPtr;
    if Pm^.Msg <> Lister^.ItemList^.At(Lister^.FocusedItem) then exit;
    Lister^.FocusItem(aitem);
    if Lister^.FocusedItem = aitem then Pm^.NewMsg(Lister^.ItemList^.At(aitem))
                                   else Message(Pm,evCommand,cmClose,Pm);
    GoGo := true;
  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evBroadcast: case Event.Command of
                   cmIsThatYou : if (Event.InfoPtr <> NIL) then exit;
                   cmOwnerNext : if not GoGo(Lister^.FocusedItem+1) then exit;
                   cmOwnerPrev : if not GoGo(Lister^.FOcusedItem-1) then exit;
                   else if Event.Command <> IdCmd then exit;
                 end; {case}
    evCommand : case Event.Command of
                  cmShowMsg   : ShowMsg(Lister^.FocusedItem);
                  else exit;
                end; {case}
    evKeyDown : case Event.KeyCOde of
                  kbLeft,kbRight : begin
                    ClearEvent(Event);
                    Message(@Self,evCommand,cmClose,NIL);
                    exit;
                  end;
                  else exit;
                end; {case}
    else exit;
  end; {case}
  ClearEvent(Event);
end;

{- TMsgDialog -}
constructor TMsgDialog.Init;
var
  s:string;
  R:TRect;
  willbeselected:PView;
  y:integer;
  definputsize:integer;
  procedure putinput(aprompt:FnameStr; reallen:byte; selectable:boolean);
  var
    P:PInputLine;
  begin
    Insert(new(PLabel,Init(5,y+1,aprompt,fntPropSmall)));
    New(P,Init(50,y,definputsize-50,fntProp,reallen));
    if not selectable then P^.SetState(Scf_Disabled,True);
    Insert(P);
    inc(y,P^.Size.Y+3);
  end;

begin
  R.Assign(0,0,0,0);
  inherited Init(R,ahdr);
  Options := (Options or Ocf_Centered) and not Ocf_ZoomEffect;
  y := 5;
  s := GetBlock(5,y,mnfHorizontal,
    NewButton(gtid(msMsgDialogOK),cmOk,
    NewButton(gtid(msMsgDialogCancel),cmCancel,
    NewButton(gtid(msMsgDialogFilter),cmFilterLookup,
    NewButton(gtid(msMsgDialogAddrLookup),cmAddrLookup,
    NewButton(gtid(msMsgDialogAreaChange),cmAreaChange,
    NewButton(gtid(msMsgDialogNetFlags),cmNetmailFlags,
    NIL)))))));
  GetBlockBounds(s,R);
  definputsize := r.b.x-r.a.x;
  InsertBlock(s);
  N := PButton(Top);
  inc(y,Top^.Size.Y+5);
  putinput(gtid(msMsgHeaderArea),49,false);
  putinput(gtid(msMsgHeaderFrom),35,false);
  putinput(gtid(msMsgHeaderTo),35,true);
  willbeselected := Top;
  putinput(gtid(msMSgHeaderSubj),71,true);
  putinput(gtid(msMsgHeaderAddr),25,false);
  Addr := PInputLine(Top);
  Addr^.ValidChars := ['0'..'9',':','/','.'];
{  R.Move(0,y);

  InsertBlock(s);}
  N^.SetState(Scf_Disabled,not anetmail);
  willbeselected^.Select;
  FitBounds;
  Filter := setup.FilterCmd;
  SetConfig(wfNetmail,anetmail);
end;

procedure TMsgDialog.HandleEvent;
  procedure ChangeArea;
  var
    Pa:PArea;
  begin
    Pa := AreaChangeDialog(Area,acHideSelf or (byte(GetConfig(wfNetmail))*acNetmail));
    if Pa = NIL then exit;
    Area := Pa;
    Update(Area,Msg);
  end;

  procedure ChangeNetFlags;
  var
    P:PNetFlagChangeDialog;
    code:word;
  begin
    New(P,Init);
    P^.SetData(Msg^.Netflags);
    code := GSystem^.ExecView(P);
    if code = cmOK then P^.GetData(Msg^.Netflags);
    Dispose(P,Done);
  end;

  procedure FilterLookup;
  var
    P:PFilterLookupDialog;
    code:word;
  begin
    New(P,Init(gtid(msFltHeader)));
    P^.SetData(setup.FilterCmd);
    code := GSystem^.ExecView(P);
    if code = cmOK then begin
      EventWait;
      P^.GetData(Filter);
    end;
    Dispose(P,Done);
  end;

  procedure AddrLookup;
  var
    P:PPerson;
    Pc:PPersonColl;
    n:integer;
    scr:TReplyScr;
    key:string;
  begin
    if Regged then begin
      if Area^.AreaType <> watNetmail then exit;
      EventWait;
      Pc := GetPersonList;
      if Pc <> NIL then begin
        GetData(scr);
        key := scr.Too;
        Strip(key);
        FastUpper(key);
        for n:=0 to Pc^.Count-1 do begin
          P := Pc^.At(n);
          if Upper(P^.Name) = key then begin
            scr.Addr := Addr2Str(P^.Netmail);
            SetData(scr);
            PaintView;
            break;
          end;
        end;
      end;
      Dispose(pc,Done);
    end else UnregDialog;
  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand : case Event.Command of
                  cmAreaChange : ChangeArea;
                  cmNetmailFlags : ChangeNetFlags;
                  cmFilterLookup : FilterLookup;
                  cmAddrLookup : AddrLookup;
                  else exit;
                end;
    else exit;
  end; {case}
  ClearEvent(Event);
end;

procedure TMsgDialog.Update(aarea:PArea; amsg:PMsg);
var
  scr:TReplyScr;
  net:boolean;
begin
  Lock;
  Area := aarea;
  Msg  := amsg;
  with scr do begin
    From := Msg^.From;
    Too  := Msg^.Too;
    Subj := Msg^.Subj;
  end;
  Scr.Area := Area^.Name;
  net := Area^.AreaType = watNetmail;
  Addr^.SetState(Scf_Disabled,not net);
  N^.SetState(Scf_Disabled,not net);
  if net then scr.Addr := Addr2Str(Msg^.Addr)
         else scr.Addr := '';
  SetData(scr);
  UnLock;
end;

procedure TMsgDialog.GetInfo;
var
  scr:TReplyScr;
begin
  GetData(scr);
  with Msg^ do begin
    From := scr.From;
    Too  := scr.Too;
    Subj := scr.Subj;
    if scr.Addr <> '' then begin
      ClearBuf(Origin,SizeOf(Origin)); {msg's origin}
      Str2Addr(scr.Addr,Addr);
    end;
  end;
end;

{- TTaglineLister -}
procedure TTaglineLister.ItemDoubleClicked;
begin
  Message(Owner,evCommand,cmOK,NIL);
end;

{- TTaglineInputDialog -}
constructor TTaglineInputDialog.Init;
var
  R:TRect;
  procedure putview(av:PView);
  begin
    av^.GetBounds(R);
    Insert(av);
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(msTaglineInputDialogHdr));
  Options := Options or Ocf_Centered;
  putview(New(PLabel,Init(5,5,gtid(msTaglineInputDialogPrompt),fntPropSmall)));
  putview(New(PInputLine,Init(r.b.x+5,5,100,fntPropSmall,76)));
  if savebox then putview(New(PSingleCheckBox,Init(5,r.b.y+5,fntPropSmall,gtid(msTaglineInputDialogCheck))));
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal,
    NewButton(gtid(msTaglineInputDialogOK),cmOK,
    NewButton(gtid(msTaglineInputDialogCancel),cmCancel,
    NIL))));
  SelectNext(True);
  HelpContext := hcTaglineInputDialog;
  FitBounds;
end;

{- TtaglineSelectDialog -}
constructor TTaglineSelectDialog.Init;
var
  R:TRect;
  Ps:PScrollBar;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(msTaglineSelectDialogHdr));
  Options := Options or Ocf_Centered;
  R.Assign(0,0,450,150);
  R.Move(5,5);
  New(lister,Init(R,fntProp));
  Lister^.newList(alist);
  Lister^.GetBounds(R);
  r.a.x := r.b.x + 5;
  r.b.x := r.a.x + scrollerXSize;
  New(Ps,Init(R));
  Insert(Ps);
  Insert(lister);
  lister^.AssignScroller(ps);
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msTaglineSelect),cmOK,
    NewButton(gtid(msTaglineRandom),cmRandom,
    NewButton(gtid(msTaglineNew),cmManualEntry,
    NewButton(gtid(msTaglineGoAhead),cmWhatever,
    NewButton(gtid(msTaglineCancel),cmCancel,
    NewButton(gtid(msTaglineHelp),cmHelp,
    NIL))))))));
  HelpContext := hcTaglineSelectDialog;
  FitBounds;
end;

procedure TTaglineSelectDialog.HandleEvent;
  procedure ChooseRandom;
  var
    sansliTavsan:integer;
  begin
    sansliTavsan := Random(Lister^.ItemList^.Count);
    if sansliTavsan = Lister^.FocusedItem then inc(sansliTavsan);
    if sansliTavsan > Lister^.ItemList^.Count-1 then sansliTavsan := 0;
    Lister^.FocusItem(sansliTavsan);
  end;

  procedure ManualEntry;
  type
    theberec = record
      s      : string[76];
      save   : boolean;
    end;
  var
    rec:THeberec;
    code:word;
    P:PTaglineInputDialog;
  begin
    New(P,Init(true));
    code := GSystem^.ExecView(P);
    if code = cmOK then P^.GetData(rec);
    Dispose(P,Done);
    if (code <> cmOK) or (rec.s='') then exit;
    if rec.save then Adopt(rec.s);
    Lister^.ItemList^.AtInsert(0,NewStr(rec.s));
    Lister^.FocusedItem := 0;
    EndModal(cmOK);
  end;
begin
  inherited HandleEvent(Event);
  if Event.What = evCommand then case Event.Command of
    cmRandom : ChooseRandom;
    cmWhatever : begin
                   ChooseRandom;
                   EndModal(cmOK);
                 end;
    cmManualEntry : ManualEntry;
    else exit;
  end else exit; {case}
  ClearEvent(Event);
end;

{- TMarkDialog -}
constructor TMarkDialog.Init;
var
  R:TRect;
  procedure putbut(amsg:FnameStr; acmd:word);
  var
    Pb:PButton;
  begin
    New(Pb,SizedInit(R,amsg,acmd));
    R.Move(0,Pb^.Size.Y+5);
    Insert(Pb);
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(msLaterHdr));
  Options := (Options or Ocf_Centered) and not Ocf_ZoomEffect;
  r.assign(0,0,200,21);
  r.move(5,5);
  putbut(gtid(msLaterReply),cmYes);
  putbut(gtid(msLaterRead),cmNo);
  putbut(gtid(msLaterSave),cmOK);
  putbut(gtid(msLaterCancel),cmCancel);
  FitBounds;
end;

{- TPersonInputDialog -}
constructor TPersonInputDialog.Init(ahdr:FnameStr);
var
  R:TRect;
  b:byte;
  procedure putinput(aprompt:FnameStr; maxlen:byte; axsize:word);
  var
    PL:PLabel;
    PI:PInputLine;
  begin
    New(PL,Init(r.a.x,r.a.y,aprompt,fntPropSmall));
    Insert(PL);
    New(PI,Init(r.a.x+65,r.a.y,axsize,fntPropSmall,maxlen));
    Insert(PI);
    inc(r.a.y,PI^.Size.Y+5);
  end;
begin
  R.Assign(0,0,0,0);
  inherited INit(R,ahdr);
  Options := Options or Ocf_Centered;
  r.a.x := 5;
  r.a.y := 5;
  putinput(gtid(msAddrNameSurname),35,100);
  putinput(gtid(msAddrNetmail),20,100);
  putinput(gtid(msAddrVoice),20,100);
  putinput(gtid(msAddrData),20,100);
  putinput(gtid(msAddrNotes),70,150);
  for b:=1 to 3 do putinput('',70,150);
  InsertBlock(GetBlock(r.a.x,r.a.y+5,mnfHorizontal,
    NewButton(gtid(msAddrOK),cmOK,
    NewButton(gtid(msAddrCancel),cmCancel,NIL))));
  SelectNext(True);
  FitBounds;
end;

procedure TPersonInputDialog.GetData;
var
  scr:TPersonScr;
  b:byte;
begin
  inherited GetData(scr);
  with TPerson(rec) do begin
    Name := scr.Name;
    Str2Addr(scr.Netmail,Netmail);
    Tel1 := scr.Tel1;
    Tel2 := scr.Tel2;
    for b:=1 to 4 do Notes[b] := scr.Notes[b];
  end;
end;

procedure TPersonInputDialog.SetData;
var
  scr:TPersonScr;
  b:byte;
begin
  with TPerson(rec) do begin
    scr.Name := Name;
    scr.Netmail := Addr2Str(Netmail);
    scr.Tel1 := Tel1;
    scr.Tel2 := Tel2;
    for b:=1 to 4 do scr.Notes[b] := Notes[b];
  end;
  inherited SetData(scr);
end;

{- TPersonLister -}

function TPersonLister.GetText;
var
  P:PPerson;
begin
  P := ItemList^.At(item);
  GetText := P^.Name+'|'+Addr2Str(P^.NetMail);
end;

procedure TPersonLister.ItemDoubleClicked;
begin
  Message(Owner,evCommand,cmEdit,@Self);
end;

{- TPersonDialog -}
constructor TPersonDialog.Init(ahdr:FnameStr);
var
  R:TRect;
  Ps:PScrollBar;
  procedure putb(amsg:fnameStr; acmd:word);
  var
    P:PButton;
  begin
    New(P,SizedInit(R,amsg,acmd));
    P^.Options := P^.options and not Ocf_Selectable;
    R.Move(0,P^.Size.Y+5);
    Insert(P);
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,ahdr);
  Options := Options or Ocf_Centered;
  New(Lister,Init(5,5,fntPropSmall,10,
    NewColumn(gtid(msAddrColumnPerson),160,0,
    NewColumn(gtid(msAddrColumnAddr),72,0,
    NIL))));
  Lister^.NewList(GetPersonList);
  Lister^.GetBounds(R);
  Insert(Lister);
  r.a.x := r.b.x+1;
  r.b.x := r.a.x+scrollerXSize;
  New(Ps,Init(R));
  Insert(Ps);
  Lister^.AssignScroller(ps);
  r.a.x := r.b.x+5;
  r.b.x := r.a.x+60;
  r.b.y := r.a.y+25;
  Insert(New(PAccelerator,Init(
    NewAcc(kbIns,cmAdd,
    NewAcc(kbDel,cmDelete,
    NewAcc(kbEnter,cmEdit,
    NIL))))));
  putb(gtid(msAddrAdd),cmAdd);
  putb(gtid(msAddrEdit),cmEdit);
  putb(gtid(msAddrDelete),cmDelete);
  putb(gtid(msAddrClose),cmClose);
  putb(gtid(msAddrHelp),cmHelp);
  FitBounds;
  HelpContext := hcPersonDialog;
end;

function TPersonDialog.Valid;
begin
  WritePersonList(Lister^.ItemList);
  Valid := true;
end;

procedure TPersonDialog.HandleEvent(var Event:TEvent);

  procedure RefreshDatabase;
  begin
    WritePersonList(Lister^.ItemList);
  end;

  function Excellence(ahdr:FnameStr; var rec:TPerson):word;
  var
    P:PPersonInputDialog;
    code:word;
  begin
    New(P,Init(ahdr));
    P^.SetData(rec);
    code := GSystem^.ExecView(P);
    if code = cmOK then P^.GetData(rec);
    Dispose(P,Done);
    Excellence := code;
  end;

  procedure AddMan;
  var
    P:PPerson;
  begin
    New(P);
    ClearBuf(P^,SizeOf(TPerson));
    if Excellence(gtid(msAddrNewHdr),P^) = cmOK then begin
      EventWait;
      Lister^.ItemList^.Insert(P);
      Lister^.PaintView;
      RefreshDatabase;
    end else Dispose(P);
  end;

  procedure EditMan;
  var
    P:PPerson;
  begin
    if Lister^.itemList^.Count = 0 then exit;
    P := Lister^.itemList^.At(Lister^.FocusedItem);
    if Excellence(gtid(msAddrEditHdr),P^) = cmOK then begin
      EventWait;
      RefreshDatabase;
    end;
  end;

  procedure DeleteMan;
  begin
    if Lister^.itemList^.Count = 0 then exit;
    if MessageBox(gtid(msAddrAskDeleteTxt),0,mfYesNo) = cmYes then begin
      EventWait;
      Lister^.DeleteItem(Lister^.FocusedItem);
      RefreshDatabase;
    end;
  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evBroadcast : case Event.Command of
                    cmAddressBookChanged : begin
                                             Lister^.NewList(GetPersonList);
                                             Lister^.PaintView;
                                           end;
                    cmAreYouAddrBook : ;
                    else exit;
                  end;
    evCommand   : case Event.Command of
                    cmAdd : AddMan;
                    cmEdit : EditMan;
                    cmDelete : DeleteMan;
                    else exit;
                  end; {case}
    else exit;
  end; {case}
  ClearEvent(Event);
end;

{- TSearchDialog -}
constructor TSearchDialog.Init;
const
  deflabelsize = 40;
  definputsize = 150;
var
  R:TRect;
  procedure putinput(id:word; maxlen:byte);
  var
    Pl:PLabel;
    Pi:PInputLine;
  begin
    New(Pl,Init(5,r.a.y+1,gtid(id),fntPropSmall));
    Insert(Pl);
    New(Pi,Init(5+deflabelsize,r.a.y,definputsize,fntProp,maxlen));
    pi^.GetBounds(R);
    r.a.y := r.b.y + 5;
    Insert(Pi);
  end;
begin
  R.Assign(0,0,0,0);
  inherited INit(R,ahdr);
  Options := Options or Ocf_Centered;
  r.a.y := 5;
  putinput(msSearchDialogFrom,35);
  putinput(msSearchDialogTo,35);
  putinput(msSearchDialogSubj,71);
  putinput(msSearchDialogText,71);
  InsertBlock(getBlock(5,r.a.y,mnfHorizontal,
    NewButton(gtid(msSearchStart),cmOK,
    newButton(gtid(msSearchCancel),cmCancel,
    newButton(gtid(msSearchHelp),cmHelp,
    NIL)))));
  SelectNext(true);
  HelpContext := hcSearchDialog;
  FitBounds;
  SetData(setup.lastSearch);
end;
{constructor TSearchDialog.Init;
var
  R:TRect;
  pi:Pinputline;
  pr:PRadioButton;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,ahdr);
  Options := Options or Ocf_Centered;
  Insert(New(PLabel,Init(5,6,gtid(msSearchDialogHdr),fntPropSmall)));
  New(Pi,Init(90,5,82,fntProp,79));
  pi^.GetBounds(R);
  Insert(Pi);
  r.a.x := 5;
  r.a.y := r.b.y+5;
  r.b.y := r.a.y+65;
  New(Pr,Init(r,fntPropSmall,
    NewChooser(gtid(msSearchFrom),
    NewChooser(gtid(msSearchTo),
    NewChooser(gtid(msSearchSubj),
    NewChooser(gtid(msSearchText),
    NIL))))));
  Pr^.Getbounds(R);
  Insert(Pr);
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal,
    NewButton(gtid(msSearchStart),cmOk,
    NewButton(gtid(msSearchCancel),cmCancel,
    NIL))));
  SelectNext(true);
  FitBounds;
end;}

{- TAreaAdjustDialog -}
constructor TAreaAdjustDialog.Init(ahdr:FnameStr);
var
  R:TRect;
  Pl:PLabel;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,ahdr);
  Options := Options or Ocf_Centered;
  New(Pl,Init(5,5,gtid(msAreaAdjustHdr),fntPropSmall));
  Insert(Pl);
  r.a.y := Pl^.Size.Y+10;
  r.a.x := 5;
  r.b.x := 220;
  r.b.y := r.a.y+65;
  Insert(New(PRadioButton,Init(R,fntPropSmall,
    NewChooser(gtid(msAreaAdjustNone),
    NewChooser(gtid(msAreaAdjustPers),
    NewChooser(gtid(msAreaAdjustPAll),
    NewChooser(gtid(msAreaAdjustAll),
    NIL)))))));
  InsertBlock(getBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msAreaAdjustOK),cmOK,
    NewButton(gtid(msAreaAdjustCancel),cmCancel,
    NIL))));
  FitBounds;
end;

procedure TAreaAdjustDialog.SetData;
var
  w:word;
begin
  w := byte(rec);
  inc(w);
  inherited SetData(w);
end;

procedure TAreaAdjustDialog.GetData;
var
  w:word;
begin
  inherited GetData(w);
  byte(rec) := w-1;
end;

{- TNetFlagChangeDilaog -}
constructor TNetFlagChangeDialog.Init;
var
  R:TRect;

  procedure Putcb(msg:FnameStr; reqflag:word);
  var
    P:PSingleCheckBox;
  begin
    New(P,Init(5,r.a.y,fntPropSmall,msg));
    inc(r.a.y,P^.Size.Y+5);
    if user.Netflags and reqflag = 0 then P^.SetState(Scf_Disabled,True);
    Insert(P);
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(msNetFlagsHdr));
  Options := (Options or Ocf_Centered) and not Ocf_ZoomEffect;
  r.a.y := 5;
  putcb(gtid(msNetFlagsCrash),wnfCrash);
  putcb(gtid(msNetFlagsKill),wnfKill);
  putcb(gtid(msNetFlagsHold),wnfHold);
  putcb(gtid(msNetFlagsImmed),wnfImmed);
  putcb(gtid(msNetFlagsDirect),wnfDirect);
  InsertBlock(GetBlock(5,r.a.y,mnfHorizontal,
    NewButton(gtid(msNetFlagsOK),cmOK,
    NewButton(gtid(msNetFlagsCancel),cmCancel,
    NewButton(gtid(msNetFlagsHelp),cmHelp,
    NIL)))));
  FitBounds;
  SelectNext(True);
  HelpContext := hcNetFlagChangeDialog;
end;

procedure TNetFlagChangeDialog.SetData;
var
  scr:TNetScr;
  function isit(flag:word):boolean;
  begin
    isit := word(rec) and flag <> 0;
  end;
begin
  scr.Crash := isit(wnfCrash);
  scr.KSent := isit(wnfKill);
  scr.Hold  := isit(wnfHold);
  scr.Immed := isit(wnfImmed);
  scr.Direct := isit(wnfDirect);
  inherited SetData(scr);
end;

procedure TNetFlagChangeDialog.GetData;
var
  scr:TNetScr;
  procedure setit(flag:word);
  begin
    word(rec) := word(rec) or flag;
  end;
begin
  inherited GetData(scr);
  word(rec) := 0;
  if scr.Crash then setit(wnfCrash);
  if scr.KSent then setit(wnfKill);
  if scr.Hold  then setit(wnfHold);
  if scr.Immed then setit(wnfImmed);
  if scr.Direct then setit(wnfDirect);
end;

{- TFilterSelLister -}
procedure TFilterSelLister.ItemDoubleClicked;
begin
  Message(Owner,evCommand,cmDelRequest,NIL);
end;

{- TFilterLister -}
constructor TFilterLister.Init;
begin
  inherited Init(x,y,fntPropSmall,10,
    NewColumn(gtid(msFltListerName),60,0,
    NewCOlumn(gtid(msFltListerDesc),210,0,
    NewColumn(gtid(msFltListerAuthor),70,0,
    NIL))));
  SetConfig(Lvc_KeepList,True);
  NewList(filterList);
end;

procedure TFilterLister.itemDoubleClicked;
begin
  Message(Owner,evCommand,cmAddRequest,NIL);
end;

function TFilterLister.GetText;
var
  P:PFilter;
begin
  P := ItemList^.At(item);
  GetText := Lower(P^.Cmd)+'|'+P^.Desc+'|'+P^.Author;
end;

{- TFilterLookupDialog -}
constructor TFilterLookupDialog.Init(ahdr:string);
var
  R:TRect;
  procedure putlabel(msg:string);
  begin
    Insert(New(PLabel,FullInit(r.a.x,r.a.y,msg,cRed,Col_Back,fntPropSmall)));
    inc(r.a.y,Top^.Size.Y+2);
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,ahdr);
  Options := Options or Ocf_Centered;
  r.a.x := 5;
  r.a.y := 5;
  putlabel(gtid(msFltSelLabel));
  r.b.x := r.a.x+60;
  r.b.y := r.a.y+133;
  New(LLister,Init(R,fntPropSmall));
  LLister^.GetBounds(R);
  Insert(LLister);
  r.a.x := r.b.x+5;
  r.a.y := 60;
  InsertBlock(GetBlock(r.a.x,r.a.y,mnfVertical,
    NewButton('~<-',cmAddRequest,
    NewButton('-~>',cmDelRequest,
    NIL))));
  inc(r.a.x,Top^.Size.X+5);
  r.a.y := 5;
  New(RLister,init(r.a.x,r.a.y));
  RLister^.GetBounds(R);
  Insert(RLister);
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msGenericSaveButton),cmOK,
    NewButton(gtid(msGenericCancelButton),cmCancel,
    NewButton(gtid(msGenericHelpButton),cmHelp,
    NIL)))));
  HelpContext := hcFilterLookup;
  FitBounds;
end;

procedure TFilterLookupDialog.HandleEvent(var Event:Tevent);
var
  P:PFilter;
begin
  inherited HandleEvent(Event);
  if Event.What = evCommand then case Event.Command of
    cmAddRequest : if RLister^.ItemList^.Count > 0 then begin
      P := RLister^.ItemList^.At(RLister^.FocusedItem);
      LLister^.ItemList^.Insert(NewStr(lower(P^.Cmd)));
      LLister^.PaintView;
      ClearEvent(Event);
    end;
    cmDelRequest : if LLister^.ItemList^.Count > 0 then begin
      LLister^.DeleteItem(LLister^.FocusedItem);
      ClearEvent(Event);
    end;
  end; {case}
end;

procedure TFilterLookupDialog.SetData;
var
  b:byte;
  P:PTextCollection;
  s:string;
begin
  New(P,Init(10,10));
  for b:=1 to GetParseCount(string(rec),'+') do begin
    s := GetParse(string(rec),'+',b);
    Strip(s);
    FastLower(s);
    P^.Insert(NewStr(s));
  end;
  LLister^.NewList(P);
end;

procedure TFilterLookUpDialog.GetData;
var
  s:string;
  n:integer;
begin
  s := '';
  for n:=0 to LLister^.ItemList^.Count-1 do s := s + PString(LLister^.ItemList^.At(n))^ + '+';
  if s <> '' then if s[length(s)] = '+' then dec(byte(s[0]));
  string(rec) := s;
end;

function TFilterLookupDialog.DataSize:word;
begin
  DataSize := SizeOf(setup.FilterCmd);
end;

{- TDateTimeViewer -}
constructor TDateTimeViewer.Init;
var
  R:TRect;
begin
  R.Assign(0,0,102,GetFontHeight(afont)+1);
  R.Move(x,y);
  inherited Init(R);
  Options := Options or (Ocf_TopSelect or Ocf_AlwaysOnTop or Ocf_PreProcess or Ocf_FullDrag or Ocf_Move);
  EventMask := evMouse or evBroadcast;
  Font      := afont;
  GetMoment(Moment);
end;

procedure TDateTimeViewer.HandleEvent;
  procedure SetupAlarm;
  var
    code:word;
    s:string[5];
    b:byte;
  begin
    if GetConfig(dtcAlarm) then with Alarm do s := z2s(Hour,2)+':'+z2s(Min,2)
                           else s := '';
    if InputBox(gtid(msalarmSetupHdr),gtid(msalarmSetupPrompt),hcAlarmSetupDialog,fntProp,s,5) then begin
      Strip(s);
      if s <> '' then begin
        Move(Moment,Alarm,SizeOf(TMoment));
        b := pos(':',s);
        if b = 0 then inc(Alarm.Min,s2l(s)) else begin
          Alarm.Hour := s2l(copy(s,1,b-1));
          Alarm.Min  := s2l(copy(s,b+1,2));
        end;
        SetConfig(dtcAlarm,True);
      end;
    end;
  end;

begin
  case Event.What of
    evMouseDown : if Event.Buttons = mbLeft then begin
                     SetupAlarm;
                     ClearEvent(Event);
                  end else begin
                    Drag(Event,dmDragMove+dmLimitAll);
                    ClearEvent(Event);
                  end;
  end;
end;

procedure TDateTimeViewer.GetMoment(var amoment:TMoment);
var
  temp:word;
  xhour,xmin:word;
  xmonth,xday:word;
begin
  with amoment do begin
    GetTime(xhour,xmin,temp,temp);
    GetDate(year,xmonth,xday,temp);
    Hour := xhour;
    Min  := xmin;
    Month := xmonth;
    Day   := xday;
  end;
end;

procedure TDateTimeViewer.Paint;
var
  R:TRect;
  function tostr(hebe:byte):string;
  begin
    tostr := z2s(hebe,2);
  end;
begin
  PaintBegin;
    GetExtent(R);
    ShadowBox(R,True);
    SetTextColor(cBlue,Col_Back);
    with Moment do XPrintStr(1,1,Size.X-1,Font,
       tostr(Day)+'/'+tostr(Month)+'/'+l2s(Year)+' '+tostr(Hour)+':'+tostr(Min));
  PaintEnd;
end;

procedure TDateTimeViewer.Backprocess;
var
  temp:TMoment;
  cause:FnameStr;
begin
  GetMoment(temp);
  if not BufCmp(temp,Moment,SizeOf(Moment)) then begin
    GetMoment(Moment);
    PaintView;
    if GetConfig(dtcAlarm) then if BufCmp(Moment,Alarm,SizeOf(Moment)) then begin
      cause := gtid(msAlarmPopUpMsg);
      Replace(cause,'%T',z2s(moment.hour,2)+':'+z2s(moment.min,2));
      ExecBox(cause,gtid(msAlarmPopUpHdr),0,
        GetBlock(0,0,mnfHorizontal,NewButton(gtid(msAlarmPopUpBtn),cmOK,NIL)));
    end;
  end;
end;

function ReadSetup:TSetupResult;
var
  T:TSetupStream;
  procedure getbuf(key:string; var buf; size,flags:word);
  begin
    if XSetupGetKey(key,buf,size,T,flags) <> rsiOK then ReadSetup := setupMismatch;
  end;

  procedure getstr(key:string; var s:string; maxlen:byte);
  begin
    getbuf(key,s[0],maxlen+1,0);
  end;
begin
  T.Init(wkdir^+setupFile,stOpenRead);
  if T.Status <> stOK then begin
    ReadSetup := setupNone;
    T.Done;
    exit;
  end;
  ReadSetup := setupOK;
  with setup do begin
    getstr('ULDir',ULDir,79);
    getstr('DLDir',DLDir,79);
    getstr('EditCmd',EditCmd,79);
    getstr('FilterCmd',FilterCmd,79);
    getstr('QuoteStr',QuoteStr,79);
    getstr('Password',Password,15);
    if Regged then getbuf('Flags',Flags,2,gfNormal);
    getbuf('Behaviour',Behaviour,2,gfNormal);
    if Regged then getstr('ColorScheme',ColorScheme,40);
    getstr('XPrnFileName',prnFile,SizeOf(prnFile)-1);
    getbuf('MsgWindow',MsgWindow,SizeOf(TRect),gfNoRecover);
    if Regged then begin
      getBuf('readerFont',readerFont,SizeOf(readerFont),gfNormal);
      getBuf('readerColors',readerColors,SizeOf(readerColors),gfNoRecover);
    end;
    getBuf('RClock',rclock,SizeOf(rclock),gfNoRecover);
    getBuf('RCDPlayer',rcdplayer,SizeOf(rcdplayer),gfNoRecover);
{    getBuf('RPerc',rperc,SizeOf(rperc),gfNoRecover);}
    getBuf('lastSearch',lastSearch,SizeOf(lastSearch),gfNormal);
  end;
  setup.readerFontH := GetFontHeight(setup.readerFont);
  T.Done;
end;

procedure WriteSetup;
var
  T:TSetupStream;
  procedure putbuf(key:string; var buf; size:word);
  begin
    XSetupPutKey(key,buf,size,T);
  end;
  procedure putstr(key,s:string; maxlen:byte);
  begin
    putbuf(key,s[0],maxlen+1);
  end;
begin
  EventWait;
  T.Init(wkdir^+setupFile,stCreate);
  with Setup do begin
    putstr('AppVersion',rVersion,SizeOf(rVersion)-1);
    putstr('ULDir',ULDir,79);
    putstr('DLDir',DLDir,79);
    putstr('EditCmd',EditCmd,79);
    putstr('FilterCmd',FilterCmd,79);
    putstr('QuoteStr',QuoteStr,79);
    putstr('Password',Password,15);
    putbuf('Flags',Flags,2);
    putbuf('Behaviour',Behaviour,2);
    putstr('ColorScheme',ColorScheme,40);
    putstr('XPrnFileName',prnFile,SizeOf(prnFile)-1);
    putbuf('MsgWindow',MsgWindow,SizeOf(TRect));
    putbuf('readerFont',readerFOnt,SizeOf(readerFont));
    putbuf('readerColors',readerColors,SizeOf(readerColors));
    if DateTimeViewer <> NIL then DateTimeViewer^.GetBounds(rclock);
    if CDPlayer <> NIL then CDPlayer^.GetBounds(rcdplayer);
{    if Persia <> NIL then Persia^.GetBounds(rperc);}
    putbuf('RClock',rclock,SizeOf(rclock));
    putbuf('RCDPlayer',rcdplayer,SizeOf(RCDPlayer));
    putbuf('lastSearch',lastSearch,SizeOf(lastSearch));
{    putbuf('RPerc',rperc,SizeOf(rperc));}
  end;
  T.Done;
  Message(GSystem,evBroadcast,cmUpdatePacketList,NIL);
end;

procedure CheckReplyPacketConflict;
var
  P:Pmsg;
  warnmsg:string;
begin
  if (replypacket^.msglist^.count > 0) and (setup.Flags and ufNoConflictCheck = 0) then begin
    P := replypacket^.msglist^.at(0);
    if (upper(p^.from) <> upper(user.name)) and (upper(p^.from) <> upper(user.alias)) then begin
      warnmsg := gtid(msReplyConflictMsg);
      Replace(warnmsg,'%F',p^.From);
      if ExecBox(warnmsg,gtid(msReplyConflictHdr),hcReplyPacketConflict,
              GetBlock(0,0,mnfHorizontal,
              NewButton(gtid(msReplyCOnflictIgnore),cmClose,
              NewButton(gtid(msReplyConflictDontAsk),cmNo,
              NewButton(gtid(msReplyCOnflictHelp),cmHelp,
              NIL))))) = cmNo then begin
                setup.Flags := setup.Flags or ufNoConflictCheck;
                WriteSetup;
              end;
    end;
  end;
end;

function OpenPacket(apakname,apakdir,arepdir:FnameStr; unpackreplies,fakepacket:boolean):boolean;
var             {if fakepacket apakname becomes the inf file}
  dir:DirStr;
  name:NameStr;
  ext:ExtStr;
  prefix:string[8];
  repn:FnameStr;
  pk:TPakType;
begin
  OpenPacket := false;
  if packetOpen then exit;
  EventWait;
  Debug('opening '+apakname);
  FSplit(apakname,dir,name,ext);
  prefix := name;
  currentPacketName := apakname;
  CreateDir(apakdir);
  CreateDir(arepdir);
  XDeleteWild(apakdir+'*.*');
  XDeleteWild(arepdir+'*.*');
  if fakepacket then begin
    CopyFile(infDir^+apakname,apakdir+apakname);
  end else begin
    if not UnpackArchive(apakname,apakdir) then exit;
  end;
  pk := DetectPakType(apakdir);
  if pk=pakUnknown then exit;
  repn := setup.ULDir+prefix+ReplyExt(pk);
  if unpackreplies and XFileExists(repn) then UnPackArchive(repn,arepdir);
  Packet := GenericInit(pk,apakdir);
  if Packet = NIL then exit;
  ReplyPacket := GenericReplyInit(pk,arepdir);
  if ReplyPacket = NIL then Abort('OpenPacket','replypacket nil? nasi?');
  if Packet^.Load then begin
    if not ReplyPacket^.Load then Debug('Reply packet load failed')
                             else CheckReplyPacketConflict;
    packetOpen := true;
    OpenPacket := packetOpen;
  end else begin
    Dispose(Packet,Done);
    packetOpen := false;
  end;
end;

procedure ClosePacket;
begin
  if not packetOpen then exit;
  EventWait;
  {dispose area list is not needed here!!}
  with Packet^ do begin
    RemoveDir(Packet^.Where);
    RemoveDir(ReplyPacket^.Where);
  end;
  Dispose(Packet,Done);
  Dispose(ReplyPacket,Done);
  ReplyPacket := NIL;
  Packet      := NIL;
  packetOpen := false;
end;

end.