{
Wolverine common procs

updates:
--------
12th Aug 96 - 12:49 - going...
17th Aug 96 - 06:36 - added anti-debug things...
20th Aug 96 - 20:07 - some fixes...
23rd Aug 96 - 15:26 - multilingual adaptation...
 9th Oct 96 - 01:10 - good...
10th Oct 96 - 01:46 - bugfix in exec... (not from 1.x)
}

unit WProcs;

interface

uses XStream,Dos,Objects,WTypes;

procedure Str2Addr(s:FnameStr; var addr:TAddr);
function  Addr2Str(var addr:TAddr):string;
procedure CreateDir(adir:FnameStr);
procedure RemoveDir(adir:FnameStr);
function  XExec(afile,aparams:string; flags:word):boolean;
procedure UnregDialog;
procedure Adopt(line:FnameStr);
procedure PrintMsgHeader(amsg:PMsg);
procedure KeyDecode(var buf;count:word);
procedure GetOrigin(text:PMsgText; var a:TAddr);
function  GetTaglineList:PTaglineCollection;
function  GetColorList:PStringCollection;
function  MKS2Long(mks:longint):longint;
function  EditFile(fn:FnameStr):boolean;
function  GetPersonList:PPersonColl;
procedure WritePersonList(P:PCollection);
procedure SaveMsg(amsg:PMsg; atext:PMsgText);
function  getInitials(s:string):string;
function  GetQuoteStr(amsg:PMsg):string;
procedure GetExtraPacketInfo(var rec:TPacket);
procedure Usage;
procedure ReportMem;
procedure ReportSystemStatus;
procedure CikCik;
function NewText(size:word):PMsgText;
function Stream2Text(var T:TStream; size:longint):PMsgText;
function Regged:boolean;
procedure PadBuf(var buf; size:word);
procedure DisposeText(P:PMsgText);
procedure CopyStream(var I,O:TStream; size:longint);
procedure CopyFile(fn1,fn2:FnameStr);
procedure Abort(source,why:FnameStr);
function DetectPakType(where:FnameStr):TPakType;
function DetectReplyPakType(where:FnameStr):TPakType;
function ReplyName(apacket:PMsgPacket):FnameStr;
function ReplyExt(apaktype:TPakType):ExtStr;
function GenericInit(what:TPakType; where:FnameStr):PMsgPacket;
function GenericReplyInit(what:TPakType; where:FnameStr):PMsgPacket;
procedure FileErrorBox(f:FnameStr);
procedure WriteMsgHeaderToTextStream(amsg:Pmsg; var astream:TDosStream);
procedure AddTearline(fn:FnameStr);
procedure DoFilter(fn:FnameStr; acmd:string);
function  GetSampleText:PMsgText;
function  GetFontList:PStringCollection;
function  IsThisPacketOpen(p:PPacket):boolean;
procedure PostMsg(fn:FnameStr; amsg:PMsg);
procedure PackReplies;
procedure SetFont(id:word);
procedure Status(msg:string);
function  IsThatMsg(T:TMsg; apattern:TSearchScr):boolean;

implementation

uses

{$IFNDEF DPMI}Exec,XOver,{$ENDIF}

WHelp,WArch,WHAM,WQWK,WBlue,XGfx,XMouse,XStr,XDebug,Debris,QText,XDev,
XSys,AXEServ,WLan,XLan,Tools,XDiag,Drivers,GView,XIO,XBuf,XPrn,
XTypes;

procedure NoCycle;
begin
end;

function IsThatMsg;
var
  pat2:TSearchScr;
  b:boolean;
  procedure adjust(var s:string);
  begin
    Strip(s);
    FastUpper(s);
  end;
  function ciss(var s1,s2:string):boolean;
  begin
    if s1 = '' then ciss := true else ciss := pos(s1,s2) > 0;
  end;
begin
  IsThatMsg := false;
  with apattern do begin
    adjust(from);
    adjust(too);
    adjust(subj);
  end;
  with T do begin
    adjust(from);
    adjust(too);
    adjust(subj);
  end;
  isthatmsg := ciss(apattern.from,T.from) and
       ciss(apattern.too,T.too) and
       ciss(apattern.subj,T.subj);
end;

procedure Status;
begin
  if StatusLine <> NIL then StatusLine^.NewText(msg);
end;

{procedure InitWPerc;
begin
  New(Persia,Init(R,cLightRed,cBlack));
  Persia^.Options := Persia^.Options or OCf_AlwaysOnTop;
  GSystem^.Insert(Persia);
end;

procedure WPerc;
begin
  if Persia <> NIL then Persia^.NewPerc(aval,amax)
                   else Debug('*** ILLEGAL CALL TO WPERC ***');
end;

procedure WPercReset;
begin
  WPerc(0,0);
end;}

function GetFontList;
var
  Ps:PStringCollection;
  w:word;
  P:PFont;
  s:string;
begin
  New(Ps,Init(10,10));
  for w:=1 to 100 do begin
    P := GetRscByid(rtFont,w);
    s := GetRscName(rtFont,w);
    Strip(s);
    if P <> NIL then if P^.FontType = ftBitmapped then Ps^.Insert(NewStr(s));
  end;
  GetFontList := Ps;
end;

procedure SetFont(id:word);
begin
  setup.readerFont := id;
  setup.readerFontH := GetFontHeight(id);
end;

procedure PackReplies;
begin
  EventWait;
  PackArchive(ReplyName(Replypacket),Replypacket^.Where+'*.*');
end;

procedure PostMsg(fn:FnameStr; amsg:PMsg);
begin
  if not Regged then UnRegDialog;
  AddTearline(fn);
  ReplyPacket^.PostMsg(amsg,fn);
  if setup.Flags and ufReplySafe > 0 then PackReplies;
  Message(GSystem,evBroadcast,cmRefreshReplies,amsg);
end;

function IsThisPacketOpen(p:PPacket):boolean;
begin
  IsThisPacketOpen := packetOpen and (p^.Name = XGetFileName(currentPacketName));
end;

procedure FileErrorBox;
begin
  MessageBox(gtid(msFileErrorMsg)+' ('+f+')',hcFileError,mfError);
end;

function GetColorList:PStringCollection;
var
  T:TDosStream;
  s:string;
  Ps:PStringCollection;
begin
  New(Ps,Init(10,10));
  T.Init(wkdir^++colFile,stOpenRead);
  if T.Status = stOK then while T.GetPos < T.GetSize do begin
    IniReadln(T,s);
    if pos('[',s) = 1 then begin
      Delete(s,1,1);
      dec(byte(s[0]));
      Ps^.Insert(NewStr(s));
    end;
  end;
  Ps^.Insert(NewStr(defaultSchemeName));
  T.Done;
  GetColorList := Ps;
end;

procedure GetOrigin(text:PMsgText; var a:TAddr);
var
  w:word;
  line:string;
  function lookforline:boolean;
  var
    b:byte;
  begin
    lookforline := false;
    Strip(line);
    b := pos(#1'MSGID:',line);
    if b > 0 then begin
      line := copy(line,b+8,255);
      b := pos(#32,line);
      if b = 0 then begin
        Debug('invalid MSGID?');
        exit;
      end;
      line := copy(line,1,b-1);
      Str2Addr(line,a);
      lookforline := true;
    end;
  end;
begin
  EventWait;
  line := '';
  ClearBuf(a,SizeOf(a));
  for w:=0 to Text^.size-1 do begin
    if text^.Data[w] = #13 then begin
      if lookforline then exit;
      line := '';
    end else line := line + text^.Data[w];
  end;
  if line <> '' then lookforline;
end;

function GetTaglineList;
var
  tagList:PTaglineCollection;
  T:TDosStream;
  s:String;
begin
  New(tagList,Init(10,10));
  T.Init(wkdir^+tagFile,stOpenRead);
  if T.Status = stOK then while T.GetPos < T.GetSize do begin
    SReadln(T,s);
    Strip(s);
    if s <> '' then if s[1] <> ';' then tagList^.Insert(NewStr(s));
  end;
  T.Done;
  GetTaglineList := tagList;
end;

const

  Cst_XorB = nVersion xor $75;

procedure KeyDecode(var buf;count:word);assembler;
asm
    cld
    push  ax
    push  di
    les   di,buf
    mov   cx,count
    mov   bh,Cst_XorB
@Loop:
    mov   bl,byte ptr es:[di]
    ror   bl,3
    xor   bl,bh
    ror   bl,3
    mov   byte ptr es:[di],bl
    inc   di
    loop  @Loop
    pop   di
    pop   ax
end;

procedure Str2Addr(s:FnameStr; var addr:TAddr);
var
  b1,b2,b3:byte;
begin
  b1 := pos(':',s);
  b2 := pos('/',s);
  b3 := pos('.',s);
  if (b1 = 0) or (b2 = 0) then exit;
  if b1 > 0 then Addr.Zone := s2l(copy(s,1,b1-1));
  if b2 > 0 then Addr.Net  := s2l(copy(s,b1+1,(b2-b1)-1));
  if b3 = 0 then Addr.Node := s2l(copy(s,b2+1,255)) else begin
    Addr.Node  := s2l(copy(s,b2+1,(b3-b2)-1));
    Addr.Point := s2l(copy(s,b3+1,255));
  end;
end;

function Addr2Str(var addr:TAddr):string;
var
  temp:FnameStr;
begin
  with addr do temp := l2s(zone)+':'+l2s(net)+'/'+l2s(node);
  if addr.point > 0 then temp := temp + '.'+l2s(addr.point);
  Addr2Str := temp;
end;

procedure CreateDir(adir:FnameStr);
begin
  if adir[length(adir)] = '\' then dec(byte(adir[0]));
  MkDir(adir);
end;

procedure RemoveDir(adir:FnameStr);
begin
  XMakeDirStr(adir,false);
  XDeleteWild(adir+'\*.*');
  RmDir(adir);
end;

function XExec(afile,aparams:string; flags:word):boolean;
const
  NullPointer : Pointer = NIL;
var
  old    : TPoint;
  code   : word;
  shell  : boolean;
begin
  XExec := false;
  shell := (afile = '') and (aparams = '');
  if shell then Debug('shelling')
           else Debug('xexec: '+afile+' '+aparams);
  if flags and xfDirect > 0 then begin
    if not XFileExists(afile) then begin
      Debug('exe not found');
      exit;
    end;
  end else begin
    if shell then begin
      afile := Interpreter^;
      aparams := '';
    end else begin
      aparams := '/C ' + afile + ' ' + aparams;
      afile   := Interpreter^;
    end;
  end;
  if flags and xfHideOutput = 0 then begin
    PointingDevice^.GetPosition(old);
    DoneGfx;
  end else XSetRedirection('NUL');
  PointingDevice^.DisableEvents;
  SwapVectors;
  Exec(afile,aparams);
  SwapVectors;
  code := Lo(DosExitCode) or DosError;
  if flags and (xfHideOutput+xfPause) = xfPause then begin
    qWrite(#13#10+gtid(msExecPause));
    asm
      xor ax,ax
      int 16h
    end;
  end;
  if flags and xfHideOutput = 0 then begin
    InitGfx;
    NullPalette;
    GSystem^.Paint;
  end else begin
    XSetRedirection('CON');
    with PointingDevice^ do begin
      GetPosition(old);
      SetPosition(old.x,old.y);
    end;
  end;
  with PointingDevice^ do begin
    EnableEvents;
    Show;
  end;
  EventWait;
  GSystem^.SetSysPalette;
  Tickstart := 0;
  XExec := code = 0;
  if code = 0 then XExec := true else Debug('exec failed');
end;

procedure unregdialog;
begin
  ExecBox(gtid(msUnregMsg),gtid(msUnregHeader),hcRegistration,GetBlock(0,0,mnfHorizontal,
    NewButton(gtid(msUnregExplain),cmHelp,
    NewButton(gtid(msUnregCancel),cmCancel,
    NIL))));
end;

procedure Adopt(line:FnameStr);
var
  T:TDosStream;
  s:string;
begin
  if not regged then begin
    unregdialog;
    exit;
  end;
  if pos('... ',line) = 1 then System.Delete(line,1,4);
  Strip(line);
  T.Init(wkdir^+tagFile,stOpen);
  if T.Status <> stOK then begin
    T.Init(wkdir^+tagFile,stCreate);
    SWriteln(T,line);
    T.Done;
    exit;
  end;
  while T.Getpos < T.GetSize do begin
    SReadln(T,s);
    Strip(s);
    if s = line then begin
      T.Done;
      exit;
    end;
  end;
  SWriteln(T,line);
  T.Done;
end;

function MKS2Long(mks:longint):longint;
begin
  MKS2Long := ((mks and not $ff000000) or $00800000) shr (24 -((mks shr 24) and $7f)) - 1;
end;

function EditFile(fn:FnameStr):boolean;
var
  oldattr:word;
begin
  EditFile := false;
  XSetFileAttr(fn,0);
  XExec('',setup.EditCmd+' '+fn,0);
  EditFile := XGetFileAttr(fn) and Archive > 0;
end;

function GetPersonList:PPersonColl;
var
  P:PPersonColl;
  Pp:PPerson;
  T:TDosStream;
begin
  EventWait;
  New(P,Init(10,10));
  T.Init(wkdir^+addrFile,stOpenRead);
  if T.Status = stOK then while T.GetPos < T.GetSize do begin
    New(PP);
    T.Read(PP^,SizeOf(Tperson));
    P^.Insert(PP);
  end;
  T.Done;
  GetPersonList := P;
end;

procedure WritePersonList(P:PCollection);
var
  Pp:PPerson;
  n:integer;
  T:TDosStream;
begin
  EventWait;
  T.Init(wkdir^+addrFile,stCreate);
  if T.Status = stOK then for n:=0 to P^.Count-1 do begin
    Pp := P^.At(n);
    T.Write(Pp^,SizeOf(TPerson));
  end else Debug('create error');
  T.Done;
end;

procedure WriteMsgHeader(amsg:PMsg; aproc:TWriteProc);
var
  gap:string[3];
  nm:FnameStr;
begin
  nm := amsg^.Area^.Name;
  aproc(Duplicate(lineChar,3)+' '+nm+' '+
        Duplicate(lineChar,71-length(nm)));
  gap := ' : ';
  aproc(Fix(gtid(msMsgHeaderFrom),6)+gap+amsg^.From);
  aproc(Fix(gtid(msMsgHeaderTo),6)+gap+amsg^.Too);
  aproc(Fix(gtid(msMsgHeaderSubj),6)+gap+Fix(amsg^.Subj,70));
  aproc(Fix(gtid(msMsgHeaderdate),6)+gap+Date2Str(LongRec(amsg^.Date).Hi,true));
  aproc(Duplicate(lineChar,76));
end;

procedure PrintMsgHeader(amsg:PMsg);
begin
  WriteMsgHeader(amsg,WritePrn);
end;

function getInitials(s:string):string;
var
  where:byte;
begin
  where := pos(' ',s);
  if where = 0 then where := 2;
  Getinitials := #32+s[1]+s[where+1]+'> ';
end;

function GetQuoteStr(amsg:PMsg):string;
var
  temp:string;
  b:byte;
  procedure Rep(what:FnameStr);
  var
    len:byte;
  begin
    if b < length(temp)-1 then if temp[b+2] in ['1','2'] then begin
      len := 3;
      what := GetParse(what,' ',byte(temp[b+2])-byte('0'));
    end else len := 2;
    Delete(temp,b,len);
    Insert(what,temp,b);
  end;
begin
  temp := setup.QuoteStr;
  while pos('@',temp) > 0 do begin
    b := pos('@',temp);
    case upcase(temp[b+1]) of
      'F' : rep(amsg^.From);
      'T' : rep(amsg^.Too);
      'S' : rep(amsg^.Subj);
      'D' : rep(Date2Str(LongRec(amsg^.Date).Hi,True)+' '+Time2Str(LongRec(amsg^.Date).Lo));
      'M' : rep(#13#10);
    end; {case}
  end;
  GetQuoteStr := temp;
end;

procedure GetExtraPacketInfo(var rec:TPacket);
var
  T:TDosStream;
  temp:TPacket;
begin
  T.Init(wkdir^+cacheFile,stOpenRead);
  if T.Status = stOK then while T.GetPos < T.GetSize do begin
    T.Read(temp,SizeOf(temp));
    if T.Status <> stOK then break;
    if Upper(temp.Name) = Upper(rec.Name) then begin
      rec.Total := temp.Total;
      rec.Unread := temp.Unread;
      rec.WillReply := temp.WillReply;
      rec.WillRead  := temp.WillRead;
      rec.Personal := temp.Personal;
      T.Done;
      exit;
    end;
  end;
  T.Done;
end;

procedure Usage;
var
  l:longint;
begin
  for l:=msUsage1 to msMaxusage-1 do writeln(gtid(l));
  halt(2);
end;

procedure ReportSystemStatus;
begin
  case GSystem^.MT of
    mtWindows  : Debug('Windows detected');
    mtOS2      : Debug('OS/2 detected');
    mtDESQview : Debug('DESQview detected');
    else Debug('No multitasker detected');
  end; {case}
  {$IFNDEF DPMI}
  case ovrStatus of
    xoNone : Debug('Overlays disabled');
    xoDisk : Debug('No overlay caches');
    xoXMS  : Debug('Overlays loaded in XMS');
    xoEMS  : Debug('Overlays loaded in EMS');
  end; {case}
  {$ENDIF}
  if GetSystem(Sys_Relax) then Debug('Multitasker support disabled');
  if runFaast then Debug('running faast');
  Debug('VAPI not detected');
  Debug('current config: '+setupFile);
  Debug('language: '+gtid(-1));
end;

procedure CikCik;
begin
  Debug('beep disabled to remove CRT unit usage');
end;

function ReplyName;
begin
  with apacket^ do ReplyName := setup.ULDir+packetid+ReplyExt(GetType);
end;

function ReplyExt(apaktype:TPakType):ExtStr;
begin
  case apaktype of
    pakWHAM : ReplyExt := '.WR';
    pakBW   : ReplyExt := '.NEW';
    pakQWK  : ReplyExt := '.REP';
    else Abort('ReplyExt','Unknown packet type');
  end; {case}
end;

procedure ReportPakType(what:TPakType);
begin
  case what of
    pakUnknown : Debug('couldn''t recognize packet type');
    pakQWK     : Debug('QWK detected');
    pakBW      : Debug('BlueWave detected');
    pakWHAM    : Debug('WHAM! detected');
    pakHudson  : Debug('Hudson message base detected');
  end; {case}
end;

function DetectPakType;
var
  hebe:TPakType;
begin
  hebe := pakUnknown;
  if XFileExists(where+'INFO.W') then hebe := pakWHAM else
  if XFilesExist(where+'*.INF') then hebe := pakBW else
  if XFileExists(where+'CONTROL.DAT') then hebe := pakQWK else
  if XFileExists(where+'MSGHDR.BBS') then hebe := pakHudson;
  ReportPakType(hebe);
  DetectPakType := hebe;
end;

function DetectReplyPakType;
var
  hebe:TPakType;
begin
  hebe := pakUnknown;
  if XFileExists(where+'INFO.W') then hebe := pakWHAM else
  if XFilesExist(where+'*.UPL') then hebe := pakBW else
  if XFileExists(where+'CONTROL.DAT') then hebe := pakQWK;
  ReportPakType(hebe);
  DetectReplyPakType := hebe;
end;

procedure Abort;
begin
  if DebugActive then begin
    SetConsole(true);
    Debug(source+': '+why);
    Debug('Program terminated - press any key to exit');
    asm
      xor ax,ax
      int 16h
    end;
  end;
  Error(source,why);
end;

function Regged:boolean;
begin
  Regged := true;
end;

procedure PadBuf(var buf; size:word);assembler;
asm
  les  di,buf
  mov  cx,size
  mov  al,32
  rep  stosb
end;

procedure CopyStream;
var
  buf:pointer;
  bufsize:word;
begin
  if size = 0 then begin
    Debug('null copy request - ignored');
    exit;
  end;
  Debug('copying stream of '+l2s(size)+' bytes');
  inc(size,I.GetPos);
  while I.GetPos < size do begin
    bufsize := 65000;
    if bufsize > size-I.getpos then bufsize := size-i.getpos;
    if bufsize > maxavail then bufsize := maxavail;
    getmem(buf,bufsize);
    i.read(buf^,bufSize);
    if i.status <> stok then debug('read error');
    o.write(buf^,bufSize);
    if o.status <> stok then debug('write error');
    freemem(buf,bufSize);
    if (i.status or o.status) <> 0 then break;
  end;
end;

procedure CopyFile(fn1,fn2:FnameStr);
var
  I,O:TDosStream;
begin
  Debug('copying '+fn1+' to '+fn2);
  I.init(fn1,stOpenRead);
  O.init(fn2,stCreate);
  CopyStream(I,O,I.GetSize);
  I.Done;
  O.Done;
end;

function NewText;
var
  P:PmsgText;
begin
  New(P);
  P^.Next := NIL;
  P^.Size := size;
  GetMem(P^.Data,P^.Size);
  NewText := P;
end;

procedure DisposeText;
begin
  if P^.Next <> NIL then DisposeText(P^.Next);
  FreeMem(P^.Data,P^.Size);
  Dispose(P);
end;

function Stream2Text(var T:TStream; size:longint):PMsgText;
var
  finishPos:longint;
  bufsize:word;
  Pt:PMsgText;
  PPt:PMsgText;
  w:word;
  b:byte;
begin
  finishPos := T.GetPos+size;
  Pt := NIL;
  PPt := NIL;
{  Debug('reading '+l2s(size)+' bytes of text');}
  while T.GetPos < finishpos do begin
    bufsize := 65000;
    if bufsize > finishpos-T.GetPos then bufSize := finishpos-T.GetPos;
    if bufSize > MaxAvail then bufSize := maxavail;
    if Pt = NIL then begin
      Pt := NewText(bufSize);
      PPt := Pt;
    end else begin
      PPt := Pt;
      while PPt^.Next <> NIL do PPt := PPt^.Next;
      PPt^.Next := NewText(bufSize);
      PPt := PPt^.Next;
    end;
    T.Read(PPt^.Data^,bufSize);
    if T.Status <> stOK then begin
      Debug('broken stream');
      break;
    end;
  end;
  if Pt <> NIL then begin
    PPt := Pt;
    while PPt <> NIL do begin
      for w:=0 to PPt^.Size-1 do begin
        b := pos(PPt^.Data[w],WinTransDest);
        if b > 0 then PPt^.Data[w] := TrTransDest[b];
      end;
      PPt := Ppt^.Next;
    end;
  end;
  Stream2Text := Pt;
end;

function GenericInit;
begin
  case what of
    pakBW   : GenericInit := New(PBWPacket,Init(where,''));
    pakQWK  : GenericInit := New(PQWKPacket,Init(where,''));
    pakWham : GenericInit := New(PWHAMPacket,Init(where,''));
    else GenericInit := NIL;
  end; {case}
end;

function GenericReplyInit;
var
  pid:string[8];
begin
  if Packet <> NIL then pid := Packet^.Packetid
                   else pid := '';
  case what of
    pakBW : GenericReplyInit := New(PBWREplyPacket,Init(where,pid));
    pakQWK : GenericReplyInit := New(PREPPacket,Init(where,pid));
    pakWham : GenericReplyINit := New(PWHamPacket,Init(where,pid));
    else GenericReplyInit := NIL;
  end; {case}
end;

procedure ReportMem;
begin
  Debug('MaxAvail = '+l2s(Maxavail)+' - MemAvail: '+l2s(MemAvail));
end;

var
  hebelekHubelek:PDosStream; {!!!}

procedure subWrite(s:string);far;
begin
  SWriteln(hebelekHubelek^,s);
end;

procedure WriteMsgHeaderToTextStream(amsg:Pmsg; var astream:TDosStream);
begin
  hebelekHubelek := @astream;
  WriteMsgHeader(amsg,subwrite);
end;

procedure SaveMsg(amsg:PMsg; atext:PMsgText);
var
  T:TDosStream;
  destfile:FnameStr;
  line:string;
  lastword:string;
  gap:string[3];
  fn:string[12];
  b:byte;
  Pt:PMsgText;
  code:word;
  w:word;
  mode:word;
begin
  mode := stCreate;
  fn := '';
  repeat
    if not InputBox(gtid(msSaveMsgHeader),gtid(msSaveMsgPrompt),0,fntProp,fn,12) then exit;
    destFile := txtDir^+fn;
    if pos('\',fn) > 0 then MessageBox(gtid(msSaveMsgInvalidName),hcSaveMsgInvalidName,mfError) else
    if XFileExists(destFile) then case ExecBox(gtid(msSaveMsgExistsTxt),
                                              gtid(msSaveMsgExistsHdr),0,GetBlock(0,0,mnfHorizontal,
      NewButton(gtid(msSaveMsgExistsAppend),cmNo,
      NewButton(gtid(msSaveMsgExistsDelete),cmYes,
      NewButton(gtid(msSaveMsgExistsNewName),cmOK,
      NewButton(gtid(msSaveMsgExistsCancel),cmCancel,NIL)))))) of
      cmYes : break;
      cmNo : begin
               mode := stOpen;
               break;
             end;
      cmOK : continue;
      else exit;
    end else break;
  until false;
  EventWait;
  T.Init(destFile,mode);
  if T.Status <> stOK then begin
    T.Done;
    FileErrorBox(destFile);
    exit;
  end;
  T.Seek(T.GetSize);
  WriteMsgHeaderToTextStream(amsg,T);
  line := '';
  lastword := '';
  Pt := aText;
  while Pt <> NIL do begin
    for w:=0 to Pt^.Size-1 do case Pt^.Data[w] of
      #13 : begin
              SWriteln(T,line+lastword);
              line := '';
              lastword := '';
            end;
      #32 : if length(line+lastword) > wrapLimit then begin
              SWriteln(T,line);
              line := lastword;
              lastword := '';
            end else begin
              line := line + lastword + #32;
              lastword := '';
            end;
      else lastword := lastword + Pt^.Data[w];
    end;
    Pt := Pt^.next;
  end;
  SWriteln(T,line+lastword);
  T.Done;
end;

procedure AddTearline(fn:FnameStr);
var
  tearline:FnameStr;
  temp:string;
  T:TDosStream;
begin
  EventWait;
  tearline := '--- '+appName+' v'+rVersion+' [Public Domain]';
  Debug('tearing line');
  T.Init(fn,stOpen);
  T.Seek(0);
  while T.GetPos < T.GetSize do begin
    SReadln(T,temp);
    Strip(temp);
    if temp = tearline then begin
      T.Done;
      exit;
    end;
  end;
  T.Reset;
  T.Seek(T.GetSize);
  SWriteln(T,tearline);
  T.Done;
end;

function GetSampleText:PMsgText;
var
  P:PMsgText;
  n:integer;
  totsize:word;
  w:word;
  s:string;
begin
  for n:=msSampleText1 to msSampleText10 do inc(totsize,length(gtid(n)));
  P := NewText(totsize);
  w := 0;
  for n:=msSampleText1 to msSampleText10 do begin
    s := gtid(n);
    Move(s[1],P^.Data[w],length(s));
    inc(w,length(s));
  end;
  GetSampleText := P;
end;

procedure DoFilter;
var
  param:string;
  cmd:string;
  n:byte;
  olddir:string;
  temp:string;
  Filter:PFilter;
  oki:boolean;
  function FindFilter(afiltercmd:fnameStr):PFilter;far;
  var
    subloop:integer;
    P:PFilter;
    b:byte;
    s:string[8];
  begin
    b := pos('.',afiltercmd);
    if b > 0 then afiltercmd := copy(afiltercmd,1,b-1);
    for subloop := 0 to filterList^.Count-1 do begin
      P := filterList^.At(subloop);
      b := pos('.',P^.Cmd);
      if b > 0 then s := copy(p^.cmd,1,b-1) else s := P^.cmd;
      if afiltercmd = s then begin
        FindFilter := P;
        exit;
      end;
    end;
    FindFilter := NIL;
  end;
begin
  param := fn+' '+wkdir^+filterFile;
  for n:=1 to GetParseCount(acmd,'+') do begin
    cmd := GetParse(acmd,'+',n);
    strip(cmd);
    FastUpper(cmd);
    Filter := FindFilter(cmd);
    if Filter <> NIL then begin
      Debug('filtering: '+cmd);
      XDeleteAnyway(wkdir^+filterFile);
      GetDir(0,olddir);
      temp := fltdir^;
      dec(byte(temp[0]));
      ChDir(fltdir^);
      if XExec(Filter^.Cmd,param,(xfDirect*byte(not Filter^.Batch)) or xfHideOutput) then begin
        if XFileExists(wkdir^+filterFile) then begin
          XDeleteAnyway(fn);
          XRenameFile(wkdir^+filterFile,fn);
        end;
      end;
      ChDir(olddir);
    end else Debug('invalid filter: '+cmd);
  end;
end;

procedure CalcUnreads;
var
  n:integer;
  P:PMsg;
  Pa:PArea;
begin
  if not PacketOpen then Abort('CalcUnreads','Unexpected call');
  for n:=0 to Packet^.areaList^.Count-1 do begin
    Pa := Packet^.areaList^.At(n);
    Pa^.Unread := Pa^.Total;
  end;
  for n:=0 to Packet^.msgList^.Count-1 do begin
    P := Packet^.msgList^.At(n);
    if P^.Flags and wmfRead > 0 then dec(P^.Area^.Unread);
  end;
end;

end.