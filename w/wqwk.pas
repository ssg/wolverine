{ Wolverine QWK packet extensions

updates:
--------
20st Jun 96 - 23:46 - Made code more stable...
 2nd Jul 96 - 14:23 - Aferim leen...
 7th Aug 96 - 23:51 - adapted to wham...
}

{$C MOVEABLE DEMANDLOAD DISCARDABLE}
{$O+}

unit WQWK;

interface

uses Objects,WTypes;

const

  qwkActive = #225;
  qwkKilled = #226;

type

  TQWKMsgRec = record
    Status     : char;
    Num        : array[1..7] of char;
    Date       : array[1..8] of char;
    Time       : array[1..5] of char;
    Too        : array[1..25] of char;
    From       : array[1..25] of char;
    Subj       : array[1..25] of char;
    Passwd     : array[1..12] of char;
    MsgNum     : array[1..8] of char;
    Blocks     : array[1..6] of char; {num of blocks plus 1}
    MsgActive  : char;
    AreaNum    : word;
    LogicalNum : word;
    NetTag     : char;
  end;

  PQWKPacket = ^TQWKPacket;
  TQWKPacket = object(TMsgPacket)
    constructor Init(awhere,apacketid:FnameStr);
    function   Load:boolean;virtual;
    function   ReadMsgText(amsg:PMsg):PMsgText;virtual;
    function   GetType:TPakType;virtual;
    procedure  UpdateMsgFlags(amsg:PMsg);virtual;
  end;

  PREPPacket = ^TREPPacket;
  TREPPacket = object(TQWKPacket)
    constructor Init(awhere,apacketid:FnameStr);
    function    Load:boolean;virtual;
    function    GetType:TPakType;virtual;
    procedure   PostMsg(amsg:PMsg; afile:FnameStr);virtual;
    procedure   WriteMsgHeader(amsg:Pmsg; anew:boolean);virtual;
    procedure   MsgToFile(amsg:Pmsg; afile:FnameStr);virtual;
    procedure   FileToMsg(afile:FnameStr; amsg:PMsg);virtual;
    procedure   DeleteMsg(amsg:Pmsg);virtual;
  end;

implementation

uses WProcs,XDebug,Debris,XIO,Dos,XStr,XGfx,XStream,XBuf;

const

  newsFile   : string[12] = 'NEWFILES.DAT';

procedure q2t(var buf; size:word);assembler;
asm
  mov  cx,size
  les  di,buf
@loop:
  mov  al,es:[di]
  cmp  al,227
  jne  @skip
  mov  byte ptr es:[di],13
@skip:
  inc  di
  loop @loop
@exit:
end;

procedure QWK2Text(text:PMsgText);
begin
  Debug('converting QWK text');
  while text <> NIL do begin
    q2t(text^.Data^,text^.Size);
    text := text^.next;
  end;
end;

procedure t2q(var buf; size:word);assembler;
asm
  mov  cx,size
  les  di,buf
@loop:
  mov  al,es:[di]
  cmp  al,13
  jne  @skip
  mov  byte ptr es:[di],227
@skip:
  inc  di
  loop @loop
@exit:
end;

procedure Text2QWK(text:PMsgText);
begin
  Debug('converting text to QWK');
  while text <> NIL do begin
    t2q(text^.Data^,text^.Size);
    text := text^.next;
  end;
end;

function StrQWKDateToUnix(date:FnameStr; time:FnameStr):longint;
var
  T:DateTime;
  l:longint;
  b:byte;
begin
  T.Month := s2l(copy(date,1,2));
  T.Day   := s2l(copy(date,4,2));
  T.year  := (s2l(copy(date,7,2)))+1900;
  T.Hour  := s2l(copy(time,1,2));
  T.Min   := s2l(copy(time,4,2));
  PackTime(t,l);
  StrQWKDateToUnix := l;
end;

function Char2Str(P:PChar; len:byte):string;
var
  s:string;
  b:byte;
begin
  s := '';
  for b:=0 to len-1 do begin
    inc(byte(s[0]));
    s[length(s)] := P[b];
  end;
  Strip(s);
  Char2Str := s;
end;

function QWK2Msg(var aqwk:TQWKMsgRec; var xti:TXTIRec):PMsg;
var
  Pm:PMsg;
begin
  New(Pm);
  Pm^.Flags := xti.Flags;
  Pm^.Marks := xti.marks;
  if aqwk.Status in ['-','*','+','`','^','#'] then Pm^.Flags := wmfRead;
  Pm^.From  := Char2Str(@aqwk.From,25);
  Pm^.Too   := Char2Str(@aqwk.Too,25);
  Pm^.Subj  := Char2Str(@aqwk.Subj,25);
  Pm^.Date  := StrQWKDateToUnix(Char2Str(@aqwk.Date,8),Char2Str(@aqwk.Time,5));
  Pm^.FtiNum := aqwk.LogicalNum;
  ClearBuf(Pm^.Addr,SizeOf(TAddr));
  Pm^.Length := (s2l(Char2Str(@aqwk.Blocks,6))-1)*128;
  QWK2Msg := Pm;
end;

{- TQWKPacket -}

function TQWKPacket.Load;
var
  Control:TDosStream;
  T,X:TDosStream;
  xti:TXTIRec;
  usexti:boolean;
  totalconf:integer;
  n:integer;
  temp:string;
  Pa:PArea;
  msgrec:TQWKMsgRec;
  Pm:PMsg;
  ftinum:longint;
  controlstatus:boolean;
  function GetControlLine:string;
  var
    s:string;
  begin
    if not controlstatus then exit;
    SReadln(Control,s);
    if Control.Status <> stOK then begin
      s := '';
      Debug('control read error');
      controlstatus := false;
      exit;
    end;
    Strip(s);
    GetControlLine := s;
  end;
  procedure SkipLines(lines:byte);
  var
    s:string;
  begin
    if not controlstatus then exit;
    while lines > 0 do begin
      SReadln(Control,s);
      if Control.Status <> stOK then begin
        Debug('control seek error');
        controlstatus := false;
        exit;
      end;
      dec(lines);
    end;
  end;
  function MesajAlanIliskisi(hebe:PArea):boolean;far;
  var
    b:boolean;
  begin
    b := msgrec.areanum = hebe^.Number;
    if b then begin
      inc(hebe^.Total);
      if upper(Pm^.Too) = Upper(user.Name) then inc(hebe^.Pers);
    end;
    MesajAlanIliskisi := b;
  end;
  procedure CloseAllStreams;
  begin
    T.Done;
    X.DOne;
  end;
begin
  Config := Config or mpcInfo;
  Load := false;
  Control.Init(Where+'CONTROL.DAT',stOpenRead);
  if Control.Status <> stOK then begin
    Control.Done;
    Debug('control init error');
    exit;
  end;
  controlstatus := true;
  user.Alias := '';
  user.Netflags := 0;
  user.BBS := GetControlLine;
  SkipLines(2);
  user.SysOp := GetControlLine;
  PacketId := GetControlLine;
  SkipLines(1);
  user.Name := GetControlLine;
  SkipLines(3);
  temp := GetControlLine;
  totalconf := s2l(temp);
  if not controlstatus then begin
    Control.Done;
    Debug('qwk info corrupt');
    exit;
  end;
  Debug(user.BBS+' loading');
  for n:=0 to totalconf do begin
    New(Pa);
    Pa^.Number  := s2l(GetControlLine);
    Pa^.Name    := GetControlLine;
    Pa^.EchoTag := 'SHIT';
    Pa^.Total   := 0;
    Pa^.Pers    := 0;
    Pa^.Unread  := 0;
    Pa^.UserStat := wusAll;
    Pa^.AreaType := watEcho;
    Pa^.Flags   := wafPost;
    arealist^.Insert(pa);
  end;
  Control.Done;
  Debug('loading messages');
  T.Init(Where+'MESSAGES.DAT',stOpenRead);
  if T.Status <> stOK then begin
    T.Done;
    Debug('messages init error');
    exit;
  end;
  T.Seek(128);
  X.Init(Where+PacketId+'.XTI',stOpenRead);
  usexti := X.Status = stOK;
  ClearBuf(xti,SizeOf(xti));
  if not usexti then X.Done else Debug('utilizing XTI');
  ftinum := 0;
  while T.GetPos < T.GetSize do begin
    Cycle;
    T.Read(msgrec,SizeOf(msgrec));
    if T.Status <> stOK then begin
      T.Done;
      if usexti then X.Done;
      Control.Done;
      Debug('stream read error');
      exit;
    end;
    if usexti then begin
      X.Read(xti,SizeOf(xti));
      if X.Status <> stOK then begin
        usexti := false;
        X.Done;
        ClearBuf(xti,SizeOf(xti));
      end;
    end;
    Pm := QWK2Msg(msgrec,xti);
    Pm^.Where := T.GetPos;
    inc(ftinum);
    Pm^.Area := arealist^.FirstThat(@MesajAlanIliskisi);
    if Pm^.Flags and wmfRead = 0 then inc(Pm^.Area^.Unread);
    MsgList^.Insert(Pm);
    T.Seek(Pm^.Where+Pm^.Length);
    if T.Status <> stOK then begin
      Debug('overseek');
      CloseAllStreams;
      exit;
    end;
  end;
  CloseAllStreams;
  if XFileExists(Where+newsFile) then AddInfo(newsFile,user.BBS+' - yeni dosyalar');
  Load := true;
  Debug('load successful');
end;

function GetQWKText(afile:FnameStr; amsg:PMsg):PMsgText;
var
  T:TDosStream;
  Pt:PMsgText;
begin
  T.init(afile,stOpenRead);
  T.Seek(amsg^.Where);
  pt := Stream2Text(T,amsg^.Length);
  QWK2Text(pt);
  GetQWKText := Pt;
  T.Done;
end;

function TQWKPacket.ReadMsgText(amsg:PMsg):PMsgText;
begin
  ReadMsgtext := GetQWKText(Where+'MESSAGES.DAT',amsg);
end;

constructor TREPPacket.Init;
begin
  inherited INit(Awhere,apacketid);
  Config := Config or mpcCanPost or mpcCanDelete or mpcCanOLC;
end;

function TREPPacket.Load;
var
  P:PMsg;
  T:TDosStream;
  rec:TQWKMsgRec;
  xti:TXTIRec;
  ftinum:longint;
  function Test(aa:PArea):boolean;
  begin
    Test := aa^.Number = rec.AreaNum;
  end;
begin
  Debug('loading replies');
  T.Init(Where+PacketId+'.MSG',stOpenRead);
  if T.Status <> stOK then begin
    T.Done;
    exit;
  end;
  if T.GetSize > 256 then begin
    T.Seek(128);
    ClearBuf(xti,SizeOf(xti));
    ftinum := 0;
    while T.GetPos < T.GetSize do begin
      T.Read(rec,SizeOf(rec));
      if T.Status <> stOK then begin
        Debug('read error');
        break;
      end;
      P := QWK2Msg(rec,xti);
      Debug('reply to '+P^.Too);
      P^.Where := T.GetPos;
      if rec.MsgActive = qwkActive then begin
        P^.Area  := areaList^.FirstThat(@Test);
        msglist^.Insert(P);
      end else Debug('reply is killed - skipped');
      if p^.length < 0 then begin
        Debug('negative rep length error');
        break;
      end;
      if T.GetPos+p^.length > t.getsize then begin
        Debug('corrupt reply packet');
        break;
      end;
      T.Seek(T.GetPos+p^.length);
    end;
  end else Debug('too small reply packet');
  T.Done;
  Debug(l2s(msglist^.Count)+' replies read');
end;

procedure TREPPacket.PostMsg(amsg:PMsg; afile:FnameStr);
var
  I,O:TDosStream;
  temp:array[1..128] of char;
  tempSize:word;
begin
  I.Init(afile,stOpenRead);
  tempSize := 128-(I.GetSize mod 128);
  amsg^.Length := I.GetSize+tempSize;
  WriteMsgHeader(amsg,true);
  O.Init(Where+PacketId+'.MSG',stOpen);
  O.Seek(O.GetSize);
  CopyStream(I,O,I.GetSize);
  if tempSize > 0 then begin
    PadBuf(temp,SizeOf(temp));
    O.Write(temp,tempSize);
  end;
  I.Done;
  O.Done;
end;

procedure PutStr(s:string; p:PChar; maxlen:byte);
begin
  FastFix(s,maxlen);
  Move(s[1],p^,maxlen);
end;

procedure Msg2QWK(amsg:PMsg; var rec:TQWKMsgRec);
var
  l:longint;
  tempDate:DateTime;
begin
  PadBuf(rec,SizeOf(rec));
  rec.Status := ' '; {public/unread}
  PutStr(amsg^.From,@rec.From,25);
  PutStr(amsg^.Too,@rec.Too,25);
  PutStr(amsg^.Subj,@rec.Subj,25);
  PutStr(l2s((amsg^.Length div 128)+1),@rec.Blocks,6);
  LongRec(l).Lo := GetSysTime;
  LongRec(l).Hi := GetSysDate;
  UnPackTime(l,tempDate);
  with TempDate do begin
    PutStr(z2s(Month,2)+'-'+z2s(Day,2)+'-'+z2s(Year-1900,2),@rec.Date,8);
    PutStr(z2s(Hour,2)+':'+z2s(Min,2),@rec.Time,5);
  end;
  PutStr(l2s((amsg^.Length div 128)+1),@rec.Blocks,6);
  rec.msgActive := qwkActive;
  rec.AreaNum   := amsg^.Area^.Number;
  rec.LogicalNum := amsg^.FTINum;
  rec.NetTag     := ' '; {no network tagline??}
end;

function MatchQWKMsg(var O:TStream; var rec:TQWKMsgRec):boolean;
var
  rec2:TQWKMsgRec;
begin
  O.Seek(128);
  while O.GetPos < O.GetSize do begin
    O.Read(rec2,SizeOf(rec2));
    with rec2 do if rec2.LogicalNum = rec.LogicalNum then begin
      O.Seek(O.GetPos-SizeOf(rec2));
      MatchQWKMsg := true;
      exit;
    end;
  end;
  MatchQWKMsg := false;
end;

procedure TREPPacket.WriteMsgHeader;
var
  O:TDosStream;
  rec:TQWKMsgRec;
  temp:array[1..128] of char;
begin
  Debug('writing '+GetBool(anew,'new','old')+' reply header');
  O.Init(Where+PacketId+'.MSG',stOpen);
  if O.Status <> stOK then begin
    O.Done;
    if not anew then Abort('TQWKPacket.WriteReplyHeader','inconsistency');
    Debug('creating new reply file');
    O.Init(Where+PacketId+'.MSG',stCreate);
    if O.status <> stOK then begin
      Debug('create error');
      O.Done;
      exit;
    end;
    PadBuf(temp,SizeOf(temp));
    Move(PacketId[1],temp,length(PacketId));
    O.Write(temp,SizeOf(temp));
  end;
  msg2QWK(amsg,rec);
  if not anew then begin
    Debug('looking for old header');
    if matchQWKMsg(O,rec) then begin
      Debug('found old header');
      O.Write(rec,SizeOf(rec));
      Debug('old header updated');
      exit;
    end else Abort('TQWKPacket.WriteReplyHeader','Couldn''t find match for reply');
  end;
  O.Seek(O.GetSize);
  O.Write(rec,SizeOf(rec));
  O.Done;
  Debug('reply header written');
end;

procedure TREPPacket.MsgToFile;
var
  I,O:TDosStream;
  buf:pointer;
  bufSize:word;
  finishPos:longint;
begin
  Debug('reply to file begin');
  I.Init(Where+PacketId+'.MSG',stOpenRead);
  O.init(afile,stCreate);
  Debug('reply is at '+l2s(amsg^.Where));
  I.Seek(amsg^.Where);
  if I.Status <> stOK then Debug('stream failed. code = '+l2s(I.Status));
  finishPos := amsg^.Where+amsg^.Length;
  Debug('finishpos = '+l2s(finishpos));
  if finishPos > I.GetSize then Abort('TQWKPacket.ReplyToFile','overfinish... file size is '+l2s(I.GetSize));
  while I.GetPos < finishPos do begin
    bufSize := 65000;
    if bufSize > finishPos-I.GetPos then bufSize := finishPos-I.GetPos;
    if bufSize > maxAvail then bufSize := maxavail;
    Debug('bufsize = '+l2s(bufSize));
    GetMem(buf,bufSize);
    I.Read(buf^,bufSize);
    q2t(buf^,bufSize);
    O.Write(buf^,bufSize);
    FreeMem(buf,bufSize);
  end;
  I.Done;
  O.Done;
  Debug('reply to file success');
end;

{arep^.length must be original - it is updated by this procedure }

function comp128(l:longint):longint;
begin
  comp128 := l + (128-(l mod 128));
end;

procedure TREPPacket.FileToMsg;
var
  I,O:TDosStream;
  rec:TQWKMsgRec;
  curpos:longint;
  buf:pointer;
  bufSize:word;
  temp:array[1..128] of char;
begin
  Debug('file to reply begin');
  I.Init(Where+Packetid+'.MSG',stOpenRead);
  msg2QWK(amsg,rec);
  if not matchQWKmsg(I,rec) then Abort('TQWKPacket.FileToReply','match not found');
  O.Init(Where+'hebe.tmp',stCreate);
  curpos := I.GetPos;
  I.Seek(0);
  CopyStream(I,O,curpos);
  I.Seek(curpos+amsg^.length+128);
  CopyStream(I,O,I.GetSize-I.GetPos);
  I.Done;
  Debug('exclusion successful');
  I.Init(afile,stOpenRead);
  amsg^.Length := comp128(I.GetSize);
  msg2QWK(amsg,rec);
  O.Write(rec,SizeOf(rec));
  while I.GetPos < I.GetSize do begin
    bufSize := 65000;
    if bufSize > I.GetSize-I.GetPos then bufSize := I.GetSize-I.GetPos;
    if bufSize > maxAvail then bufSize := maxAvail;
    GetMem(buf,bufSize);
    I.Read(buf^,bufSize);
    t2q(buf^,bufSize);
    O.Write(buf^,bufSize);
  end;
  padbuf(temp,SizeOf(temp));
  O.Write(temp,amsg^.Length-I.getSize);
  I.Done;
  O.Done;
  XRenameAnyway(Where+'hebe.tmp',Where+Packetid+'.MSG');
  Debug('file to reply successful');
end;

procedure TREPPacket.DeleteMsg;
var
  I,O:TDosStream;
  rec:TQWKMsgRec;
  curpos:longint;
begin
  Debug('DeleteReply begins');
  I.Init(Where+Packetid+'.MSG',stOpenRead);
  Msg2QWK(amsg,rec);
  if not MatchQWKMsg(I,rec) then Abort('TQWKPacket.DeleteReply','no match');
  curpos := I.getPos;
  I.Seek(0);
  O.Init(Where+'hebe.tmp',stCreate);
  CopyStream(I,O,curpos);
  I.Seek(I.getPos+amsg^.Length);
  CopyStream(I,O,I.getSize-I.GetPos);
  I.Done;
  O.Done;
  XRenameAnyway(Where+'hebe.tmp',Where+packetid+'.MSG');
  Debug('Deletereply success');
end;

procedure TQWKPacket.UpdateMsgFlags;
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

function TQWKPAcket.GetType;
begin
  GetType := pakQWK;
end;

constructor TQWKPacket.Init;
begin
  inherited Init(awhere,apacketid);
  Config := mpcInfo;
end;

function TREPPacket.GetType;
begin
  GetType := pakQWK;
end;

end.