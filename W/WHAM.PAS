{
Wham Packet implementation

updates:
--------
 7th Aug 96 - 23:52 - almost ok...
12th Aug 96 - 12:17 - seems like finished...
 4th Sep 96 - 02:32 - Some fixes...
}

unit Wham;

interface

uses

Objects,WTypes;

const

  WHAMSign = $4D414857;  { wham signature }

  wptNormal    = 0;  { normal mail packet }
  wptReply     = 1;  { reply packet }
  wptArchive   = 2;  { archive packet }

  wpfEncrypted = 1;  { packet has been encrypted - n/a yet }

type

  TWHAMInfo = record
    Sign     : longint;      { wham signature }
    Version  : byte;         { packet version }
    PakType  : byte;         { packet type }
    Flags    : word;         { packet flags }
    Netflags : word;         { allowed netmail flags }
    System   : string[39];   { system name which packet has been created on }
    SysOp    : string[39];   { sysop of system }
    Addr     : TAddr;        { netmail addr of system }
    Owner    : string[39];   { name of the packet owner }
    Alias    : string[39];   { alias of packet owner }
    ProgName : string[39];   { program which created the packet }
    Pad      : array[1..294] of byte; {512 bytes}
  end;

  TWHAMArea = record
    Name     : string[49];
    Number   : word;
    AreaType : byte;
    UserStat : byte;
    Flags    : word;
    Total    : word;
    Pers     : word;
    Null     : array[1..66] of byte; {padding }
  end;

  TWHAMIndex = record
    From     : string[39];
    Too      : string[39];
    Subj     : string[79];
    Area     : word;
    Date     : longint;
    Origin   : TAddr;
    Dest     : TAddr;
    Flags    : word;
    mFlags   : byte;
    mMarks   : byte;
    Size     : longint;
    Where    : longint;
    Filename : string[12];
    Null     : array[1..49] of byte; {padding}
  end;

  PWHAMPacket = ^TWHAMPacket;
  TWHAMPacket = object(TMsgPacket)
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
  end;

implementation

uses XIO,WProcs,XBuf,XStr,XDebug;

const

  infoFile  : string[12] = 'Info.W';
  indexFile : string[12] = 'Index.W';
  textFile  : string[12] = 'Text.W';

procedure WHAM2Msg(var wm:TWHAMIndex; var msg:TMsg);
begin
  ClearBuf(msg,SizeOf(msg));
  with wm do begin
    Msg.From := From;
    Msg.Too  := Too;
    Msg.Subj := Subj;
    Msg.Addr := Origin;
    Msg.Date := Date;
    Msg.WHere  := Where;
    Msg.Length := Size;
    Msg.Flags  := mFlags;
    Msg.Marks  := mMarks;
  end;
end;

{- TWHAMPacket -}

function TWHAMPacket.Load;
var
  T:TDosStream;
  rec:TWHAMInfo;
  areaRec:TWHAMArea;
  msgrec:TWHAMIndex;
  Pm:PMsg;
  Pa:PArea;
  curfti:word;
  realPacket:boolean;
  function mytest(Ph:PArea):boolean;far;
  begin
    mytest := Ph^.Number = msgrec.Area;
  end;
begin
  Load := false;
  Config := mpcInfo;
  T.Init(Where+infoFile,stOpenRead);
  if T.Status <> stOK then begin
    Debug('info open error');
    T.Done;
    exit;
  end;
  T.Read(rec,SizeOf(rec));
  if T.Status <> stOK then begin
    Debug('info read error');
    T.Done;
    exit;
  end;
  case rec.PakType of
    wptReply : Config := Config or mpcCanPost or mpcCanDelete or mpcCanOLC;
    wptArchive : Config := Config or mpcCanPost or mpcCanDelete;
  end; {case}
  if rec.Sign <> WHAMSign then begin
    Debug('signature mismatch');
    T.Done;
    exit;
  end;
  if rec.Flags and wpfEncrypted > 0 then begin
    Debug('encrypted packet - cannot handle this');
    T.Done;
    exit;
  end;
  with user do begin
    Name := rec.Owner;
    Alias := rec.Alias;
    SysOp := rec.SysOp;
    BBS := rec.System;
    Netflags := rec.Netflags;
  end;
  Debug(user.BBS+' loading');
  while T.GetPos < T.GetSize do begin
    T.Read(areaRec,SizeOf(areaRec));
    if T.Status <> stOK then begin
      Debug('area broken');
      T.Done;
      exit;
    end;
    New(Pa);
    ClearBuf(Pa^,SizeOf(TArea));
    with Pa^ do begin
      Number  := areaRec.Number;
      Name    := areaRec.Name;
      Total   := areaRec.Total;
      Pers    := areaRec.Pers;
      Flags   := areaRec.Flags;
      UserStat := areaRec.UserStat;
      AreaType := areaRec.areaType;
      EchoTag := 'shit';
    end;
    arealist^.Insert(Pa);
  end;
  T.Done;
  T.Init(Where+indexFile,stOpenRead);
  realPacket := T.Status = stOK;
  if realPacket then begin
    curfti := 0;
    while T.GetPos < T.getSize do begin
      T.Read(msgrec,SizeOf(msgrec));
      if msgrec.Flags and wmfDeleted = 0 then begin
        New(Pm);
        WHAM2Msg(msgrec,Pm^);
        Pm^.Area := arealist^.FirstThat(@mytest);
        if Pm^.Area = NIL then Debug('orphaned message!');
        Pm^.FTINum := curfti;
        inc(curfti);
        msglist^.Insert(Pm);
      end else Debug('deleted message encountered');
    end;
  end;
  T.Done;
  Load := true;
end;

function TWHAMPacket.ReadMsgText(amsg:PMsg):PMsgText;
var
  T:TDosStream;
begin
  T.Init(Where+textFile,stOpenRead);
  T.Seek(amsg^.Where);
  ReadMsgText := stream2Text(T,amsg^.Length);
  T.Done;
end;

procedure TWHAMPacket.PostMsg;
var
  I,O:TDosStream;
  fn:FnameStr;
begin
  if Config and mpcCanPost = 0 then exit;
  I.Init(afile,stOpenRead);
  fn := XGetUniqueName(Where,'.WM');
  O.Init(Where+fn,stCreate);
  CopyStream(I,O,I.getSize);
  I.Done;
  O.Done;
  amsg^.Filename := fn;
  WriteMsgHeader(amsg,true);
end;

procedure GetWHAMInfo(var rec:TWHAMInfo);
begin
  ClearBuf(rec,SizeOf(rec));
  with rec do begin
    Sign     := WHAMSign;
    Flags    := 0;
    Netflags := 0;
    System   := user.BBS;
    SysOp    := user.SysOp;
    Owner    := user.Name;
    Alias    := user.Alias;
    ProgName := appName+' '+rVersion;
  end;
end;

procedure Reply2WHAM(var rr:TMsg; var wm:TWHAMIndex);
begin
end;

procedure TWHAMPacket.WriteMsgHeader;
var
  T:TDosStream;
  rec:TWHAMIndex;
  h:TWHAMInfo;
begin
  if Config and mpcCanPost = 0 then exit;
  if not XFileExists(Where+infoFile) then begin
    if not anew then Abort('TWHAMPacket.WriteReplyHeader','Inconsistency!');
    Debug('creating new infoFile');
    GetWHAMInfo(h);
    T.Init(Where+infoFile,stCreate);
    T.Write(h,SizeOf(h));
    T.Done;
    T.Init(Where+indexFile,stCreate);
  end else T.Init(Where+indexFile,stOpen);
  if anew then T.Seek(T.GetSize) else begin
    while T.GetPos < T.GetSize do begin
      T.Read(rec,SizeOf(rec));
      if rec.Filename = amsg^.Filename then begin
        T.Seek(T.GetPos-SizeOf(rec));
        break;
      end;
    end;
  end;
  Reply2WHAM(amsg^,rec);
  T.Write(rec,SizeOf(rec));
  T.Done;
end;

procedure TWHAMPacket.MsgToFile;
begin
  CopyFile(Where+amsg^.Filename,afile);
end;

procedure TWHAMPacket.FileToMsg;
begin
  CopyFile(afile,Where+amsg^.Filename);
end;

procedure TWHAMPacket.DeleteMsg;
begin
end;

procedure TWHAMPacket.UpdateMsgFlags;
var
  T:TDosStream;
  apos:longint;
  rec:TWHAMIndex;
begin
  T.Init(Where+indexFile,stOpen);
  apos := amsg^.FTINum*SizeOf(TWHAMIndex);
  T.Seek(apos);
  T.Read(rec,SizeOf(rec));
  rec.mFlags := amsg^.Flags;
  rec.mMarks := amsg^.Marks;
  T.Seek(apos);
  T.Write(rec,SizeOf(rec));
  T.Done;
end;

function TWHAMPacket.GetType;
begin
  GetType := pakWHAM;
end;

procedure GetReplyHeader(var hdr:TWHAMInfo);
begin
  ClearBuf(hdr,SizeOf(hdr));
  with hdr do begin
    Sign    := WHAMSign;
    PakType := wptReply;
    Owner   := user.Name;
    Alias   := user.Alias;
    ProgName := appName+' v'+rVersion;
  end;
end;

procedure Area2WHAM(var a:TArea; var w:TWHAMArea);
begin
  ClearBuf(w,SizeOf(w));
  with w do begin
    Name := a.Name;
    Number := a.Number;
    AreaType := a.AreaType;
    UserStat := a.UserStat;
    Flags    := a.Flags;
  end;
end;

procedure TWHAMPacket.OLC;
var
  T:TDosStream;
  h:TWHAMInfo;
  rec:TWHAMArea;
  n:integer;
  P:PArea;
begin
  T.Init(Where+infoFile,stOpen);
  if T.Status <> stOK then begin
    T.Init(Where+infoFile,stCreate);
    GetReplyHeader(h);
    T.Write(h,SizeOf(h));
  end else T.Seek(SizeOf(h));
  for n:=0 to areaList^.Count-1 do begin
    P := areaList^.At(n);
    if (P^.AreaType <> watInfo) and (P^.UserStat <> wusNone) then begin
      Area2WHAM(P^,rec);
      T.Write(rec,SizeOf(rec));
    end;
  end;
  T.Done;
end;

end.