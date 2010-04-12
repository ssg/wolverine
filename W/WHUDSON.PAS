{
Wolverine Hudson Message Base Extensions - 13th Dec 96 - 03:46 - SSG
}

unit WHudson;

interface

uses WTypes;

type

  PHudsonBase = ^THudsonBase;
  THudsonBase = object(TObject)
    constructor Init(awhere,apacketid:FnameStr);
    function   Load:boolean;virtual;
    function   ReadMsgText(amsg:PMsg):PMsgText;virtual;
    procedure  MsgToFile(amsg:Pmsg; afile:FnameStr);virtual;
  end;

implementation

const

  maDeleted =       1;                 {Message is deleted}
  maUnmovedNet =    2;                 {Unexported Netmail message}
  maNetMail =       4;                 {Message is netmail message}
  maPriv =          8;                 {Message is private}
  maRcvd =         16;                 {Message is received}
  maUnmovedEcho =  32;                 {Unexported Echomail message}
  maLocal =        64;                 {"Locally" entered message}

  naKillSent =      1;                 {Delete after exporting}
  naSent =          2;                 {Msg has been sent}
  naFAttach =       4;                 {Msg has file attached}
  naCrash =         8;                 {Msg is crash}
  naReqRcpt =      16;                 {Msg requests receipt}
  naReqAudit =     32;                 {Msg requests audit}
  naRetRcpt =      64;                 {Msg is a return receipt}
  naFileReq =     128;                 {Msg is a file request}

type

  TMsgTxtRec   = string[255];

  TMsgToIdxRec = string[35];

  TMsgInfo = record
    LowMsg,HighMsg : word;
    Active         : word;
    AreaActive     : array[1..200] of word; {msgs active in each area}
  end;

  TMsgIdxRec = record
    MsgNum   : word;
    Area     : byte;
  end;

  TMsgHdr = record
    MsgNum : word;
    ReplyTo: Word;                       {Message is reply to this number}
    SeeAlso: Word;                       {Message has replies}
    Extra: Word;                         {No longer used}
    StartRec: Word;                      {starting seek offset in MsgTxt.Bbs}
    NumRecs: Word;                       {number of MsgTxt.Bbs records}
    DestNet: Integer;                    {NetMail Destination Net}
    DestNode: Integer;                   {NetMail Destination Node}
    OrigNet: Integer;                    {NetMail Originating Net}
    OrigNode: Integer;                   {NetMail Originating Node}
    DestZone: Byte;                      {NetMail Destination Zone}
    OrigZone: Byte;                      {NetMail Originating Zone}
    Cost: Word;                          {NetMail Cost}
    MsgAttr: Byte;                       {Message attribute - see constants}
    NetAttr: Byte;                       {Netmail attribute - see constants}
    Area: Byte;                          {Message area}
    Time: String[5];                     {Message time in HH:MM}
    Date: String[8];                     {Message date in MM-DD-YY}
    MsgTo: String[35];                   {Message is intended for}
    MsgFrom: String[35];                 {Message was written by}
    Subj: String[72];                    {Message subject}
  end;

  TLastRead = array[1..200] of word;

const

  areaFile    : string[12] = 'AREAS.BBS';
  msginfoFile : string[12] = 'MSGINFO.BBS';

constructor THudsonBase.Init;
begin
  inherited Init(awhere,apacketid);
  Config := 0;
end;

function THudsonBase.Load;
var
  T:TDosStream;
  arearec:string[35];
  mirec:TMsgInfo;
  b:byte;
  Pa:PArea;
begin
  Load := false;
  Debug('hudson base loading');
  with user do begin
    Name  := '?';
    Alias := '?';
    SysOp := '?';
    BBS   := '?';
    NetFlags := 0;
  end;
  T.Init(Where + msginfoFile,stOpenRead);
  T.Read(mirec,SizeOf(mirec));
  T.Done;
  T.Init(Where + areaFile,stOpenRead);
  for b:=1 to 200 do begin {areas}
    if (highmsg > lowmsg) then begin
      New(Pa);
      Pa^.Name   := 'Area #'+l2s(b);
      Pa^.Number := b;
      Pa^.AreaType := watEcho;
      Pa^.Flags    := 0;
      Pa^.Pers     := 0;
      Pa^.Unread   :=
    if mirec.AreaActive[
  end;
end;

function THudsonBase.ReadMsgText;
begin
end;

procedure THudsonBase.MsgToFile;
begin
end;

end.