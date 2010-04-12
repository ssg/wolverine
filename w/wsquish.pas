{
Wolverine Squish Message base extensions
}

unit WSquish;

interface

uses WTypes;

implementation

const

  sqFrameId = $AFAE4453;

  {squish frame types}
  sqftNormal  = 0;
  sqftFree    = 1;
  sqftLZSS    = 2;
  sqftLocked  = 3; {frame is being used by another task}

  sqmaPrivate = 1;
  sqmaCrash   = 2;
  sqmaRead    = 4;
  sqmaSent    = 8;
  sqmaFile    = 16; {filename is in the subject}
  sqmaFwd     = 32;
  sqmaOrphan  = 64;
  sqmaKill    = 128;
  sqmaLocal   = 256;
  sqmaHold    = 512;
  sqmaFRq     = 2048; {file request... name is in the subject}
  sqmaRRq     = 4096;
  sqmaCpt     = 8192; {return receipt}
  sqmaARq     = 16384; {audit trail}
  sqmaURq     = 32768; {update request... name is in the subject field}
  sqmaScanned = 65536;
  sqmaMSGUID  = $20000; {uid field is valid in TXMSG}

type

  UMSGID = longint;

  TSQHdr = record
    Ofs          : longint;
    UId          : UMSGID;
    Hash         : longint;
  end;

  TSQMsg = record
    Attr         : longint; {message attributes}
    From         : array[1..36] of char;
    Too          : array[1..36] of char;
    Subject      : array[1..72] of char;
    Orig         : TAddr;
    Dest         : TAddr;
    DateWritten  : TDOSDateTime;
    DateArrived  : TDOSDateTime;
    UTCOfs       : integer; {utc offset}
    ReplyTo      : UMSGID;
    Replies      : array[1..9] of UMSGID;
    UId          : UMSGID;
    FTSCDate     : array[1..20] of char;
  end;

  TSQFrame = record
    Id          : longint;
    NextFrame   : longint; {0 if no next}
    PrevFrame   : longint; {0 if no prev}
    FrameLength : longint; { length of frame }
    MsgLength   : longint; { length of data in frame }
    CLen        : longint; { control information field length }
    FrameType   : word;
    Reserved    : word;
  end;

  TSQBase = record
    Len            : word;
    Reserved1      : word;
    NumMsg         : longint; {number of messages in base}
    HighMsg        : longint; {highest message number (equals to nummsg}
    SkipMsg        : longint;
    HighWater      : longint; {UMSGID of highest scanned mail in echo area}
    UID            : longint; {next uid assigned to the message}
    Base           : array[1..80] of char; {message base name (w/o extension)}
    BeginFrame     : longint; {first frame in message chain}
    LastFrame      : longint;
    FreeFrame      : longint; {first free frame offset}
    LastFreeFrame  : longint;
    EndFrame       : longint; {eof offset}
    MaxMsg         : longint; {maximum number of messages for this area}
    KeepDays       : word;    {maximum age of messages in this area}
    SzSQHdr        : word;    {size of sqhdr field}
    Reserved2      : array[1..124] of byte;
  end;

end.