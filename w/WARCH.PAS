{ Wolverine Archive Extensions }

{$O+}

unit WArch;

interface

uses Objects,WProcs,WTypes;

type

  TArchiver = record
    Name       : string[8];
    Sign       : string[20];
    PackCmd    : FnameStr;
    UnPackCmd  : FnameStr;
    ShowOutput : boolean;
  end;

const

  maxArchivers = 5;
  Archivers : array[1..maxArchivers] of TArchiver =
  ((Name:'ZIP';
    Sign:'504B0304';
    PackCmd:'pkzip @a @f';
    UnPackCmd:'pkunzip @a @d';
    ShowOutput:false),

   (Name:'ARJ';
    Sign:'60EA';
    PackCmd:'arj a @a @f';
    UnPackCmd:'arj e @a @d';
    ShowOutput:false),

   (Name:'LHA';
    Sign:'? ? 2D6C68';
    PackCmd:'lha a @a @f';
    UnPackCmd:'lha e @a @d';
    ShowOutput:false),

   (Name:'ARC';
    Sign:'1A';
    PackCmd:'arc a @a @f';
    UnPackCmd:'arc e @a @d';
    ShowOutput:false),

   (Name:'RAR';
    Sign:'526172211A';
    PackCmd:'rar a -std @a @f';
    UnPackCmd:'rar e -std @a @d';
    ShowOutput:false));

function UnpackArchive(archive,destdir:FnameStr):boolean;
function PackArchive(archive,filespec:FnameStr):boolean;
procedure InitArchivers;
procedure WriteArchivers;

implementation

uses XIO,XDebug,XStr;

function DetectArchive(afile:FnameStr):byte;
var
  T:TDosStream;
  temp:string[3];
  n:byte;
  b:byte;
  buffer:array[1..10] of byte;
  tb:byte;
  oki:boolean;
begin
  DetectArchive := 0;
  T.Init(afile,stOpenRead);
  if T.Status <> stOK then begin
    T.Done;
    exit;
  end;
  T.Read(buffer,SizeOf(buffer));
  if T.Status <> stOK then begin
    Debug('read error');
    T.Done;
    exit;
  end;
  T.Done;
  for b:=1 to maxArchivers do begin
    n := 1;
    oki := true;
    while n < length(Archivers[b].Sign) do begin
      temp := copy(Archivers[b].Sign,n,2);
      if temp[1] <> '?' then begin
        Insert('$',temp,1);
        tb := s2l(temp);
        if tb <> buffer[(n+1) div 2] then begin
          oki := false;
          break;
        end;
      end;
      inc(n,2);
    end;
    if oki then begin
      Debug(Archivers[b].Name+' detected');
      DetectArchive := b;
      exit;
    end;
  end;
end;

function UnpackArchive(archive,destdir:FnameStr):boolean;
var
  cmdline:FnameStr;
  b:byte;
  mode:word;
begin
  UnpackArchive := false;
  b := DetectArchive(archive);
  if b = 0 then exit;
  cmdline := Archivers[b].UnpackCmd;
  FastUpper(cmdline);
  Replace(cmdline,'@A',archive);
  Replace(cmdline,'@D',destdir);
  if Archivers[b].ShowOutput then mode := 0 else mode := xfHideOutput;
  UnpackArchive := XExec('',cmdline,mode);
end;

function PackArchive(archive,filespec:FnameStr):boolean;
var
  cmdline:FnameStr;
  mode:word;
  olddate:longint;
  b:byte;
begin
  packArchive := false;
  if XFileExists(archive) then b := DetectArchive(archive) else b:=1;
  if b = 0 then exit;
  olddate := XGetFileDate(archive);
  cmdline := Archivers[b].packCmd;
  FastUpper(cmdline);
  Replace(cmdline,'@A',archive);
  Replace(cmdline,'@F',fileSpec);
  if Archivers[b].ShowOutput then mode := 0 else mode := xfHideOutput;
  packArchive := XExec('',cmdline,mode);
  XSetFileDate(archive,olddate);
end;

procedure WriteArchivers;
var
  T:TDosStream;
begin
  T.Init(wkdir^+arcFile,stCreate);
  T.Write(Archivers,SizeOf(Archivers));
  T.Done;
end;

procedure InitArchivers;
var
  T:TDosStream;
begin
  if not XFileExists(wkdir^+arcFile) then Debug(arcFile+' not found - using defaults') else begin
    T.init(wkdir^+arcFile,stOpenRead);
    if T.GetSize = SizeOf(Archivers) then begin
      T.Read(Archivers,SizeOf(Archivers));
      if T.Status <> stOK then Debug(arcFile+' read error!');
    end else Debug(arcFile+' invalid!');
    T.Done;
  end;
end;

end.