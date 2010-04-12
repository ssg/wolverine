{ pal2col -  1nd Jun 96 - 03:51 - SSG }

uses Dos,XIO,Objects;

type

  TScheme = record
    Name  : string[40];
    Pal   : array[1..768] of byte;
  end;

var
  dirinfo:SearchRec;
  T:TDosStream;
  hebe:TDosStream;
  rec:TScheme;
begin
  XAppInit('Wolverine Color Scheme Compiler','1.00a','SSG',1,'palfiles[.PAL]');
  FindFirst(XAddExt(paramStr(1),'.PAL'),archive+readonly,dirinfo);
  if DosError <> 0 then XAbort('no file(s) found');
  T.init('W.COL',stOpen);
  if T.Status <> stOK then begin
    T.Done;
    T.Init('W.COL',stCreate);
  end;
  T.Seek(T.GetSize);
  while DosError = 0 do begin
    write(dirinfo.name+': ');
    readln(rec.name);
    if rec.name = '' then XAbort('user break');
    hebe.Init(dirinfo.Name,stOpenRead);
    hebe.Read(rec.Pal,SizeOf(rec.Pal));
    hebe.Done;
    T.Write(rec,SizeOf(rec));
    FindNext(dirinfo);
  end;
  T.Done;
end.