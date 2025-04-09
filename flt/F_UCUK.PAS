{
F_UCUK
Wolverine filter example
Author: SSG
MaKeS MeSSaGeS LooK LiKe THiS
}

function locase(c:char):char;
begin
  if c in ['A'..'Z'] then inc(byte(c),byte('a')-byte('A'));
  locase := c;
end;

var
  s:string;
  f:text;
  out:text;
  b:byte;
begin
  if ParamCount <> 2 then begin
    writeln('This filter requires Wolverine');
    halt;
  end;
  Assign(F,ParamStr(1));
  Reset(F);
  Assign(Out,ParamStr(2));
  ReWrite(Out);
  while not Eof(F) do begin
    readln(f,s);
    for b:=1 to length(s) do
      if upcase(s[b]) in ['A','E','I','O','U'] then s[b] := locase(s[b])
                                               else s[b] := upcase(s[b]);
    writeln(out,s);
  end;
  Close(F);
  Close(Out);
end.