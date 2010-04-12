{
Quick Text routines 2.01a
(c) 1995 SSG
}

{$R-,S-,O-,N-,E-}

unit QText;

interface

const

  Black        = 0;
  Blue         = 1;
  Green        = 2;
  Cyan         = 3;
  Red          = 4;
  Magenta      = 5;
  Brown        = 6;
  LightGray    = 7;

  DarkGray     =  8;
  LightBlue    =  9;
  LightGreen   = 10;
  LightCyan    = 11;
  LightRed     = 12;
  LightMagenta = 13;
  Yellow       = 14;
  White        = 15;

  qAttr  : byte = 7;

procedure qWrite(const s:string);
procedure qWriteln(s:string);
procedure qCenter(s:string);
procedure qLocate(x,y:byte);
procedure qSetColor(fc,bc:byte);
procedure qCls;
procedure qSetFC(fc:byte);
procedure qSetBC(bc:byte);
procedure qSetBlink(ablink:boolean);
procedure qNL;
procedure qScrollUp;
procedure qSetMode(amode:byte);
procedure qCursor(enable:boolean);
procedure qVGABlink(enable:boolean);
procedure qErase(x1,y1,x2,y2:byte);

function qWhere:word;
function qX:byte;
function qY:byte;
function qXSize:byte;
function qYSize:byte;

function qGetKey:word;
function qGetChar:char;
function qIsKey:boolean;

implementation

uses XBuf;

const

  wXSize = $4a;
  wYSize = $84;
  wMode  = $49;

  cursor : boolean = true;

procedure _setvideoseg;assembler;
asm
  push ax
  mov  ax,Seg0040
  mov  es,ax
  mov  ax,SegB800
  cmp  byte ptr es:wMode,7
  jne  @skip
  mov  ax,SegB000
@skip:
  mov  es,ax
  pop  ax
end;

procedure qErase;assembler;
asm
  call qXSize
  cld
  xor  dh,dh
  mov  dl,al
  xor  ah,ah
  mul  y1
  mov  bl,x1
  xor  bh,bh
  add  ax,bx
  shl  ax,1
  mov  di,ax
  xor  ah,ah
  mov  al,x2
  sub  ax,bx
  inc  ax
  mov  bx,ax {line length}
  sub  dx,ax {words to skip}
  shl  dx,1
  mov  ah,y1
  mov  al,y2
  sub  al,ah
  inc  al
  xor  ah,ah
  mov  cx,ax
  call _setvideoseg
  mov  ah,qAttr
  mov  al,32
@loop:
  push cx
  mov  cx,bx
  rep  stosw
  add  di,dx
  pop  cx
  loop @loop
end;

procedure qSetBlink;assembler;
asm
  mov al,qAttr
  and al,$7f
  mov ah,ablink
  shl ah,7
  or  al,ah
  mov qAttr,al
end;

procedure qVGABlink;assembler;
asm
  mov  dx,3dah
  in   al,dx {reset flip/flop of attribute controller}
  mov  dx,3c0h
  mov  al,10h
  out  dx,al
  mov  al,enable
  mov  ah,4
  or   al,al
  je   @skip
  or   ah,8
@skip:
  mov   al,ah
  out   dx,al
end;

procedure qCursor;assembler;
asm
  mov  ah,1
  mov  ch,14
  xor  cl,cl
  mov  cursor,false
  cmp  enable,false
  je   @skip
  mov  cl,15
  mov  cursor,true
@skip:
  int  10h
end;

function qXSize;assembler;
asm
  mov ax,seg0040
  mov es,ax
  mov al,byte ptr es:wXSize
end;

function qYSize;assembler;
asm
  mov ax,seg0040
  mov es,ax
  mov al,byte ptr es:wYSize
  inc al
end;

function qIsKey;assembler;
asm
  xor bh,bh
  mov ah,1
  int 16h
  je  @Fuck
  inc bh
@Fuck:
  mov al,bh
end;

procedure qSetMode;assembler;
asm
  xor ah,ah
  mov al,amode
  int 10h
end;

procedure qScrollUp;assembler;
asm
  cld
  push ds
  mov  ax,seg @Data
  mov  ds,ax
  mov  ah,qAttr
  push ax
  mov  ax,Seg0040
  mov  es,ax
  mov  dl,byte ptr es:wXSize
  mov  dh,byte ptr es:wYSize
  call _setvideoseg
  push es
  pop  ds
  xor  ah,ah
  mov  al,dl
  shl  ax,1
  mov  si,ax
  xor  di,di
  shr  ax,1
  mul  dh
  mov  cx,ax
  shr  cx,1
  db   66h
  rep  movsw

  mov  bx,seg @Data
  mov  ds,bx
  mov  di,ax
  shl  di,1
  pop  ax
  xor  al,al
  xor  ch,ch
  mov  cl,dl
  rep  stosw
  pop  ds
end;

procedure qWrite(const s:string);assembler;
asm
  jmp  @Init
@XSize:
  db   0
@IncY:
  cmp  bh,dh
  jb   @Incit
  push es
  pusha
  call qScrollUp
  popa
  pop  es
  push ax
  xor  ah,ah
  mov  al,bl
  shl  ax,1
  sub  di,ax
  pop  ax
  jmp  @XOk
@Incit:
  push dx
  xor  dh,dh
  shl  dx,1
  add  di,dx
  pop  dx
  inc  bh
  jmp  @XOk
@Init:
  cld
  call qWhere
  mov  bx,ax
  mov  ax,seg0040
  mov  es,ax
  mov  dh,byte ptr es:wYSize
  mov  dl,byte ptr es:wXSize
  mov  ah,qAttr

  call _setvideoseg
{  mov  al,byte ptr es:wMode
  mov  cx,SegB000
  cmp  al,7
  je   @B000
  mov  cx,SegB800
@B000:
  mov  es,cx}

  push ds
  lds  si,s
  push ax
  mov  cl,ds:[si]
  or   cl,cl
  jz   @Exit
  xor  ch,ch
  inc  si
  xor  di,di
  xor  ah,ah
  mov  al,bh
  mul  dl
  mov  di,ax
  xor  ah,ah
  mov  al,bl
  add  di,ax
  shl  di,1
  pop  ax
@Loop:
  lodsb
  cmp  al,32
  jae  @WriteIt
  cmp  al,8
  jne  @Other
  or   bl,bl
  jz   @Other
  dec  bl
  sub  di,2
  jmp  @XOk
@Other:
  cmp  al,13
  jne  @Another
  push ax
  mov  al,bl
  shl  al,1
  xor  ah,ah
  sub  di,ax
  xor  bl,bl
  pop  ax
  jmp  @Xok
@Another:
  cmp  al,10
  jne  @XOk
  jmp  @IncY
@WriteIt:
  stosw
  inc  bl
  cmp  bl,dl
  jbe  @XOk
  xor  bl,bl
  jmp  @IncY
@XOk:
  loop @Loop
  mov  ax,seg @data
  mov  ds,ax
  cmp  cursor,true
  jne  @exit
  mov  ah,2
  mov  dx,bx
  xor  bh,bh
  int  10h
@Exit:
  pop  ds
end;

procedure qNL;assembler;
asm
  call qWhere
  mov  bx,seg0040
  mov  es,bx
  mov  bl,byte ptr es:wYSize
  xor  al,al
  cmp  ah,bl
  jb   @Exit
  dec  ah
  push ax
  call qScrollUp
  pop  ax
@Exit:
  inc  ah
  mov  dx,ax
  xor  bh,bh
  mov  ah,2
  int  10h
end;

function qGetChar;assembler;
asm
  xor ah,ah
  int 16h
end;

function qGetKey;assembler;
asm
  xor ah,ah
  int 16h
end;

function qX;assembler;
asm
  call qWhere
end;

function qY;assembler;
asm
  call qWhere
  mov  al,ah
end;

procedure qSetBC;assembler;
asm
  and  qAttr,$0F
  mov  al,bc
  shl  al,4
  or   qAttr,al
end;

procedure qSetFC;assembler;
asm
  and  qAttr,$F0
  mov  al,fc
  or   qAttr,al
end;

function qWhere;assembler;
asm
  mov  ah,3
  xor  bh,bh
  int  10h
  mov  ax,dx
end;

procedure qWriteln(s:string);
begin
  qWrite(s+#13#10);
end;

procedure qLocate(x,y:byte);assembler;
asm
  mov  ah,2
  xor  bh,bh
  mov  dh,y
  mov  dl,x
  int  10h
end;

procedure qSetColor(fc,bc:byte);assembler;
asm
  mov  al,bc
  shl  al,4
  or   al,fc
  mov  qAttr,al
end;

procedure qCls;assembler;
asm
  cld
  mov  ax,seg0040
  mov  es,ax
  xor  ah,ah
  mov  al,byte ptr es:wXSize
  mov  bl,byte ptr es:wYSize
  inc  bl
  mul  bl
  mov  cx,ax
  xor  di,di

  call _setvideoseg
{  mov  al,byte ptr es:wMode
  mov  dx,SegB000
  cmp  al,7
  je   @B000
  mov  dx,SegB800
@B000:
  mov  es,dx}

  xor  al,al
  mov  ah,qAttr
  rep  stosw
  mov  ah,2
  xor  bh,bh
  xor  dx,dx
  int  10h
end;

procedure qCenter;
begin
  qLocate(((qXSize-length(s)) div 2)-1,qY);
  qWrite(s);
end;

end.