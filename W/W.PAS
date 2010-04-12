{- WOLVERINE -}

{
neler yapilcak bak:

- UNLIMITED message count... find a way to overcome that collection
  problem...
- adam gibi bi setup?
- help update edilcek...
- bi tane hot-area list... sIk sIk okunan veya mesaj yazilan alanlarin ayrica
  oldugu bi liste...
- internal editor...
- quote ba$inda bo$luk birakma olayi optional olsun??
- nibbles?
- install... (new version upgrade'ler icin $art)
- pakedi kaparken hatirlatirken kaci pers soylesin...
- generic ar$ivle olayi... WHAM olayi henuz tam acikliga kavu$mu$ degil...
  beta bi destek konabilir...
- bakiciya fileopendialog yapilcak...
- message base reader?

pek onemli olmayan veya olmasi zor secenekler:

- wrap'ler perfect'lenecek...
- squish support... (virtual collection veya unlimited collection olayina
  girmem lazim)... ayrica lanet olasi squish bir anda bir alan uzerinde
  i$lem yapmaya izin vermiyo mu deli edecek beni... ooof of... bi de squish.cfg
  diye bi$eyi okutmak gerekiyo gerekli bilgileri almak icin... lanet olsun...
- listview'larda keyboard'la search...
- msglistwindow'larda "message Write" button'u...
- filter messaging api... dosya tabanli filtre ile ileti$im kontrolu... bir
  api olayi... hmm...
- kullandigi editoru de otomatik bulsun?
- mukemmel inputline'lar...
- hard diski ararken tum down ve up'lari bulmasi...
- XMF... (font ve v2v rutinleri gerek)
- sound effects...
- kaydederken ansi'ler strip edilcek...
- background'da fallenangel!!! (veya fallenangel yerine hiddenangel?)
- info read ederken bazi button'lar disabled olcak...
- address book'ta optional sort... most recent used'a gore sort??

-fast olunca:

 þ arka plan background'u yuklenmiyo
 þ shutdown effect yok
 þ window zooming'ler yok..

2.30
~~~~
! ansi file uudecode crash fix...
! unregged fake KEY trick fix...
! sola yana$ik quote olayi duzeltildi...
! quote'larda date ve time'in mesaj yerine bugunu gosterme hatasi
  giderildi...
! crosspost cali$ir hale getirildi...
! QWK'lardaki smart rename hatasi giderildi...
! QWK'lardaki unread hatasi giderildi...
! gui restructured... bugfixes... daha az ve daha oz kod...
! pentagon paint hatasi fixed...
! echotag mismatch fix...
! language inconsistencies fix...
! perfected CD player...
! gui restructured...
+ replywindow artikin son yazilan mesaja seek ediyor otomatikman...
+ enhanced search capabilities!
+ added activity indicator while loading packets..
+ ba$langictaki directory arama optional oldu...
+ messagebox'lar iyile$tirildi...
+ Viva la Sewage feature added!
+ better help...
+ added hand-scrolling...
+ pakedi kaparken yapilan hatirlatma optional oldu... (unreadreminder)
+ TH? ve FR?'ler icin smartrename eklendi...
+ -NOCD parameter...
+ -CD:xx parameter...
+ -NEWCFG parameter...
+ -NOCYCLE parameter...

possible bugs:

- bi kac msglistwindow acikken birbirlerinin OwnerNext ve OwnerPrev komutlarini
  payla$iyorlar... hata... hala var mi bilmiyom ama pek de onemli degil...
  salla...

- bw.lookolc'da hata var?

- 16,380 mesaj limitini a$mak gerek...
}

program Wolverine;

{$IFNDEF DPMI}
go hell!
{$ENDIF}

uses

  {wolverine units}
  WLan, WArch, WTypes, WProcs, WHelp, WObjects,

  {bp units}
  Drivers, Dos, Strings, Graph, Objects,

  {fatalvision units}
  AXEServ, XStr, GView, XGfx, XTypes, XBuf, XStream, XLan, XDev, XPal, XHelp,
  XIO, XGH, XPrn, XDebug, XCD, XCDPlay, XDiag, XUnix, Debris, XScroll, XErr,
  XColl, Tools, XSys, XDPMI;

type

  TReader = object(TSystem)
    HL    : PHelpWindow;
    Tick  : longint;
    constructor Init;
    destructor  Done;virtual;
    procedure   HandleEvent(var Event:TEvent);virtual;
    procedure   PrimaryHandle(var Event:TEvent);virtual;
    procedure   Backprocess;virtual;
    procedure   SetSysPalette;virtual;
    procedure   ShutdownEffect;virtual;
    procedure   FallenAngel;
    procedure   ScreenSaver;
{    procedure   InitPointingDevice;virtual;}
  end;

  PTaglineManagerDialog = ^TTaglineManagerDialog;
  TTaglineManagerDialog = object(TDialog)
    Lister : PStringViewer;
    constructor Init;
    procedure HandleEvent(var Event:TEvent);virtual;
  end;

  PSetupDialog = ^TSetupDialog;
  TSetupDialog = object(TDialog)
    constructor Init;
    procedure HandleEvent(var Event:TEvent);virtual;
    procedure SetData(var rec);virtual;
    procedure GetData(var rec);virtual;
  end;

  PBBSInfoLister = ^TBBSInfoLister;
  TBBSInfoLister = object(TListViewer)
    function GetText(item:longint):string;virtual;
    procedure itemDoubleClicked(item:longint);virtual;
  end;

  PPacketLister = ^TPacketLister;
  TPacketLister = object(TFormattedLister)
    function GetText(item:longint):string;virtual;
    procedure itemDoubleClicked(item:longint);virtual;
  end;

  PPacketOpenDialog = ^TPacketOpenDialog;
  TPacketOpenDialog = object(TDialog)
    Lister : PPacketLister;
    BBSLister : PBBSInfoLister;
    constructor Init;
    procedure HandleEvent(var Event:TEvent);virtual;
    procedure UpdatePacketList;
  end;

  PPassDialog = ^TPassDialog;
  TPassDialog = object(TDialog)
    constructor Init(ahdr:FnameStr);
    function    Valid(acmd:word):boolean;virtual;
  end;

  PMenuWindow = ^TMenuWindow;
  TMenuWindow = object(TWindow)
    constructor Init;
    procedure   HandleEvent(var Event:TEvent);virtual;
  end;

  PTextFileViewWindow = ^TTextFileViewWindow;
  TTextFileViewWindow = object(TFlexDialog)
    Viewer      : PTextViewer;
    constructor Init(afname:FnameStr);
    procedure   HandleEvent(var Event:TEvent);virtual;
  end;

  PColorCategoryLister = ^TColorCategoryLister;
  TColorCategoryLister = object(TStringViewer)
    procedure ItemFocused(item:longint);virtual;
  end;

  PPaletteLister = ^TPaletteLister;
  TPaletteLister = object(TStringViewer)
    procedure ItemFocused(item:longint);virtual;
  end;

  PFontLister = ^TFontLister;
  TFontLister = object(TStringViewer)
    procedure ItemFocused(item:longint);virtual;
  end;

  PColorConfigDialog = ^TColorConfigDialog;
  TColorConfigDialog = object(TDialog)
    SampleText     : PTextViewer;
    ColorSelector  : PColorSelector;
    PaletteLister  : PPaletteLister;
    CategoryLister : PColorCategoryLister;
    fontLister     : PFontLister;
    constructor Init(ahdr:FnameStr);
    procedure   HandleEvent(var Event:TEvent);virtual;
  end;

  PArchiverSetupDialog = ^TArchiverSetupDialog;
  TArchiverSetupDialog = object(TDialog)
    constructor Init(ahdr:FnameStr);
  end;

const

  openDialog : PPacketOpenDialog = NIL;

procedure WPal;external;
{$L WPAL}

procedure SetWPalette;
begin
  XGfx.SetPalette(PRGBPalette(@WPal)^);
end;

procedure SetScheme(name:FnameStr);
var
  T:TDosStream;
  s:string;
  pal:TRGBpalette;
  n:integer;
begin
  T.Init(wkdir^+colFile,stOpenRead);
  if T.Status = stOK then while T.GetPos < T.GetSize do begin
    IniReadln(T,s);
    if s[1] = '[' then begin
      System.Delete(s,1,1);
      dec(byte(s[0]));
      if s = name then begin
        for n:=0 to 7 do begin
          IniReadln(T,s);
          pal[n].R := s2l(GetParse(s,',',1));
          pal[n].G := s2l(GetParse(s,',',2));
          pal[n].B := s2l(GetParse(s,',',3));
        end;
        for n:=8 to 15 do begin
          IniReadln(T,s);
          pal[n+64].R := s2l(GetParse(s,',',1));
          pal[n+64].G := s2l(GetParse(s,',',2));
          pal[n+64].B := s2l(GetParse(s,',',3));
        end;
        XGfx.SetPalette(pal);
        T.Done;
        exit;
      end;
    end;
  end;
  T.Done;
  SetWPalette;
end;

procedure InitPacketWindows;
var
  Pa:PAreaListWindow;
  Pr:PReplyListWindow;
begin
  if not packetOpen then Abort('InitPacketWindows','Internal shit');
  New(Pa,Init(user.BBS+' ('+user.Name+')'));
  New(Pr,Init);
  GSystem^.Insert(Pr);
  GSystem^.Insert(Pa);
end;

{rexx}

{- TFontLister -}
procedure TFontLister.ItemFocused;
var
  s:string;
  id:word;
begin
  s := PString(ItemList^.At(FocusedItem))^;
  Debug('setting font '+s);
  id := GetRscId(rtFont,s);
  if id <> $ffff then begin
    WProcs.SetFont(id);
    Gsystem^.Paint;
  end;
end;

{- TArchiverSetupDialog -}
constructor TArchiverSetupDialog.Init;
var
  R:TRect;
  b:byte;
  nexty:integer;
  procedure putlabel(msg:string; addsize:word);
  begin
    Insert(New(PLabel,FullInit(r.a.x,r.a.y,msg,cRed,Col_Back,fntPropSmall)));
    inc(r.a.x,addsize+10);
  end;

  procedure putinput(maxlen:byte; xsize:word; disabled:boolean);
  begin
    Insert(New(PInputLine,Init(r.a.x,r.a.y,xsize,fntProp,maxlen)));
    Top^.SetState(Scf_Disabled,disabled);
    inc(r.a.x,xsize+10);
    nexty := r.a.y+Top^.Size.Y+5;
  end;

  procedure putcb;
  begin
    insert(new(PSingleCheckBox,Init(r.a.x,r.a.y,fntPropSmall,'')));
  end;

  procedure putarchiver(var rec:TArchiver);
  begin
    putinput(8,50,true);
    putinput(20,50,true);
    putinput(79,100,false);
    putinput(79,100,false);
    putcb;
    r.a.y := nexty;
    r.a.x := 5;
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,ahdr);
  Options := Options or Ocf_Centered;
  r.a.x := 5;
  r.a.y := 5;
  putlabel(gtid(msArchiverName),50);
  putlabel(gtid(msArchiverSign),50);
  putlabel(gtid(msArchiverCompressCmd),100);
  putlabel(gtid(msArchiverExtractCmd),100);
  putlabel(gtid(msArchiverScreenOutput),50);
  inc(r.a.y,Top^.Size.Y+5);
  r.a.x := 5;
  for b:=1 to maxArchivers do putarchiver(Archivers[b]);
  InsertBlock(GetBlock(5,r.a.y,mnfHorizontal,
    NewButton(gtid(msGenericSaveButton),cmOK,
    NewButton(gtid(msGenericCancelButton),cmCancel,
    NewButton(gtid(msGenericHelpButton),cmHelp,
    NIL)))));
  SelectNext(true);
  HelpContext := hcArchiverSetupDialog;
  FitBounds;
end;

{- TPaletteLister -}
procedure TPaletteLister.ItemFocused;
var
  P:PString;
begin
  P := ItemList^.At(item);
  SmoothPalSet := true;
  SetScheme(P^);
  SmoothPalSet := false;
end;

{- TColorCategoryLister -}
procedure TColorCategoryLister.ItemFocused;
begin
  Message(Owner,evBroadcast,cmColorCategoryChanged,@Self);
end;

{- TColorConfigDialog -}
constructor TColorConfigDialog.Init;
var
  R:TRect;
  SR:TRect;
  Pb:PScrollBar;
  Ps:PStringCollection;
  n:integer;
  destscheme:string;
const
  defxsize = 100;
  defysize = 200;
  procedure putlabel(msg:string);
  begin
    with r.a do Insert(New(PLabel,FullInit(x,y,msg,cRed,Col_Back,fntPropSmall)));
    inc(r.a.y,Top^.Size.Y+5);
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,ahdr);
  Options := Options or Ocf_Centered;
  r.a.x := 5;
  r.a.y := 5;
  putlabel(gtid(msColorConfigPalette));
  r.b.x := r.a.x + 85;
  r.b.y := r.a.y + (defysize div 2);
  New(PaletteLister,Init(R,fntPropSmall));
  PaletteLister^.NewList(GetColorList);
  destscheme := setup.colorscheme;
  if destscheme = '' then destscheme := defaultSchemeName;
  for n:=0 to PaletteLister^.ItemList^.Count-1 do if PString(PaletteLister^.itemList^.At(n))^ = destScheme then begin
    PaletteLister^.FocusItem(n);
    break;
  end;
  PaletteLister^.GetBounds(R);
  Insert(PaletteLister);
  r.a.y := r.b.y + 5;
  putlabel(gtid(msColorConfigFont));
  r.b.y := r.a.y + ((defysize div 2)-(10+Top^.Size.Y));
  New(FontLister,Init(R,fntPropSmall));
  FontLister^.NewList(getFontList);
  destscheme := GetRscName(rtFont,setup.readerFont);
  Strip(destscheme);
  for n:=0 to fontLister^.ItemList^.Count-1 do if PString(fontLister^.itemList^.At(n))^ = destscheme then begin
    fontLister^.FocusItem(n);
    break;
  end;
  FontLister^.GetBounds(R);
  Insert(fontLister);
  r.a.x := r.b.x+5;
  r.a.y := 5;
  putlabel(gtid(msCOlorCOnfigCategory));
  r.b.x := r.a.x + 60;
  r.b.y := r.a.y + defysize;
  New(CategoryLister,Init(R,fntPropSmall));
  New(Ps,Init(maxreadercolors,maxreadercolors));
  for n:=1 to maxreadercolors do Ps^.Insert(NewStr(readerColors[n].Name));
  CategoryLister^.NewList(Ps);
  CategoryLister^.GetBounds(R);
  Insert(CategoryLister);
  r.a.x := r.b.x + 5;
  r.a.y := 5;
  putlabel(gtid(msColorConfigColor));
  r.b.x := r.a.x + 25;
  r.b.y := r.a.y + defysize;
  ColorSelector := New(PColorSelector,Init(R));
  ColorSelector^.GetBounds(R);
  Insert(ColorSelector);
  r.a.y := 5;
  r.a.x := r.b.x + 5;
  PutLabel(gtid(msColorConfigSampleMsg));
  r.b.x := r.a.x + 300;
  r.b.y := r.a.y + defysize;
  sr := r;
  sr.a.x := r.b.x+1;
  sr.b.x := sr.a.x + scrollerXSize;
  New(Pb,Init(sr));
  New(SampleText,Init(R,GetSampleText,Pb));
  SampleText^.GrowMode := 0;
  SampleText^.EventMask := evCommand or evBroadcast;
  Insert(Pb);
  Insert(SampleText);
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msGenericSaveButton),cmOK,
    newButton(gtid(msGenericCancelButton),cmCancel,
    NewButton(gtid(msGenericHelpButton),cmHelp,NIL)))));
  HelpContext := hcColorConfigDialog;
  FitBounds;
end;

procedure TColorConfigDialog.HandleEvent(var Event:TEvent);
  function getactivecategory:byte;
  var
    s:string;
    b:byte;
  begin
    s := PString(CategoryLister^.ItemList^.At(CategoryLister^.FocusedItem))^;
    for b:=1 to maxreadercolors do if readerColors[b].Name = s then begin
      getactivecategory := b;
      exit;
    end;
    Abort('GetActiveCategory','Inconsistency failure');
  end;
begin
  inherited HandleEvent(Event);
  if Event.What = evBroadcast then case Event.Command of
    Brc_ColorSelected : begin
      EventWait;
      readerColors[getactivecategory].Color := ColorSelector^.Focused;
      GSystem^.Paint;
      ClearEvent(Event);
    end;
    cmColorCategoryChanged : begin
      ColorSelector^.Focused := readerColors[getactivecategory].Color;
      ColorSelector^.Paint;
      ClearEvent(Event);
    end;
  end; {case}
end;

{- TTextFileViewWindow -}
constructor TTextFileViewWindow.Init;
var
  R:TRect;
  Text:PMsgText;
  Ps:PScrollBar;
  T:TDosStream;
  Pb:PButton;
begin
  R.Assign(0,0,400,200);
  afname := FExpand(afname);
  inherited Init(R,Lower(afname));
  GetVisibleBounds(R);
  R.Move(-r.a.x,-r.a.y);
  R.Grow(-5,-5);
  r.a.x := r.b.x-scrollerXSize;
  New(Ps,Init(R));
  Ps^.GrowMode := gmFixedAll and not gmFixedLoX;
  Ps^.GetBounds(R);
  Insert(Ps);
  r.b.x := r.a.x-5;
  r.a.x := 5;
  EventWait;
  T.Init(afname,stOpenRead);
  Text := stream2Text(T,T.GetSize);
  T.Done;
  New(Viewer,Init(R,Text,Ps));
  Viewer^.Options := Viewer^.Options and not Ocf_PaintFast;
  Viewer^.GetBounds(R);
  Insert(Viewer);
  r.a.x := r.b.x-70;
  r.b.y := r.a.y+21;
  New(Pb,SizedInit(R,gtid(msViewerEditButtonCaption),cmEditFile));
  Pb^.GrowMode := gmFixedHiX;
  Insert(Pb);
  FitBounds;
end;

procedure TTextFileViewWindow.HandleEvent;
var
  T:TDosStream;
begin
  inherited HandleEvent(Event);
  if Event.What = evCommand then if Event.Command = cmEditFile then begin
    ClearEvent(Event);
    if EditFile(Header^) then begin
      EventWait;
      T.Init(Header^,stOpenRead);
      Viewer^.NewText(stream2Text(T,T.GetSize));
      T.Done;
    end;
  end;
end;

{- TTaglineManagerDialog -}
constructor TTaglineManagerDialog.Init;
var
  R:TRect;
  Ps:PScrollBar;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(mstaglineManagerHdr));
  Options := Options or Ocf_Centered;
  R.Assign(0,0,400,150);
  R.Move(5,5);
  New(Lister,Init(R,fntProp));
  Lister^.NewList(GetTaglineList);
  Lister^.GetBounds(R);
  r.a.x := r.b.x+1;
  r.b.x := r.a.x+scrollerXSize;
  New(Ps,Init(R));
  Insert(Lister);
  Insert(Ps);
  Lister^.AssignScroller(Ps);
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(mstaglineManagerAdd),cmAdd,
    newButton(gtid(mstaglineManagerDelete),cmDelete,
    NewButton(gtid(mstaglineManagerEdit),cmEdit,
    NewButton(gtid(mstaglineManagerSave),cmSave,
    NewButton(gtid(mstaglineManagerCancel),cmClose,
    NewButton(gtid(mstaglineManagerHelp),cmHelp,
    NIL))))))));
  Insert(New(PAccelerator,Init(
    NewAcc(kbDel,cmDelete,
    NewAcc(kbIns,cmAdd,
    NewAcc(kbEnter,cmEdit,
    NIL))))));
  HelpContext := hcTaglineManagerDialog;
  FitBounds;
end;

procedure TTaglineManagerDialog.HandleEvent;
type

  ttagstr = string[76];

  function TagPrompt(var s:ttagstr):boolean;
  var
    P:PTaglineInputDialog;
    code:word;
  begin
    New(P,init(false));
    P^.SetData(s);
    code := GSystem^.ExecView(P);
    TagPrompt := code = cmOK;
    P^.GetData(s);
    Dispose(P,Done);
  end;

  procedure addList(s:ttagstr);
  begin
    Lister^.ItemList^.Insert(NewStr(s));
    Lister^.PaintView;
  end;

  procedure AddTag;
  var
    s:ttagstr;
  begin
    s := '';
    if TagPrompt(s) then addList(s);
  end;

  procedure EditTag;
  var
    s:ttagstr;
  begin
    s := PString(Lister^.ItemList^.At(Lister^.FocusedItem))^;
    if not TagPrompt(s) then exit;
    Lister^.DeleteItem(Lister^.FocusedItem);
    addList(s);
  end;

  procedure SaveTags;
  var
    T:TDosStream;
    n:integer;
  begin
    EventWait;
    T.Init(wkdir^+tagFile,stCreate);
    SWriteln(T,gtid(msTaglineFileHeader)+#13#10);
    for n:=0 to Lister^.ItemList^.Count-1 do SWriteln(T,PString(Lister^.ItemList^.At(n))^);
    T.Done;
    Message(@Self,evCommand,cmClose,NIL);
  end;

begin
  inherited HandleEvent(Event);
  if Event.What = evCommand then begin
    case Event.Command of
      cmAdd : AddTag;
      cmDelete : if Lister^.ItemList^.Count > 0 then Lister^.DeleteItem(Lister^.FocusedItem);
      cmEdit : if Lister^.ItemList^.Count > 0 then EditTag;
      cmSave : begin
        SaveTags;
        exit;
      end;
      else exit;
    end; {case}
    ClearEvent(Event);
  end;
end;

function TBBSInfoLister.GetText;
var
  P:PBBSInfo;
begin
  P := ItemList^.At(item);
  GetText := P^.BBSName;
end;

procedure TBBSInfoLister.itemDoubleClicked;
begin
  Message(Owner,evCommand,cmOpen,@Self);
end;

constructor TMenuWindow.Init;
var
  R:TRect;
{  pv:PView;}
  T:TDosStream;
  s:String;
  sub:string;
  count:word;
  b:byte;
  procedure putit(atitle:fnamestr; avif,acmd:word);
  var
    pb:PVIFButton;
  begin
    New(pb,Init(r.a.x,r.a.y,avif,acmd));
    Insert(pb);
    Insert(New(PLabel,Init(pb^.Size.X+8,r.a.y+5,atitle,fntProp)));
    R.Move(0,pb^.Size.Y+5);
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(msMenuWindowHeader));
  Options := (Options and not (Ocf_ReSize+Ocf_Close)) or Ocf_PostProcess;
  HelpContext := hcMenuWindow;
  r.a.x := 3;
  r.a.y := 3;
  r.b.y := r.a.y+GetFontHeight(ButtonFont)+10;
  T.Init(wkdir^+menuFile,stOpenRead);
  count := cmMenuBase;
  if T.Status = stOK then while T.GetPos < T.GetSize do begin
    SReadLn(T,s);
    Strip(s);
    if (s <> '') and (s[1] <> ';') then if Regged then begin
      b := pos(',',s);
      if b > 0 then begin
        if menuList = NIL then New(menuList,Init(10,10));
        sub := copy(s,1,b-1);
        Strip(sub);
        if sub = '' then sub := '???';
        putit(l2s(menuList^.Count+1)+' '+sub,idW,count);
        sub := copy(s,b+1,255);
        Strip(sub);
        if sub = '' then sub := ' ';
        menuList^.Insert(NewStr(sub));
        inc(count);
        if menuList^.Count = 9 then break;
      end;
    end else BulletinBoard := gtid(msGlobalUnregMsg);
  end;
  T.Done;
  putit(gtid(msMenuWindowViewer),idMagni,cmMagni);
  putit(gtid(msMenuWindowAddrBook),idAddr,cmAddressBook);
  putit(gtid(msMenuWindowTaglines),idTagline,cmTaglines);
  putit(gtid(msMenuWindowSetup),idSetup,cmSetup);
  putit(gtid(msMenuWindowDosPrompt),idPrompt,cmDosShell);
  putit(gtid(msMenuWindowQuit),idExit,cmQuit);
  FitBounds;
end;

procedure TMenuWindow.HandleEvent;

  procedure Launch(index:integer);
  var
    cmdline:string;
    flags:word;
  const
    params:FnameStr='';
    function GetPacketName:string;
    var
      s:string;
      P:PPacket;
    begin
      GetPacketName := '';
      with openDialog^.Lister^ do begin
        if ItemList^.Count = 0 then exit;
        P := ItemList^.At(FocusedItem);
        if IsThisPacketOpen(P) then Message(Owner,evBroadcast,cmCloseActivePacket,NIL);
        s := P^.Name;
      end;
      GetPacketName := s;
    end;
  begin
    if menuList = NIL then exit;
    if index > menuList^.Count-1 then exit;
    ClearEvent(Event);
    cmdline := Upper(PString(menuList^.At(index))^);
    Replace(cmdline,'%P',GetPacketName);
    if pos('%?',cmdline) > 0 then begin
      if not InputBox(cmdline,gtid(msCmdParamsAskDialogPrompt),hcParamInputDialog,fntProp,params,79) then exit;
      Replace(cmdline,'%?',params);
    end;
    if pos('%!',cmdline) > 0 then begin
      flags := xfPause;
      Replace(cmdline,'%!','');
    end else flags := 0;
    XExec('',cmdline,flags);
  end;

var
  c:char;
begin
  inherited HandleEvent(Event);
  if Event.What = evKeyDown then begin
    c := GetAltChar(Event.KeyCode);
    if c in ['1'..'9'] then Launch(byte(c)-49);
  end else if Event.What = evCommand then if Event.Command >= cmMenuBase then
    if Event.InfoPtr = @Self then Launch(Event.Command-cmMenuBase);
end;

constructor TPassDialog.Init;
var
  R:TRect;
  Pi:PInputLine;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,ahdr);
  Options := Options or Ocf_Centered;
  Insert(New(PLabel,Init(5,5,gtid(msSecurityPrompt),fntPropSmall)));
  Pi := New(PInputLine,Init(60,5,90,fntProp,15));
  Pi^.SetConfig(ilPassword+ilUpper,True);
  Pi^.ValidChars := ['0'..'9','A'..'Z'];
  Pi^.GetBounds(R);
  Insert(Pi);
  Insert(New(PAccelerator,Init(NewAcc(kbEnter,cmOK,NIL))));
  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msSecurityOK),cmOK,
    NewButton(gtid(msSecurityCancel),cmCancel,
    NIL))));
  FitBounds;
end;

function TPassDialog.Valid;
var
  s:string[15];
begin
  Valid := true;
  if acmd = cmOK then begin
    GetData(s);
    if s <> setup.Password then begin
      MessageBox(gtid(msSecurityDeny),0,mfInfo);
      Valid := false;
    end;
  end;
end;

function TPacketLister.GetText;
var
  s:fnameStr;
  P:PPacket;
  function ls(l:longint):string;
  begin
    if l = 0 then ls := '' else ls := l2s(l);
  end;
begin
  P := Itemlist^.at(item);
  Str(p^.size,s);
  COnvertNumToBusiness(s);
  s := p^.Name+'|'+s+'|'+Date2Str(longrec(p^.time).hi,false)+'|'+
       Time2Str(longrec(p^.time).lo);
  if p^.Total > 0 then s := s + '|'+ls(p^.Total)+'|'+ls(p^.unread)+'|'+
                               ls(p^.willread)+'|'+ls(p^.willreply)+'|'+
                               ls(p^.personal);
  GetText := s;
end;

procedure TPacketLister.itemDoubleClicked;
begin
  Message(Owner,evCommand,cmOpen,@Self);
end;

procedure TPacketOpenDialog.UpdatePacketList;
var
  T:TBufStream;
  dirinfo:SearchRec;
  hi:TWHAMInfo;
  h:TInfHeader;
  Pc:PPacketColl;
  Pb:PBBSInfoColl;
  Pp:PPacket;
  ext:string[3];
  foc:integer;
  b:byte;
  procedure putit(bbs:FnameStr);
  var
    Pbbs:PBBSInfo;
  begin
    New(Pbbs);
    Pbbs^.BBSName := bbs;
    Pbbs^.InfName := dirinfo.name;
    Pb^.Insert(Pbbs);
  end;
begin
  EventWait;
  foc := Lister^.FocusedItem;
  new(Pc,Init(10,10));
  FindFirst(setup.DLDir+'*.*',archive+readonly,dirinfo);
  while DosError = 0 do begin
    b := pos('.',dirinfo.name);
    if b > 0 then if (dirinfo.name[length(dirinfo.name)] in ['0'..'9']) or
                     (copy(dirinfo.name,b+1,3)='QWK') then begin
      New(pp);
      with dirinfo do begin
        ClearBuf(pp^,SizeOf(TPacket));
        PP^.Name := Lower(name);
        PP^.Size := size;
        PP^.Time := time;
        GetExtraPacketInfo(PP^);
      end;
      Pc^.Insert(Pp);
    end;
    FindNext(dirinfo);
  end;
  Lister^.NewList(pc);
  Lister^.FocusItem(foc);
  Lister^.PaintView;

  New(Pb,Init(10,10));
  foc := BBSLister^.FocusedItem;
  FindFirst(infDir^+'*.*',Archive+ReadOnly,dirinfo);
  while DosError = 0 do begin
    b := pos('.',dirinfo.name);
    if b > 0 then begin
      ext := copy(dirinfo.name,b+1,3);
      if ext = 'INF' then begin
        T.Init(infDir^+dirinfo.name,stOpenRead,8192);
        if T.Status = stOK then begin
          T.Read(h,SizeOf(h));
          if T.Status = stOK then putit(StrPas(@h.SystemName));
        end;
        T.Done;
      end else if ext = 'WI' then begin
        T.Init(infDIr^+dirinfo.name,stOpenRead,8192);
        if T.status = stOK then begin
          T.Read(hi,SizeOf(hi));
          if T.Status = stOK then putit(hi.System);
        end;
        T.Done;
      end;
    end;
    FindNext(dirinfo);
  end;
  BBSLister^.NewList(Pb);
  BBSlister^.FocusItem(foc);
  BBSLister^.PaintView;
end;

constructor TPacketOpenDialog.Init;
const
  labelColor = cRed;
var
  R:TRect;
  pl:PLabel;
  procedure GiveScroller(alister:PListViewer);
  var
    Ps:PScrollBar;
  begin
    alister^.GetBounds(R);
    r.a.x := r.b.x+2;
    r.b.x := r.a.x+scrollerXSize;
    New(Ps,Init(R));
    Ps^.GetBounds(R);
    Insert(Ps);
    alister^.AssignScroller(Ps);
    Insert(alister);
  end;
begin
  R.Assign(0,0,0,0);
  inherited Init(R,gtid(msPacketListWindowHdr));
  OPtions := (Options or (Ocf_Centered)) and not Ocf_Close;
  New(Pl,FullInit(5,5,gtid(msPacketListHdr),labelColor,Col_Back,fntPropSmall));
  pl^.GetBounds(R);
  Insert(pl);
  New(Lister,Init(5,r.b.y+5,fntPropSmall,10,
    NewColumn(gtid(msPLWPacket),72,0,
    NewColumn(gtid(msPLWSize),56,cofRJust,
    NewColumn(gtid(msPLWDate),43,0,
    NewColumn(gtid(msPLWTime),30,0,
    NewColumn(gtid(msPLWTotal),24,cofRJust,
    NewColumn(gtid(msPLWUnread),24,cofRJust,
    NewColumn(gtid(msPLWWillread),24,cofRJust,
    NewColumn(gtid(msPLWWillReply),24,cofRJust,
    NewColumn(gtid(msPLWPers),24,cofRJust,
    NIL)))))))))));
  Lister^.GetBounds(R);
  New(pl,FullInit(5,r.b.y+5,gtid(msBBSListHdr),labelColor,Col_Back,fntPropSmall));
  pl^.GetBounds(R);
  Insert(pl);
  r.a.y := r.b.y+5;
  r.b.y := r.a.y+40;
  r.b.x := r.a.x+Lister^.Size.X;
  New(BBSLister,Init(R,fntPropSmall));
  UpdatePacketList;

  GiveScroller(Lister);
  GiveScroller(BBSLister);

  SelectNext(True);

  InsertBlock(GetBlock(5,r.b.y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msPODOpen),cmOpen,
    NewButton(gtid(msPODDelete),cmDel,
    NewButton(gtid(msPODMMA),cmSearch,
    NewButton(gtid(msPODRefresh),cmRefresh,
    NewButton(gtid(msPODHelp),cmHelp,
    NIL)))))));
  HelpContext := hcPacketOpenDialog;
  FitBounds;
end;

procedure TPacketOpenDialog.HandleEvent;

  function OpenExistingPacket(replies:boolean):boolean;
  var
    P:PPacket;
  begin
    OpenExistingPacket := false;
    if Lister^.ItemList^.Count = 0 then exit;
    P := Lister^.ItemList^.At(Lister^.FocusedItem);
    OpenExistingPacket := OpenPacket(setup.DLDir+P^.Name,stdpakdir^,stdrepdir^,replies,false);
  end;

  function OpenFakePacket:boolean;
  var
    P:PBBSInfo;
  begin
    OpenFakePacket := false;
    if BBSLister^.ItemList^.Count = 0 then exit;
    P := BBSLister^.ItemList^.At(BBSLister^.FocusedItem);
    OpenFakePacket := OpenPacket(P^.InfName,stdpakdir^,stdrepdir^,true,true);
  end;

  procedure DeletePacket;
  var
    victim:string[12];
    hebe:FnameStr;
  begin
    if Lister^.itemList^.Count = 0 then exit;
    victim := PPacket(Lister^.ItemList^.At(Lister^.FocusedItem))^.Name;
    if isThisPacketOpen(PPacket(Lister^.ItemList^.At(Lister^.FocusedItem))) then begin
      ExecBox(gtid(msDeleteOpenpacketTxt),gtid(msDeleteOpenPacketHdr),0,
              GetBlock(0,0,mnfHorizontal,NewButton(gtid(msDeleteOpenPacketBtn),cmOK,NIL)));
      exit;
    end;
    hebe := gtid(msDeletePacketPromptTxt);
    Replace(hebe,'%P',victim);
    if ExecBox(hebe,gtid(msDeletePacketPromptHdr),0,
      GetBlock(0,0,mnfHorizontal,
        NewButton(gtid(msDeletePacketPromptYes),cmYes,
        NewButton(gtid(msDeletePacketPromptNo),cmClose,
        NIL)))) = cmYes then begin
      EventWait;
      XDeleteAnyway(setup.DLDir+victim);
      Lister^.DeleteItem(Lister^.FocusedItem);
    end;
  end;

  procedure CachePacket;
  begin
    if Packet^.Config and mpcCanCache > 0 then begin
      Packet^.CacheTo(infDir^);
      Debug('packet cached');
    end else Debug('packet doesn''t support caching');
    UpdatePacketList;
  end;

  procedure Amaan;
  begin
    if not packetOpen then begin
      if Lister^.GetState(Scf_Focused) then begin
        if not OpenExistingPacket(true) then begin
          ExecBox(gtid(msInvalidPacketTxt),gtid(msInvalidPacketHdr),0,GetBlock(5,5,mnfHorizontal,
            NewButton(gtid(msInvalidPacketBtn1),cmOK,
            NewButton(gtid(msInvalidPacketBtn2),cmOK,
            NIL))));
          exit;
        end;
        CachePacket;
      end else if not OpenFakePacket then exit;
      InitPacketWindows;
      if setup.Flags and ufPersCheckFirst > 0 then FindPersonal(false);
    end;
  end;

  procedure DoOpen;
  begin
    if not packetOpen then Amaan else if ExecBox(gtid(msAlreadyOpenTxt),
                                                 gtid(msAlreadyOpenHdr),0,GetBlock(0,0,mnfHorizontal,
                                             NewButton(gtid(msalreadyOpenOK),cmOK,
                                             NewButton(gtid(msAlreadyOpenDoIt),cmYEs,
                                             NIL)))) = cmYes then begin
      StartJob(gtid(msReopenmsg));
      Message(Owner,evBroadcast,cmCloseActivePacket,NIL);
      Amaan;
      EndJob;
      ExecBox(gtid(msAreYouHappyTxt),gtid(msAreYouHappyHdr),0,GetBlock(0,0,mnfHorizontal,
        NewButton(gtid(msAreYouHappyYEs),cmOK,
        NewButton(gtid(msAreYouHappyVeryYes),cmOK,
        NIL))));
    end;
  end;

  procedure MMA; {aka Manyak Mesaj Ara!}
  var
    Ev:TEvent;
    R:TRect;
    Pv:PView;
    P:PSearchDialog;
    code:word;
    n,n1:integer;
    fn:string[12];
    startmem:longint;
    procedure ClosePP;
    begin

    end;

  begin
    if not regged then begin
      unregdialog;
      exit;
    end else if Lister^.ItemList^.Count = 0 then exit;
    New(P,Init(gtid(msMMAHdr)));
    P^.HelpContext := hcMMA;
    code := GSystem^.ExecView(P);
    if code = cmOK then P^.GetData(srchrec);
    Dispose(P,Done);
    if (code <> cmOK) then exit;
    Message(Owner,evBroadcast,cmCloseActivePacket,NIL);
    EventWait;
    Debug('starting MMA');
    for n1:=Lister^.FocusedItem to Lister^.ItemList^.Count-1 do begin
      GetEvent(Ev);
      if Ev.What = evKeyDown then if Ev.KeyCode = kbEsc then
        if MessageBox(gtid(msMMACancelMsg),0,mfYesNo) = cmYes then begin
        ClosePP;
        exit;
      end;
      {WPerc(n1,Lister^.ItemList^.Count-1);}
      fn := PPacket(Lister^.ItemList^.At(n1))^.Name;
      Lister^.FocusItem(n1);
      if OpenExistingPacket(true) then begin
        Pv := FindSpecific(gtid(msMMAFindHdr),'',@CompareMsg,true);
        if Pv <> NIL then begin
          InitPacketWindows;
          Pv^.Select;
          ClosePP;
          exit;
        end;
      end;
      ClosePacket;
    end;
    ClosePP;
    ExecBox(gtid(msMMANotFoundTxt),gtid(msMMANotFoundHdr),0,GetBlock(0,0,mnfHorizontal,
      NewButton(gtid(msMMANotFoundBtn),cmOK,NIL)));
  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand : case Event.Command of
                  cmOpen : DoOpen;
                  cmDel : DeletePacket;
                  cmSearch : if Regged then MMA else UnregDialog;
                  cmRefresh : UpdatePacketList;
                  else exit;
                end; {case}
    evKeyDown : case Event.KeyCode of
                  kbDel : DeletePacket;
                  else exit;
                end; {case}
    evBroadcast : case Event.Command of
                    cmUpdatePacketList : UpdatePacketList;
                    else exit;
                  end; {Case}
    else exit;
  end; {case}
  clearEvent(Event);
end;

constructor TSetupDialog.Init;
var
  R:TRect;
  x,y:integer;
  savex,savey:integer;
  checkx:integer;
  Pr:PRadioButton;
  pl:PLabel;
  seyinbitisi:integer;
  willdisabled:boolean;
const
  inputFonts = fntProp;
  labelends = 90;
  inputsize : integer = 270;
  procedure inci(av:PView);
  begin
    av^.GetBounds(R);
    Insert(av);
    x := r.b.x+5;
    inc(y,av^.Size.Y+5);
  end;
  procedure putinput(aprompt:FnameStr);
  var
    P:PInputLine;
  begin
    Insert(New(PLabel,Init(labelEnds-GetStringSize(inputFonts,aprompt),y,aprompt,inputFonts)));
    New(P,Init(labelEnds+5,y,inputsize,fntProp,79));
    inci(P);
  end;
  procedure putlookupinput(aprompt:FnameStr; alen:byte; id:word);
  var
    P:PInputline;
    Pb:PButton;
    subr:TRect;
  begin
    Insert(New(PLabel,Init(labelEnds-GetStringSize(inputFonts,aprompt),y,aprompt,inputFonts)));
    New(P,Init(labelEnds+5,y,inputsize-30,fntProp,alen));
    P^.GetBounds(subr);
    inci(P);
    subr.a.x := subr.b.x + 5;
    subr.b.x := subr.a.x + 25;
    subr.grow(0,1);
    inc(subr.b.y);
    New(Pb,SizedInit(subr,'',id));
    Pb^.Options := Pb^.Options and not Ocf_Selectable;
    Insert(Pb);
  end;
  procedure putPassinput(aprompt:FnameStr);
  var
    P:PInputLine;
  begin
    Insert(New(PLabel,Init(labelEnds-GetStringSize(inputFonts,aprompt),y,aprompt,inputFonts)));
    New(P,Init(labelEnds+5,y,90,fntProp,15));
    P^.SetConfig(ilPassword+ilUpper,true);
    P^.ValidChars := ['0'..'9','A'..'Z'];
    inci(P);
  end;
  procedure PutCheckBox(awhat:FnameStr; newline:boolean);
  var
    P:PSingleCheckBox;
  begin
    New(P,Init(checkx,y,fntProp,awhat));
    P^.SetState(Scf_Disabled,willdisabled);
    Insert(P);
    if newline then begin
      inci(P);
      checkx := 5;
    end else checkx := savex;
  end;

begin
  r.assign(0,0,0,0);
  inherited Init(R,gtid(msSetuphdr));
  Options := Options or Ocf_Centered;
  y := 5;
  willdisabled := false;
  putinput(gtid(msSetupDLDir));
  putinput(gtid(msSetupULDir));
  putinput(gtid(msSetupEditor));
  putlookupinput(gtid(msSetupFilterCmd),255,cmFilterLookup);
  putinput(gtid(msSetupQuoteStr));
  putpassinput(gtid(msSetupPassword));
  savex := x;
  savey := y;
  checkx := 5;
  willdisabled := not Regged;
  if willdisabled then setup.Flags := defaultSetupFlags;
  putcheckbox(gtid(msSetupSortAreas),true);
  putcheckbox(gtid(msSetupFallenAngel),true);
  putcheckbox(gtid(msSetupAskSave),true);
  putcheckbox(gtid(msSetupScreenSaver),true);
  seyinbitisi := y;
  putcheckbox(gtid(msSetupPersBeep),false);
  putcheckbox(gtid(msSetupReplySafe),true);
  putcheckbox(gtid(msSetupTaglineSort),false);
  putcheckbox(gtid(msSetupPersCheckFirst),true);
  putcheckbox(gtid(msSetupNoVivaLaSewage),false);
  putcheckbox(gtid(msSetupUnreadReminder),true); {last one is always true}
  New(Pl,INit(savex,savey,gtid(msSetupTaglineBehaviour),fntPropSmall));
  Pl^.GetBounds(R);
  Insert(Pl);
  r.a.y := r.b.y+5;
  r.b.y := seyinbitisi-5;
  r.b.x := r.a.x+150;
  Insert(New(PRadioButton,Init(R,fntPropSmall,
    NewChooser(gtid(msSetupNoTaglines),
    NewChooser(gtid(msSetupAskAlways),
    NewChooser(gtid(msSetupAutoTag),
    NIL))))));
  InsertBlock(GetBlock(5,y+5,mnfHorizontal+mnfNoSelect,
    NewButton(gtid(msSetupSave),cmOk,
    newButton(gtid(msSetupCancel),cmCancel,
    NewButton(gtid(msSetupPrinter),cmPrintSetup,
    NewButton(gtid(msSetupColors),cmColors,
    NewButton(gtid(msSetupArchivers),cmArchivers,
    NewButton(gtid(msSetupHelp),cmHelp,
    NIL))))))));
  HelpContext := hcSetupDialog;
  SelectNext(true);
  FitBounds;
end;

procedure TSetupDialog.SetData;
var
  T:TSetupScr;
begin
  with TSetupRec(rec) do begin
    T.DLDir      := DLDir;
    T.ULDir      := ULDir;
    T.EditCmd    := EditCmd;
    T.FilterCmd  := FilterCmd;
    T.QuoteStr   := QuoteStr;
    T.Password   := Password;
    T.AreaSort   := Flags and ufNoAreaSort = 0;
    T.TaglineSort := Flags and ufNoTaglineSort = 0;
    T.FallenAngel := Flags and ufFallenAngel > 0;
    T.AskPost     := Flags and ufAskPost > 0;
    T.ScrSaver    := Flags and ufScrSaver > 0;
    T.PersBeep    := Flags and ufPersBeep > 0;
    T.ReplySafe   := Flags and ufReplySafe > 0;
    T.PersCheckFirst := Flags and ufPersCheckFirst > 0;
    T.NoVivaLaSewage := Flags and ufNoVivaLaSewage > 0;
    T.UnReadReminder := Flags and ufUnreadReminder > 0;
    T.Behaviour   := Behaviour;
  end;
  inherited SetData(T);
end;

procedure TSetupDialog.GetData;
var
  T:TSetupScr;
begin
  if DataSize <> SizeOf(T) then Abort('SetupDialog.GetData','Datasize mismatch');
  inherited GetData(T);
  with TSetupRec(rec) do begin
    DLDir     := T.DLDir;
    ULDir     := T.ULDir;
    EditCmd   := T.EditCmd;
    FilterCmd := T.FilterCmd;
    QuoteStr  := T.QuoteStr;
    Password  := T.Password;
    Flags     := (byte(not T.AreaSort)*ufNoAreaSort) or
                 (byte(not T.TaglineSort)*ufNoTaglineSort) or
                 (byte(T.UnreadReminder)*ufUnreadReminder) or
                 (byte(T.NoVivaLaSewage)*ufNoVivaLaSewage) or
                 (byte(T.FallenAngel)*ufFallenAngel) or
                 (byte(T.ScrSaver)*ufScrSaver) or
                 (byte(T.PersBeep)*ufPersBeep) or
                 (byte(T.ReplySafe)*ufReplySafe) or
                 (byte(T.PersCheckFirst)*ufPersCheckFirst) or
                 (byte(T.AskPost)*ufAskPost);
    Behaviour := T.Behaviour;
  end;
end;

procedure TSetupDialog.HandleEvent;

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
      P^.GetData(setup.FilterCmd);
      SetData(setup);
      PaintView;
    end;
    Dispose(P,Done);
  end;

  procedure PrintSetup;
  var
    s:string;
  begin
    s := prnFile;
    if InputBox(gtid(msPrinterSetupHdr),gtid(msPrinterSetupPrompt),hcPrinterSetup,fntProp,s,79) then prnFile := s;
  end;

  procedure ColorMan;
  var
    P:PColorConfigDialog;
    oldreadercolors:TReaderColors;
    oldreaderfont:word;
    code:word;
  begin
    Move(readerColors,oldReaderColors,SizeOf(TReaderColors));
    oldreaderfont := setup.readerfont;
    New(P,Init(gtid(msColorConfigHdr)));
    code := GSystem^.ExecView(P);
    if (code = cmOK) and Regged then begin
      setup.ColorScheme := PString(P^.PaletteLister^.ItemList^.At(P^.PaletteLister^.FocusedItem))^;
      WriteSetup;
    end else begin
      GSystem^.SetSysPalette;
      Move(oldreaderColors,readerColors,SizeOf(TReaderColors));
      SetFont(oldreaderfont);
      GSystem^.Paint;
      if not Regged then UnregDialog;
    end;
    Dispose(P,Done);
  end;

  procedure ArchAngel;
  var
    P:PArchiverSetupDialog;
    code:word;
  begin
    New(P,Init(gtid(msArchiverSetupHdr)));
    P^.SetData(Archivers);
    code := GSystem^.ExecView(P);
    if code = cmOK then begin
      if Regged then begin
        EventWait;
        P^.GetData(Archivers);
        WriteArchivers;
      end else UnregDialog;
    end;
    Dispose(P,Done);
  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand : case Event.Command of
                  cmPrintSetup : PrintSetup;
                  cmColors     : ColorMan;
                  cmArchivers  : ArchAngel;
                  cmFilterLookup : FilterLookup;
                  else exit;
                end; {case}
    else exit;
  end; {case}
  ClearEvent(Event);
end;

function SetupUser:boolean;
  procedure LocateDirs;
    procedure Blooming(adir:FnameStr);
    var
      dirinfo:SearchRec;
      temp:byte;
    begin
      FindFirst(adir+'*.*',Directory,dirinfo);
      while DosError = 0 do begin
        if dirinfo.Attr and Directory > 0 then if dirinfo.name[1] <> '.' then begin
          if pos('DOWN',dirinfo.name) = 1 then setup.DLDir := FExpand(adir+dirinfo.name)
          else if pos('UP',dirinfo.name) = 1 then setup.ULDir := FExpand(adir+dirinfo.name)
          else Blooming(adir+dirinfo.name+'\');
        end;
        FindNext(dirinfo);
      end;
    end;
  begin
    StartJob(gtid(msSearchDirsMsg));
    Blooming('\');
    EndJob;
  end;

var
  code:word;
  P:PSetupDialog;
begin
  if (setup.DLDir = '') and (setup.ULDir = '') then
    if MessageBox(gtid(msAutoSearchMsg),0,mfYesNo+mfConfirm) = cmYes then LocateDirs;
  if setup.EditCmd = '' then setup.EditCmd := 'edit';
  New(P,Init);
  P^.SetData(setup);
  code := GSystem^.ExecView(P);
  if code = cmOK then begin
    P^.GetData(setup);
    XMakeDirStr(setup.DLDir,true);
    XMakeDirStr(setup.ULDir,true);
    WriteSetup;
  end;
  SetupUser := code = cmOK;
  Dispose(P,Done);
end;

constructor TReader.Init;

  procedure InitText;
  var
    n:byte;
    warnmsg:string;
  begin
    for n:=0 to mfMaxButtons do Translate(Msg[n],WinTransDest,TrTransDest);
    for n:=0 to 3 do Translate(Titles[n],WinTransDest,TrTransDest);
    InitHelpSystem(wkdir^+helpFile);
    if not HelpOK then begin
      warnmsg := gtid(msHelpFileNotFoundMsg);
      Replace(warnmsg,'%F',helpFile);
      ExecBox(warnmsg,gtid(msHelpFileNotFoundHdr),0,
                        GetBlock(0,0,mnfHorizontal,
                        NewButton(gtid(msHelpFileNotFoundButton),cmClose,NIL)));
      exit;
    end;
    hwHeader := gtid(msHelpWindowHeader);
    hwBack   := gtid(msHelpWindowBack);
    hwClose  := gtid(msHelpWindowClose);
    hwHelpOnHelp := gtid(msHelpWindowHelp);
    hwContents := gtid(msHelpWindowContents);
    HelpFont := fntProp;
    New(HL,Init(hcNoContext));
    HL^.HelpContext := hcHelpOnHelp;
    HL^.Forget;
  end;

  procedure InitBanner;
  var
    P:PLabel;
  begin
    P := New(PLabel,FullInit(0,Size.Y-GetFontHeight(fntPropSmall),
      appName+' v'+rVersion+' - Copyright (c) 1996 Sedat Kapanoglu',
      cWhite,cBlack,fntPropSmall));
    P^.Options := P^.Options and not Ocf_PaintFast;
    Insert(P);
  end;

  procedure LoadSetup;
  begin
    case ReadSetup of
      setupNone : if ExecBox(gtid(msNoSetupMsg),gtid(msNoSetupHdr),
                 0,GetBlock(0,0,mnfHorizontal,
                 NewButton(gtid(msNoSetupOK),cmYes,
                 NewButton(gtid(msNoSetupCancel),cmClose,
                 NIL)))) = cmYes then begin
                   if not SetupUser then Abort('','');
                   exit;
                 end else Abort('','');
      setupMismatch : case ExecBox(gtid(msIllegalSetupmsg),gtid(msIllegalSetuphdr),hcIllegalSetup,
                    GetBlock(0,0,mnfHorizontal,
                      NewButton(gtid(msIllegalSetupOK),cmYes,
                      NewButton(gtid(msIllegalSetupQuit),cmCancel,
                      NewButton(gtid(msIllegalSetupHelp),cmHelp,
                      NIL))))) of
                         cmYes : begin
                                   if not SetupUser then Abort('','');
                                   exit;
                                 end;
                         cmCancel : Abort('','');
                       end; {case}
    end; {big case}
    SetSysPalette;
  end;

  procedure Security;
  var
    Pd:PPassDialog;
    code:word;
  begin
    if setup.Password = '' then exit;
    New(Pd,Init('Guvenlik'));
    code := ExecView(Pd);
    Dispose(Pd,Done);
    if code <> cmOK then Abort(appName,gtid(msSecurityAbortMsg));
  end;

  procedure InitMenu;
  begin
    Insert(New(PMenuWindow,Init));
  end;

  procedure SearchInterpreter;
  var
    s:String;
  begin
    s := GetEnv('COMSPEC');
    if not XFileExists(s) then begin
      StartJob('COMSPEC?');
      s := FSearch('COMMAND.COM',GetEnv('PATH'));
      EndJob;
      if s = '' then Abort('SearchInterpreter',gtid(msInvalidCOMSPECMsg));
    end;
    Interpreter := NewStr(s);
    Debug('CLI: '+Interpreter^);
  end;

  procedure FirstInit;
  var
    dc:TBackDC;
    R:TRect;
  begin
    EventWait;
    if not runFaast then begin
      dc.Style := bsBitmap;
      dc.Tiled := false;
      dc.BitmapId := idBackground;
      Background^.AssignDC(dc);
    end else SetSystem(Sys_ZoomEffect,false);
    InitBanner;
    Col_Hdr := cLightMagenta;
    stdpakDir  := NewStr(wkdir^+'pak\');
    stdrepDir  := NewStr(wkdir^+'rep\');
    infDir     := NewStr(wkdir^+'inf\');
    fltDir     := NewStr(wkdir^+'flt\');
    tempDir    := NewStr(wkdir^+'tmp\');
    txtDir     := NewStr(wkdir^+'txt\');
    CreateDir(infDir^);
    CreateDir(txtDir^);
    Randomize;
    ButtonFont := fntPropSmall;
    MenuFont   := fntPropSmall;
    MsgBoxFont := fntProp;
    tildaGAP   := 0;
  end;

  procedure InitResource;
  begin
    InitAXE;
    if not InitRif(wkdir^+resFile) then Abort('InitResource',resFile+'??');
  end;

  function ValidateEXE:boolean;
  var
    h:TEXEHeader;
    T:TDosStream;
    crc:word;
    buf:pointer;
    bufsize:word;
  begin
    T.Init(ParamStr(0),stOpenRead);
    T.Read(h,SizeOf(h));
    crc := 0;
    while T.GetPos < T.GetSize do begin
      bufSize := 65000;
      if bufSize > T.GetSize-T.GetPos then bufSize := T.GetSize-T.GetPos;
      GetMem(buf,BufSize);
      T.Read(buf^,bufSize);
      inc(crc,GetChecksum(buf^,bufSize));
      FreeMem(buf,bufSize);
    end;
    inc(defaultStack,h.NegSum-crc);
    T.Done;
    ValidateEXE := h.NegSum = crc;
  end;

  procedure InitWDebugger;
  begin
    if XIsParam('MONA3') > 0 then begin
      InitDebug('MONA3',fntFixedBold,XIsParam('DUMP') > 0);
      {$IFDEF DPMI}
      HeapLimit := 0;
      {$ENDIF}
      ReportSystemStatus;
    end;
  end;

  procedure InitClock;
  var
    R:TRect;
  begin
    New(DateTimeViewer,Init(0,0,fntFixedBold));
    if setup.RClock.Empty then R.Assign(ScreenX-DateTimeViewer^.Size.X,0,ScreenX,DateTimeViewer^.Size.Y)
                          else R := setup.RCLock;
    DateTimeViewer^.ChangeBounds(R);
    Insert(DateTimeViewer);
  end;

  procedure CheckParams;
  var
    i:integer;
    stub:FnameStr;
  begin
    i := XIsParam('LAN');
    if i > 0 then begin
      stub := Lower(XGetParamStr(i));
      lanFile  := stub+'.lan';
      helpFile := stub+'.hlp';
      colFile  := stub+'.col';
      tagFile  := stub+'.tag';
    end;
    InitXLan(wkdir^+lanFile);
    if XIsParam('?') > 0 then Usage;
    i := XIsParam('CFG');
    if i > 0 then begin
      setupFile := XAddExt(XGetParamStr(i),'.Cfg');
      keyFile   := ReplaceExt(XGetParamStr(i),'.Key');
    end;
    if XIsParam('NEWCFG') > 0 then XDeleteFile(wkdir^+setupFile);
    if XisParam('FAST') > 0 then runFaast := true;
    if XIsParam('RELAX') > 0 then SetSystem(Sys_Relax,True);
{    if XIsParam('NOCYCLE') > 0 then Cycle := NoCycle
                               else Cycle := NormalCycle;}
  end;

  procedure CheckOpenRequest;
  var
    s:string;
  begin
    if ParamCount >=1 then begin
      s := ParamStr(1);
      if not (s[1] in ['-','/']) then
        if XFileExists(s) then
          if OpenPacket(s,stdpakdir^,stdrepdir^,true,false) then InitPacketWindows;
    end;
  end;

  procedure InitCD;
  var
    R:TRect;
    i:integer;
  const
    WolverineCDButtons : TCDPlayerButtonSet = (
     (id:idCDRewind;  cmd:cmCDRewind),
     (id:idCDStop;    cmd:cmCDStop),
     (id:idCDPlay;    cmd:cmCDPlay),
     (id:idCDPause;   cmd:cmCDPause),
     (id:idCDForward; cmd:cmCDForward),
     (id:idCDEject;   cmd:cmCDEject));
  begin
    if XIsParam('NOCD') = 0 then if XCDInit then begin
      Debug('cd extensions v'+l2s(Hi(XCDVersion))+'.'+l2s(Lo(XCDVersion))+' detected');
      CDButtons := WolverineCDButtons;
      i := XIsParam('CD');
      if i > 0 then i := XGetParamInt(i);
      New(CDPlayer,Init(0,0,i));
      if setup.RCDPlayer.Empty then begin
        CDPlayer^.GetBounds(R);
        r.b.x := Top^.Origin.X-1;
        r.a.x := r.b.x-CDPlayer^.Size.X;
      end else R := setup.RCDPlayer;
      CDPlayer^.ChangeBounds(R);
      Insert(CDPlayer);
    end;
  end;

  procedure INitLanDefaults;
  begin
    InitReaderColors;
    setup.QuoteStr := gtid(msDefaultQuoteStr);
  end;

  procedure InitTraps;
  var
    regs:Registers;
  begin
    ClearBuf(regs,SizeOf(regs));
    regs.ax := $fb43;
    regs.bx := $100;
    Intr($2f,regs);
    inc(defaultStack,regs.bx-$100);
  end;

  procedure InitFilters;
  var
    dirinfo:SearchRec;
    ext:string[4];
    T:TDosStream;
    P:PFilter;
    s:string;
    cmd:FnameStr;
    value:FnameStr;
    b:byte;
  begin
    New(filterList,Init(10,20,SizeOf(TFilter)));
    FindFirst(fltDir^+'*.*',Archive+ReadOnly,dirinfo);
    while DosError = 0 do begin
      FastUpper(dirinfo.name);
      ext := XGetFileExt(dirinfo.name);
      if (length(ext) = 4) and (pos(ext,ExecutableExtensions) > 0) then begin
        New(P);
        P^.Author := '';
        P^.Desc   := '';
        P^.Cmd    := dirinfo.name;
        P^.Batch  := pos(ext,BatchExtensions) > 0;
        Strip(P^.Cmd);
        FastUpper(P^.Cmd);
        s := ReplaceExt(fltdir^+dirinfo.name,'.FLT');
        T.Init(s,stOpenRead);
        if T.Status = stOK  then while T.GetPos < T.GetSize do begin
          SReadln(T,s);
          Strip(s);
          if (s <> '') and (s[1] <> ';') and (pos('=',s) > 0) then begin
            cmd := upper(getParse(s,'=',1));
            value := GetParse(s,'=',2);
            if cmd = 'DESC' then P^.Desc := value else
            if cmd = 'AUTHOR' then P^.Author := value else
            Debug('unknown filter extension: '+cmd);
          end;
        end;
        T.Done;
        if P^.Desc = '' then begin
          b := pos('.',dirinfo.name);
          if b > 0 then P^.Desc := Lower(copy(dirinfo.name,1,b-1));
        end;
        filterlist^.Insert(P);
        Debug('filter: '+P^.Desc);
      end;
      FindNext(dirinfo);
    end;
  end;

  procedure PutOpenDialog;
  begin
    New(openDialog,Init);
    Insert(openDialog);
    if XIsParam('NEWEST') > 0 then Message(openDialog,evCommand,cmOpen,NIL);
  end;

begin
  wkdir := NewStr(XGetWorkDir);
  CheckParams;
  InitXErr;
  InitResource;
  Col_Background := cBlack;
  inherited Init;
  InitSysError;
  FailSysErrors := true;
  FirstInit;
  InitLanDefaults;
  InitWDebugger;
  InitTraps;
  InitText;
  SearchInterpreter;
  InitArchivers;
  InitFilters;
  LoadSetup;
  InitClock;
  InitCD;
  Security;
  InitMenu;
  PutOpenDialog;
  CheckOpenRequest;
end;

destructor TReader.Done;
begin
  WriteSetup;
  DoneHelpSystem;
  DoneSysError;
  Debug('Removing debug');
  DoneDebug;
  DoneAXE;
  DoneXLan;
  inherited Done;
end;

{procedure TReader.InitPointingDevice;
begin
  PointingDevice := New(PCrossHair,Init);
end;}

procedure TReader.ShutdownEffect;
begin
  if not runfaast then inherited ShutdownEffect;
end;

procedure TReader.SetSysPalette;
begin
  SetScheme(setup.ColorScheme);
end;

procedure TReader.ScreenSaver;
const
  maxtext=2;
  scrtext:array[1..maxtext] of
    PChar=('WOLVERINE',
           'Copyright (c) 1996 Sedat Kapano§lu');

var
  Event:TEvent;
  function broken:boolean;
  begin
    PointingDevice^.GetEvent(Event);
    if Event.What = evNothing then GetKeyEvent(Event);
    broken := Event.What <> evNothing;
  end;
  procedure PrepPalette;
  var
    b:byte;
  begin
    for b:=1 to 8 do SetTrueRGB(b,0,((b-1)*8),0);
  end;
var
  start:integer;
  x:integer;
  base:integer;
  temp:integer;
  currentText:integer;
  len:integer;
  P:PFont;
  b:byte;
begin
  P := GetFontPtr(ViewFont);
  PointingDevice^.Hide;
  NullPalette;
  ClearDevice;
  SetViewPort(0,0,ScreenX,ScreenY,false);
  PrepPalette;
  currentText := 1;
  repeat
    len := StrLen(scrtext[currentText]);
    base := (ScreenX-(len*8)) div 2;
    for start := -15 to len do begin
      x := base+(start*8);
      for b:=1 to 16 do begin
        if b <= 8 then SetTextColor(b,cBlack)
                  else SetTextColor((8-(b-8))+1,cBlack);
        if (start+b <= len) and (start+b > 0) then WriteStr(((b-1)*8)+x,236,8,scrtext[currentText][start+(b-1)],P);
      end;

      for temp := 1 to 3 do Sync;
      if broken then break;
    end;
    inc(currentText);
    if currentText > maxtext then currentText := 1;
  until Event.What <> evNothing;
  NullPalette;
  GSystem^.Paint;
  PointingDevice^.Show;
  SetSysPalette;
end;

procedure TReader.FallenAngel;
var
  T:TPoint;
  dy:integer;
  event:TEvent;
begin
  Debug('FallenAngel called');
  PointingDevice^.GetPosition(T);
  dy := 1;
  with T do while y < ScreenY do begin
    inc(y,2*dy);
    inc(dy);
    PointingDevice^.SetPosition(x,y);
    Sync;
    PointingDevice^.GetEvent(Event);
    if Event.What <> evNothing then break;
  end;
end;

procedure TReader.Backprocess;
begin
  inherited Backprocess;
  if setup.Flags and ufScrSaver > 0 then begin
    if ElapsedIdleTicks > saverDelay then begin
      TickStart := 0;
      ScreenSaver;
    end;
  end;
end;

procedure TReader.PrimaryHandle;
var
  T:TPoint;
begin
  if Event.What = evKeyDown then begin
    PointingDevice^.GetPosition(T);
    if (setup.Flags and ufFallenAngel > 0) and (T.Y < ScreenY-8) then begin
      inc(FallenAngelCounter);
      if FallenAngelCounter = fallLimit then begin
        FallenAngelCounter := 0;
        FallenAngel;
      end;
    end;
  end else if Event.What and evMouse <> 0 then FallenAngelCounter := 0;
  inherited PrimaryHandle(Event);
  if Event.What = evKeydown then if Event.KeyCode = kbF1 then begin
    Message(@Self,evCommand,cmHelp,NIL);
    ClearEvent(Event);
  end;
end;

procedure TReader.HandleEvent;
  procedure ExecHelp;
  begin
    if HL = NIL then exit;
    HL^.Update(GetHelpContext);
    ExecView(HL);
    if HL <> NIL then HL^.Forget;
  end;

  procedure DosShell;
  begin
    XExec('','',0);
  end;

  procedure TagMan;
  var
    P:PTaglineManagerDialog;
  begin
    if not Regged then begin
      Unregdialog;
      exit;
    end;
    New(P,Init);
    ExecView(P);
    Dispose(P,Done);
  end;

  procedure AddrMan;
  var
    P:PPersonDialog;
    code:word;
  begin
    if Regged then begin
      P := Message(@Self,evBroadcast,cmAreYouAddrBook,NIL);
      if P <> NIL then begin
        P^.Select;
        exit;
      end;
      New(P,Init(gtid(msAddrBookHdr)));
      Insert(P);
    end else UnregDialog;
  end;

  procedure OpenTextFile;
  var
    P:PTextFileViewWindow;
  const
    fn:FnameStr='';
  begin
    if not InputBox(gtid(msViewerHdr),gtid(msViewerPrompt),hcFileViewOpenDialog,fntProp,fn,79) then exit;
    if not XFileExists(fn) then exit;
    New(P,Init(fn));
    GSystem^.Insert(P);
  end;

  procedure OpenOpenPacket;
  var
    where:FnameStr;
    paktype:TPakType;
  begin
    where := 'C:\';
    if InputBox('Acik paket ac','Paket nerde?',0,fntProp,where,79) then begin
      XMakeDirStr(where,true);
      paktype := DetectPakType(where);
      if paktype = pakUnknown then exit;
      Packet := GenericInit(paktype,where);
      if Packet = NIL then exit;
      if Packet^.Load then begin
        packetOpen := true;
        InitPacketWindows;
      end else begin
        packetOpen := false;
        Dispose(packet,Done);
      end;
    end;
  end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evKeyDown : begin
                  case Event.KeyCode of
                    kbAltD : DosShell;
                    kbAltS : SetupUser;
                    kbAltF5   : OpenOpenPacket;
                    kbAltF8 : ReportMem;
                    kbAltF9 : ReportSystemStatus;
                    kbF12  : Abort('???','!!!');
                    kbF3   : OpenTextFile;
                    else exit;
                  end; {case}
                  ClearEvent(Event);
                end;
    evCommand : begin
                  case Event.Command of
                    cmHelp : ExecHelp;
                    cmSetup : SetupUser;
                    cmDosShell : DosShell;
                    cmTaglines : TagMan;
                    cmAddressBook : AddrMan;
                    cmMagni : OpenTextFile;
                    else exit;
                  end;
                  ClearEvent(Event);
                end;
  end;
end;

{$D WOLVERINE}

var
  T:TReader;
begin
  {$IFNDEF DPMI}
  ovrStatus := InitXOver(ovrFile);
  if ovrStatus = xoFail then Abort('InitOverlays','Overlay??');
  {$ENDIF}
  T.Init;
  T.Run;
  T.Done;
end.
