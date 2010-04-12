{
wolverine notes

P+All olayi yerine daha esnek bi yontem:

if To=ALL or From=Bi$ii falan

kendime bi$ii ozel paket formati...

Wham! Wolverine packet format

packet structure:

INFO.W   information about packet
INDEX.W  message index
TEXT.W   raw text (optional)

INFO.W format:
~~~~~~~~~~~~~~
Size     Description
~~~~     ~~~~~~~~~~~~~~~~~~
DWORD    signature $4d414857 = 'WHAM'
WORD     Packet type
           00 = Standard mail packet
           01 = Reply packet
           02 = Archive packet
WORD     Packet flags
           bit 0 - packet is encrypted
WORD     Permitted netmail flags
STR39    Origin of packet (i.e. "Dead-Dragon BBS")
STR39    SysOp of system
8BYTES   Fido-style address of system
STR39    Name of user who is owner of the packet
STR39    Alias of user
STR39    Creator of packet (i.e. "M-Door")
???      Padding to 512 bytes

after header, there comes area records... format of each area
record is:

Size       Description
~~~~       ~~~~~~~~~~~~~~~~~~~~~~~~
STR49      Area name
STR5       Area number
BYTE       Area type
             0 = local
             1 = echo
             2 = netmail
             3 = internet e-mail
             4 = info area (new files etc)
BYTE       User status
             0 = user doesn't read this area
             1 = gets only personal msgs
             2 = gets P+All
             3 = gets all messages
WORD       Area flags
             bit 0 - Aliases allowed
                 1 - User can write messages to this area
68BYTES    Null-padded

INDEX.W format
~~~~~~~~~~~~~~
Size         Description
~~~~         ~~~~~~~~~~~~~~
STR39        From: field
STR39        To: field
STR79        Subj: field
DWORD        Message date (MSDOS format)
ADDR         Origin of message
ADDR         Destination of message
DWORD        Message flags
               bit 0 - message has been read
                   1 - message has been replied
                   2 - message is personal
                   3 - message has been deleted
                   4 - message is marked for reply
                   5 - message is marked for read
                   6 - message is marked for save

DWORD        Message size
DWORD        Message offset (-1 if linked to another file)
STR12        Filename that message is linked
WORD         Netmail flags
33BYTES      Null padded

XMF (Xtended Message Format)

HTML KEYWORDS LIST (netscape 2.0)
~~~~~~~~~~~~~~~~~~
head,a,tt,p,table,applet,b,select,html,textarea,caption,ul,th,meta,title,
map,input,body,plaintext,option,keygen,param,hype,xmp,font,dt,cite,h5,
div,code,hb,em,image,i,link,dl,kbd,nobr,tr,big,area,h1,pre,noframes,form,
br,blink,blockquote,h2,cell,frame,frameset,embed,noembed,img,center,li,
colormap,menu,h4,ol,base,td,basefont,wbr,small,dd,var,strike,sup,isindex,
listing,sub,strong,script,subdoc,address,hr,dir

head            = head of document (unused)
a               = symbolic link
tt              = teletype mode
p               = new paragraph
table           = ??? table??
applet          = java applet (won't be used)
b               = bold
select          = ???
html            = ??? (no effect)
textarea        = ???
caption         = ??? (no effect)
ul              = ???
th              = ??? (table something??)
meta            = ???
title           = window title
map             = symbolic link map
input           = user input request
body            = body of document (unused)
plaintext       = switches to fixed font and ignores any further HTML
                  directives...
option          = ???
keygen          = ???
param           = ???
hype            = hyphenate??
xmp             = switches to fixed font and ignores any HTML directives
                  until an /XMP directive is being encountered...
font            = changes font style...
dt              = ???
cite            = italic
h1,h2,h3,h4,h5  = font size selection...
div             = causes a line break??
code            = tt, samp, pre, xmp, listing
hb              = ???
em              = emphasis (italic)
i               = italic
dl              = ???
kbd             = keyboard entry font
nobr            = ???
tr              = ???
big             = big font select
area            = ???
pre             = fixed size font selection
noframes        = ???
form            = entry form??
br              = line break??
blink           = blink
blockquote      = quoted text
cell            = ???
frame           = ???
frameset        = ???
embed           = ???
noembed         = ???
img             = embed image
center          = center
li              = list item
colormap        = ???
menu            = ???
ol              = tabulates text?? what?
base            = ???
td              = ???
basefont        = ???
wbr             = ???
small           = small font selection
dd              = ???
var             = ???
strike          = strike
sup             = superscript
sub             = subscript
isindex         = ???
strong          = bold
script          = script-like font selection
subdoc          = ???
address         = e-mail address like font selection (italic)
script          = javascript
hr              = tearline creator
dir             = tabulation??

}
