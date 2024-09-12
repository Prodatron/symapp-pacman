;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               P a c - M a n                                @
;@                                                                            @
;@             (c) 2004-2023 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;Todo
;- fress punkte zeigen
;- sound


;==============================================================================
;### C O D E   A R E A #######################################################
;==============================================================================


;### PRGPRZ -> Programm-Prozess
dskprzn     db 2
sysprzn     db 3
windatprz   equ 3   ;Prozeßnummer
windatsup   equ 51  ;Nummer des Superfensters+1 oder 0
prgwin      db 0    ;Nummer des Haupt-Fensters
diawin      db 0    ;Nummer des Dialog-Fensters

prgprz  ld a,(prgprzn)
        ld (prgwindat+windatprz),a
        ld (configwin+windatprz),a

        call sndini
        call gamini

        ld hl,gamtims           ;Game-Timer
        ld a,(App_BnkNum)
        call SyKernel_MTADDT
        jp c,prgend
        ld (prgprztab+0),a

        ld c,MSC_DSK_WINOPN
        ld a,(App_BnkNum)
        ld b,a
        ld de,prgwindat
        call msgsnd             ;Fenster aufbauen
prgprz1 call msgdsk             ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        cp MSR_DSK_WOPNER
        jp z,prgend             ;kein Speicher für Fenster -> Prozeß beenden
        cp MSR_DSK_WOPNOK
        jr nz,prgprz1           ;andere Message als "Fenster geöffnet" -> ignorieren
        ld a,(prgmsgb+4)
        ld (prgwin),a           ;Fenster wurde geöffnet -> Nummer merken

prgprz0 call gamupd
        rst #30
        call msgget
        jr nc,prgprz0
        cp MSR_DSK_WCLICK       ;*** Fenster-Aktion wurde geklickt
        jr nz,prgprz0
        ld e,(iy+1)
        ld a,(prgwin)
        cp e
        jr z,prgprz4
        ld a,(diawin)
        cp e
        jr nz,prgprz0
        ld a,(iy+2)             ;*** DIALOG-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,prgcfgc
        jr prgprz5
prgprz4 ld a,(iy+2)             ;*** HAUPT-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,prgend
prgprz5 cp DSK_ACT_KEY          ;*** Taste wurde gedrückt
        jr z,prgkey
        cp DSK_ACT_CONTENT      ;*** Inhalt wurde geklickt
        ld l,(iy+8)
        ld h,(iy+9)
        ld a,l
        or h
        jr z,prgprz0
        ld a,(iy+3)             ;A=Klick-Typ (0/1/2=Maus links/rechts/doppelt, 7=Tastatur)
        jp (hl)

;### PRGKEY -> Taste auswerten
prgkeya equ 1
prgkeyt db 255:dw prgend

prgkey  ld hl,prgkeyt
        ld b,prgkeya
        ld de,3
        ld a,(iy+4)
        call clcucs
prgkey1 cp (hl)
        jr z,prgkey2
        add hl,de
        djnz prgkey1
        jp prgprz0
prgkey2 inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld a,7
        jp (hl)

;### PRGEND -> Programm beenden
prgend  call sndfre
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        ld (iy+0),MSC_SYS_PRGEND
        ld a,(App_BegCode+prgpstnum)
        ld (iy+1),a
        rst #10
prgend0 rst #30
        jr prgend0

;### PRGCFG -> Config window
prgcfg  ld de,configwin
prgcfg1 ld c,MSC_DSK_WINOPN     ;open window
        ld a,(App_BnkNum)
        ld b,a
        call msgsnd
prgcfg2 call msgdsk             ;get Message -> IXL=Status, IXH=sender-Prozess
        cp MSR_DSK_WOPNER
        jp z,prgprz0            ;no memory -> nothing
        cp MSR_DSK_WOPNOK
        jr nz,prgcfg2           ;other message than "windows opend" -> ignore
        ld a,(prgmsgb+4)
        ld (diawin),a           ;window opend -> store number
        inc a
        ld (prgwindat+windatsup),a
        jp prgprz0
prgcfgc ld c,MSC_DSK_WINCLS     ;close config window
        ld a,(diawin)
        ld b,a
        call msgsnd
        ld a,(configkey)        ;controller setting
        or a
        ld hl,256*01+08
        ld de,256*02+00
        jr z,prgcfg3
        ld hl,256*75+74
        ld de,256*73+72
prgcfg3 ld a,l:ld (pacmova+1),a
        ld a,h:ld (pacmovb+1),a
        ld a,e:ld (pacmovc+1),a
        ld a,d:ld (pacmovd+1),a
        jp prgprz0


;==============================================================================
;### SOUND ROUTINES ###########################################################
;==============================================================================

SND_BEGIN       equ 0
SND_EATPOINT    equ 1
SND_DEATH       equ 2
SND_EATFRUIT    equ 3
SND_EATGHOST    equ 4
SND_NEWLIFE     equ 5


snddvcflg   db 0    ;0=no hardware, 1=use psg, 2=use opl4
sndhndefx   db -1   ;effect handler
sndhndmus   db -1   ;music handler

snddatfil   db "pacman.dat",0:snddatfil0
snddatpth   ds 128
snddathnd   db 0
snddatbuf   ds 4

;### SNDPTH -> prepares sound data file path
sndpth  ld de,snddatpth
        ld hl,snddatfil
sndpth0 push hl
        ld hl,(App_BegCode)
        ld bc,App_BegCode
        dec h
        add hl,bc           ;HL = CodeEnd = path
        ld bc,128
        ldir
        ld hl,-128
        add hl,de
        ld b,127
sndpth1 ld a,(hl)           ;search end of path
        or a
        jr z,sndpth2
        inc hl
        djnz sndpth1
        jr sndpth4
        ld a,127
        sub b
        jr z,sndpth4
        ld b,a
sndpth2 ld (hl),0
        dec hl              ;search start of filename
        call sndpth5
        jr z,sndpth3
        djnz sndpth2
        jr sndpth4
sndpth3 inc hl
        ex de,hl
sndpth4 pop hl              ;replace application filename with config filename
        ld bc,snddatfil0-snddatfil
        ldir
        ret
sndpth5 ld a,(hl)
        cp "/"
        ret z
        cp "\"
        ret z
        cp ":"
        ret

;### SNDINI -> inits sound module and loads music and effects
sndini  call SySound_SNDINI         ;search and set Sound Daemon
        ret c
        ld a,l
        ld (snddvcflg),a
        call sndpth
        ld ix,(App_BnkNum-1)
        ld hl,snddatpth
        call SyFile_FILOPN
        ret c
        ld (snddathnd),a
        ld hl,(snddvcflg)
        dec l
        ld ix,4
        ld iy,0
        ld b,a
        ld a,1
        jr z,sndini1
        ld a,b
        ld hl,snddatbuf
        ld bc,4
        ld de,(App_BnkNum)
        push af
        call SyFile_FILINP
        pop bc
        jr c,sndini2
        ld ix,(snddatbuf+0)
        ld iy,(snddatbuf+2)
        ld a,128
sndini1 ld (sndefx0+1),a
        ld a,b
        ld c,0
        call SyFile_FILPOI
        jr c,sndini2
        ld a,(snddvcflg)
        ld d,a
        ld e,0
        ld a,(snddathnd)
        push af
        push de
        call SySound_EFXLOD
        pop de
        pop bc
        jr c,sndini2
        ld (sndhndefx),a
        ld a,b
        call SySound_MUSLOD
        jr c,sndini2
        ld (sndhndmus),a
        ld h,192
        call SySound_MUSVOL
sndini2 ld a,(snddathnd)
        jp SyFile_FILCLO

;### SNDFRE -> removes music and effects
sndfre  ld a,(sndhndefx)
        inc a
        jr z,sndfre1
        dec a
        call SySound_EFXFRE
sndfre1 ld a,(sndhndmus)
        inc a
        ret z
        dec a
        jp SySound_MUSFRE

;### SNDMUS -> starts or stops music
;### Input      L=subsong ID (255=stop)
;### Destroyed  AF,BC,DE,HL,IX,IY
sndmus  ld a,(sndhndmus)
        inc a
        ret z
        dec a
        inc l
        jp z,SySound_MUSSTP
        dec l
        jp SySound_MUSRST

;### SNDEFX -> play effect
;### Input      L=effect ID, H=priority (1=high, 2=low)
;### Destroyed  AF,BC,DE,HL,IX,IY
sndefx  ld a,(sndhndefx)
        inc a
        ret z
        dec a
        ld b,h
        ld h,255
sndefx0 ld c,1
        ld de,0
        jp SySound_EFXPLY


;==============================================================================
;### GAME ROUTINES ############################################################
;==============================================================================

gamdatmod   db 0    ;0=demo, 1=game
gamdatpau   db 0    ;1=pause
gamdatsta   db 0    ;0=no status change, 1=pacman died, 2=next level
gamdatani   db 0    ;animation status (1=new animation)
gamdatcnt   db 0    ;animation counter (0-255)
gamdatfld   db 0    ;flag, if field should be updated
gamdatblu   db 0    ;counter for blue-ghost status
gamdateat   db 0    ;counter for eaten ghosts
gamdatliv   db 0    ;number of lives
gamdatlev   db 0    ;number of level
gamdatpoi   db 0    ;number of missing points for the current level
gamdatlsc   dw 0    ;score difference for the next life
gamdattim   dw 0    ;game timer
gamdatfru   db 0    ;bonus fruit status (0=no fruit, 1=fruit disappears, >0=1/17s counter)
gamdatfrs   dw 100,200,500,1000
gamdatfrc   dw 2*256

;### GAMINI -> initialise game and activate demo mode
gamini  ld hl,fldgfx1
        ld de,fldxln*11*8*2+12+1
        add hl,de
        ld (sprremov5+3),hl
        call pacini
        call ghoini
        xor a
        ld (fldnum),a
        call fldmak
        xor a
        ld (gamdatmod),a
        ld (gamdatsta),a
        ld hl,txtbutrun
        ld (prgwinobj3+4),hl
        ld a,64
        ld (prgwinobj0+16+2),a
        ret

;### GAMCTL -> user hit game-control button (start/pause/continue)
gamctl  ld a,(gamdatmod)
        or a
        ld hl,txtbutpau
        jr nz,gamctl1           ;start new game
        ld (prgwinobj3+4),hl
        call gamres
gamctl0 ld e,14
        call gamsta1
        jp prgprz0
gamctl1 ld a,(gamdatpau)        ;pause/continue
        xor 1
        ld (gamdatpau),a
        jr z,gamctl2
        ld hl,txtgampau
        call gammsg
        ld hl,txtbutcon
gamctl2 ld (prgwinobj3+4),hl
        jr gamctl0

;### GAMMSG -> shows an in-between message
;### Input  HL=Text
gammsg  ld (objgammsg),hl
        ld bc,256*1+30
        ld hl,objgammsg
        call gammsg2
        ld b,50*2
gammsg1 push bc
        rst #30
        call msgget
        pop bc
        djnz gammsg1
gammsg0 ld bc,256*10+29     ;restore background
        ld hl,sprremov5
gammsg2 ld (prgwinobj4+4),hl
        ld a,b
        ld (prgwinobj4+2),a
        ld a,c
        ld (prgwingrp),a
        ld e,30
        call gamsta1
        rst #30
        ret

;### GAMEND -> ends game and goes back to demo mode
gamend  ld hl,(gamdatsco+0)
        ld bc,(gamdathig+0)
        or a
        sbc hl,bc
        ld hl,(gamdatsco+2)
        ld bc,(gamdathig+2)
        sbc hl,bc
        jr c,gamend1
        ld hl,txtnumsco+3
        ld de,txtnumhig+3
        ld bc,10+1+4
        ldir
        ld e,16
        call gamsta1
gamend1 ld hl,txtgamovr
        call gammsg
        call gamini
        jp gamsta0

;### GAMRES -> restarts complete game
gamres  xor a
        ld (gamdatlev),a
        ld (gamdatpau),a
        ld (fldnum),a
        ld l,a
        ld h,a
        ld (gamdatsco+0),hl
        ld (gamdatsco+2),hl
        inc a
        ld (gamdatmod),a
        ld (gamdatsta),a
        ld hl,9999
        ld (gamdatlsc),hl
        ld e,17
        call gamsta1
        ld a,3
        ld (gamdatliv),a
        call gamliv
        jr gamnxt

;### GAMNXT -> starts next level
gamnxt  ld hl,256*1+SND_BEGIN
        call sndefx

        ld hl,gamdatlev
        inc (hl)
        ld a,(hl)
        push af
        call gamfru3
        ld (prgwinobj6+4),hl
        ld e,11
        call gamsta1
        pop af
        call clcdez
        ld (txtgamlev+6),hl
        call fldmak
        ld a,(fldnum)
        inc a
        cp fldanz
        jr c,gamnxt1
        xor a
gamnxt1 ld (fldnum),a
        scf
        jr gamsta2

;### GAMSTA -> restarts current level
gamsta  or a
gamsta2 push af
        xor a
        ld (gamdatblu),a
        ld (gamdatfru),a
        ld l,a
        ld h,a
        ld (gamdattim),hl
        inc a
        ld (gamdatsta),a
        call pacini
        call ghoini
        ld a,10
        ld (prgwinobj0+16+2),a
        call gamsta0
        pop af
        ld hl,txtgamlev
        call c,gammsg
        ld hl,txtgamrdy
        call gammsg

        ld l,0
        call sndmus

        xor a
        ld (gamdatsta),a
        ret
gamsta0 ld de,14*256+256-15
gamsta1 ld c,MSC_DSK_WINDIN
        ld a,(prgwin)
        ld b,a
        jp msgsnd

;### GAMLIV -> updates live display
gamliv  ld ix,16*4+prgwinobj2
        ld bc,-16
        ld a,(gamdatliv)
        ld de,5*256+10
gamliv1 dec a
        jr nz,gamliv2
        ld e,64
gamliv2 ld (ix+2),e
        add ix,bc
        dec d
        jr nz,gamliv1
        ld de,5*256+256-6
        jr gamsta1

;### GAMTIM -> Timer process, checks keyboard and manages movements and collisions
gamtim  ld a,(gamdatsta)        ;new status -> do nothing
        or a
        jr nz,gamtim0
        ld hl,gamdatani
        ld (hl),1
        inc hl
        inc (hl)
        ld a,(gamdatpau)        ;pause -> no movements and changes, only animation counter
        or a
        jr nz,gamtim2
        ld a,(gamdatmod)        ;demo mode -> no pacman and game time stuff
        or a
        jr z,gamtim3
        ld hl,(gamdattim)
        inc hl
        ld (gamdattim),hl
        ld a,(gamdatblu)
        sub 1
        jr c,gamtim1
        ld (gamdatblu),a
        call z,ghonrm
gamtim1 call pacmov             ;control pacman
        call gamfru             ;check fruit
gamtim3 ld ix,0*ghodatlen+ghodatmem
        call ghomov
        ld ix,1*ghodatlen+ghodatmem
        call ghomov
        ld ix,2*ghodatlen+ghodatmem
        call ghomov
        ld ix,3*ghodatlen+ghodatmem
        call ghomov
gamtim2 rst #30
        rst #30
        ld a,(configspd)
        or a
        jr z,gamtim4
        rst #30
gamtim4 ld a,(gamdattim+1)
        cp 5
        jr nc,gamtim
gamtim0 rst #30
        jr gamtim

;### GAMFRU -> updates bonus fruit status
gamfru  ld a,(gamdatfru)
        sub 1
        ret z
        jr c,gamfru1
        ld (gamdatfru),a
        ret
gamfru1 ld hl,(gamdatfrc)
        dec hl
        ld (gamdatfrc),hl
        ld a,l
        or h
        ret nz
        ld a,r
        and 1
        inc a
        ld (gamdatfrc+1),a
        ld a,10*17
        ld (gamdatfru),a
        call gamfru3
        ld (prgwinobj4-16+4),hl
        ld a,10
        ld (prgwinobj4-16+2),a
        ret
gamfru3 ld a,(gamdatlev)
        cp 5
        jr c,gamfru2
        ld a,4
gamfru2 ld l,a
        add a:add a:add a
        add l
        ld l,a
        ld h,0
        ld bc,sprbonus1-9
        add hl,bc
        ret

;### GAMUPD -> updates game displays changed by the timer thread
gamupd  ld a,(gamdatani)
        or a
        ret z
        xor a
        ld (gamdatani),a
        ld a,(gamdatsta)
        cp 1
        jr c,gamupd3
        jr z,gamupd4
        call gamupd3            ;*** next level

        ld l,-1
        call sndmus

        jp gamnxt
gamupd4 call pacdie             ;*** PacMan died
        ld hl,gamdatliv
        dec (hl)
        jp z,gamend
        call gamliv
        jp gamsta
gamupd3 ld hl,gamdatfld         ;*** normal gameplay -> update sprites
        bit 0,(hl)
        jr z,gamupd1
        ld (hl),0
        ld de,(gamdatsco+2)
        ld ix,(gamdatsco+0)
        ld iy,txtnumsco+3
        call clcn32
        ld de,17*256+256-2
        call gamsta1
gamupd1 ld ix,0*ghodatlen+ghodatmem
        call ghoshw
        ld ix,1*ghodatlen+ghodatmem
        call ghoshw
        ld ix,2*ghodatlen+ghodatmem
        call ghoshw
        ld ix,3*ghodatlen+ghodatmem
        call ghoshw
        ld a,(gamdatmod)
        or a
        jr nz,gamupd2
        ld de,19*256+256-8
        jp gamsta1
gamupd2 call pacshw
        ld de,19*256+256-10
        ld a,(gamdatfru)
        sub 1
        jp c,gamsta1
        jr nz,gamupd5
        ld (gamdatfru),a
        ld hl,1
        ld (prgwinobj4-16+4),hl
        ld a,0
        ld (prgwinobj4-16+2),a
gamupd5 dec e
        jp gamsta1


;==============================================================================
;### PACMAN ROUTINES ##########################################################
;==============================================================================

pacdatxps   db 0    ;x position (0 -> fldxln-1)
pacdatyps   db 0    ;y position (0 -> fldyln-1)
pacdatxfn   db 0    ;x fine (-4 -> +3)
pacdatyfn   db 0    ;y fine (-4 -> +3)
pacdatxsc   db 0    ;screen x position
pacdatysc   db 0    ;screen y position
pacdatdir   db 0    ;current direction (0=left, 1=right, 2=up, 3=down)
pacdatkey   db 0    ;wanted direction
pacdatmap   dw 0    ;map pointer

;### PACINI -> initialised pacman and sets him to his starting position
pacini  xor a                   ;pacman runs left
        ld (pacdatdir),a
        ld (pacdatkey),a
        ld hl,256*15+9          ;start position
        ld (pacdatxps),hl
        ld l,a
        ld h,a
        ld (pacdatxfn),hl       ;no fine-moving
        ld a,255                ;there is no old pacman, so delete "old" pacman outside visible area
        ld (pacdatysc),a
        ld ix,pacdatxps
        call fldadr
        jp pacshw

;### PACMOV -> Updates pacmans position
pacmov  ld a,-1                             ;*** scan keyboard for new direction
pacmova ld e,8:call pacmov1:jr z,pacmov2        ;left
pacmovb ld e,1:call pacmov1:jr z,pacmov2        ;right
pacmovc ld e,0:call pacmov1:jr z,pacmov2        ;up
pacmovd ld e,2:call pacmov1:jr nz,pacmov3       ;down
pacmov2 ld (pacdatkey),a
pacmov3 ld a,(pacdatkey)
        ld c,a                  ;c=new dir
        ld de,pacdatdir
        ld a,(de)               ;a=old dir
        cp c
        jr z,pacmov5
        ld b,c                  ;b=new dir
        res 0,a                             ;*** user wants another direction
        res 0,b
        cp b                    ;compare axis of old/new
        jr z,pacmov4            ;x or y stays -> ok
        ld hl,(pacdatxfn)
        ld a,l
        or h
        jr nz,pacmov5           ;old=x/y, new=y/x and pacman is not in the middle of a field -> no change
        ld a,c
        ld hl,(pacdatmap)
        call fldwal
        jr nc,pacmov5
pacmov4 ld a,c
        ld (de),a
pacmov5 ld hl,(pacdatxfn)                   ;*** test, if pacman is in the middle of a field and so if its allowed to move on
        ld a,h
        or l
        jr nz,pacmov6
        ld a,(pacdatdir)
        ld hl,(pacdatmap)
        call fldwal0
        ret nc
        ld a,(hl)                           ;*** no wall -> test, if there is a point or a pill
        cp 1
        jr c,pacmov6
        ld c,10
        ld de,256*2+SND_EATPOINT
        jr z,pacmov7
        push hl
        call ghoblu         ;pill -> make all ghosts "blue"
        ld de,256*1+SND_EATFRUIT
        pop hl
        ld c,50
pacmov7 
        push bc
        push hl
        ex de,hl
        call sndefx
        pop hl
        pop bc

        call pacpoi
pacmov6 ld a,(gamdatfru)                    ;*** test, if fruit is there and if pacman catched it
        cp 2
        jr c,pacmov8
        ld hl,(pacdatxps)
        ld bc,11*256+9
        or a
        sbc hl,bc
        jr nz,pacmov8
        ld a,(gamdatlev)
        cp 5
        jr c,pacmov9
        ld a,4
pacmov9 add a
        ld l,a
        ld h,0
        ld bc,gamdatfrs-2
        add hl,bc
        ld c,(hl)
        inc hl
        ld b,(hl)
        call pacpoi3

        ld hl,256*1+SND_EATFRUIT
        call sndefx

        ld a,1
        ld (gamdatfru),a
        ld (gamdatfld),a
        ;..............score sequence animation
pacmov8 ld ix,pacdatxps
        ld bc,256*2+256-2
        jp ghomov5
pacmov1 inc a                               ;*** test key (sub routine)
        push af
        ld hl,jmp_keytst:rst #28
        pop af
        dec e
        ret

;### PACPOI -> Pacman eats a point or a pill
;### Input  HL=map pointer, C=score
pacpoi  ld b,0                  ;*** remove point in map
        ld (hl),b
        call pacpoi3            ;*** count point + score
        ld hl,gamdatpoi
        dec (hl)
        jr nz,pacpoi4
        ld a,2
        ld (gamdatsta),a
pacpoi4 ld a,(pacdatdir)        ;*** remove point on screen
        ld hl,(pacdatxps)
        inc l
        sub 1
        jr z,pacpoi1
        dec l:dec l
        jr c,pacpoi1
        inc l:dec h
        dec a
        jr z,pacpoi1
        inc h:inc h     ;l=new x, h=new y
pacpoi1 ld a,l                  ;*** calculate screen x/y position
        add a
        ld b,a
        add a:add a
        inc a
        ld (prgwinobj5+6),a
        ld a,h
        add a:add a:add a
        inc a
        ld (prgwinobj5+8),a
        dec a                   ;*** calculate address in bitmap
        ld de,fldxln*2
        call clcm16
        ld e,b
        ld d,0
        add hl,de
        ld de,fldgfx1+1
        add hl,de
        ld (sprfield+3),hl
        ex de,hl                ;*** remove point in bitmap
        ld bc,fldxln*9*8*2+18
        add hl,bc
        ld a,8
pacpoi2 ldi
        ldi
        ld bc,fldxln*2-2
        add hl,bc
        ex de,hl
        add hl,bc
        ex de,hl
        dec a
        jr nz,pacpoi2
        inc a
        ld (gamdatfld),a
        ret
pacpoi3 ld hl,(gamdatsco+0)     ;*** increase score by BC
        add hl,bc
        ld (gamdatsco+0),hl
        ld hl,(gamdatsco+2)
        ld de,0
        adc hl,de
        ld (gamdatsco+2),hl
        ld hl,(gamdatlsc)       ;test, if new life available
        or a
        sbc hl,bc
        jr nc,pacpoi5
        push hl
        ld hl,gamdatliv
        inc (hl)

        ld hl,256*1+SND_NEWLIFE
        call sndefx

        call gamliv
        pop hl
        ld bc,20000
        add hl,bc
pacpoi5 ld (gamdatlsc),hl
        ret

;### PACDIE -> Displays dying Pacman animation
pacdie  ld l,-1
        call sndmus
        ld hl,256*1+SND_DEATH
        call sndefx

        ld b,5
pacdie1 push bc
        ld hl,24*10*3+sprdatpac+0
        call pacdie2
        ld hl,24*10*3+sprdatpac+12
        call pacdie2
        ld hl,24*10*3+sprdatpac+6
        call pacdie2
        ld hl,24*10*3+sprdatpac+18
        call pacdie2
        pop bc
        djnz pacdie1
        ret
pacdie2 ld (sprpacman+3),hl
        ld e,28
        call gamsta1
        ld b,5
pacdie3 rst #30
        djnz pacdie3
        ret

;### PACSHW -> Displays Pacman
pacshw  ld hl,(pacdatxps)       ;*** calculate new position
        ld de,(pacdatxfn)
        ld a,l
        add a:add a:add a
        add e
        dec a
        ld c,a
        ld (prgwinobj0+16+6),a
        ld a,h
        add a:add a:add a
        add d
        ld b,a
        ld (prgwinobj0+16+8),a
        ld ix,pacdatxps         ;*** remove pacman from old position
        db #fd:ld l,10
        call ghodif
        ld a,l:ld (prgwinobj0+6),a
        ld a,h:ld (prgwinobj0+8),a
        ld a,e:ld (prgwinobj0+10),a
        ld a,d:ld (prgwinobj0+12),a
        ld a,(gamdatcnt)        ;*** choose pacman sprite
        and #0e
        rra
        cp 4
        jr c,pacshw1
        cpl
        add 8
pacshw1 or a
        ld hl,sprdatpac
        jr z,pacshw3
        ld b,a
        ld de,24*10
pacshw2 add hl,de
        djnz pacshw2
pacshw3 ld a,(pacdatdir)
        add a
        ld c,a
        add a
        add c
        ld c,a
        ld b,0
        add hl,bc
        ld (sprpacman+3),hl
        ret


;==============================================================================
;### GHOST ROUTINES ###########################################################
;==============================================================================

ghodatmem
db  8,9,0,0,0,0,2,0:dw 0,sprghost1+2,prgwinobj1+016+6,sprremov1+1:db 0,80,120
db  9,9,0,0,0,0,2,0:dw 0,sprghost2+2,prgwinobj1+048+6,sprremov2+1:db 1,70,110
db 10,9,0,0,0,0,2,0:dw 0,sprghost3+2,prgwinobj1+080+6,sprremov3+1:db 2,70,110
db  9,8,0,0,0,0,2,0:dw 0,sprghost4+2,prgwinobj1+112+6,sprremov4+1:db 3,64,100

ghodatxps   equ 0   ;x position (0 -> fldxln-1)
ghodatyps   equ 1   ;y position (0 -> fldyln-1)
ghodatxfn   equ 2   ;x fine (-4 -> +3)
ghodatyfn   equ 3   ;y fine (-4 -> +3)
ghodatxsc   equ 4   ;screen x position
ghodatysc   equ 5   ;screen y position
ghodatdir   equ 6   ;direction (0=left, 1=right, 2=up, 3=down)
ghodatmod   equ 7   ;mode (0=normal, 1=blue or blue blinking, 2=eaten)
ghodatmap   equ 8   ;absolute map address
ghodatgfx   equ 10  ;address of the graphic pointer
ghodatobj   equ 12  ;address of the object pointer
ghodatrem   equ 14  ;address of the empty graphic pointer
ghodatcol   equ 16  ;colour (0-3)
ghodatrn1   equ 17
ghodatrn2   equ 18

ghodatlen   equ 19  ;length of one ghost data record

;### GHOINI -> initialised the four ghosts and sets them to their starting positions
ghoini  ld ix,ghodatmem
        ld hl,08*256+9
        call ghoini1
        ld hl,09*256+9
        call ghoini1
        ld hl,10*256+9
        call ghoini1
        ld hl,09*256+8
ghoini1 xor a
        ld (ix+ghodatxps),h
        ld (ix+ghodatyps),l
        ld (ix+ghodatxfn),a
        ld (ix+ghodatyfn),a
        ld (ix+ghodatdir),2
        ld (ix+ghodatmod),a
        call fldadr
        call ghoshw
        ld bc,ghodatlen
        add ix,bc
        ret

;### GHOSHW -> Displays one ghost
;### Input  IX=ghost data record
ghoshw  ld l,(ix+ghodatobj+0)   ;*** calculate new position
        ld h,(ix+ghodatobj+1)
        ld a,(ix+ghodatxps)
        add a:add a:add a
        add (ix+ghodatxfn)
        dec a
        ld c,a
        ld (hl),a
        inc hl:inc hl
        ld a,(ix+ghodatyps)
        add a:add a:add a
        add (ix+ghodatyfn)
        ld e,(ix+ghodatmod)
        bit 1,e             ;eaten ghost are only 4 pixels high, so the position is 3 pixel deeper
        db #fd:ld l,10
        ld b,a
        jr z,ghoshw1
        bit 0,e
        jr z,ghoshwa
        add 3
        ld (hl),a
        ld e,(ix+ghodatxsc)
        ld d,(ix+ghodatysc)
        ld (ix+ghodatxsc),c
        ld (ix+ghodatysc),a
        res 0,(ix+ghodatmod)
        push de
        ld de,10*256+12
        jr ghoshwb
ghoshwa db #fd:ld l,4
        add 3
        ld b,a
ghoshw1 ld (hl),a
        push hl                 ;*** remove ghost from old position
        call ghodif         ;L=remove X, H=remove Y, E=Xlen, D=Ylen
        ex (sp),hl
ghoshwb ld bc,-18
        add hl,bc
        pop bc
        dec c
        ld a,c
        and #fc
        ld (hl),a
        push af
        inc (hl)
        inc hl:inc hl
        sub c
        neg
        add e
        add 3
        and #fc
        cp 16+1
        jr c,ghoshw9
        ld a,16
ghoshw9 ld c,a
        ld a,b
        ld b,d              ;c=xlen, b=ylen
        ld (hl),a
        dec a
        ld de,fldxln*2
        call clcm16
        pop af
        rra:rra
        ld e,a
        ld d,0
        add hl,de
        ld de,fldgfx1+1
        add hl,de
        ex de,hl
        ld l,(ix+ghodatrem+0)
        ld h,(ix+ghodatrem+1)
        ld (hl),c
        inc hl
        ld (hl),b
        inc hl
        ld (hl),e
        inc hl
        ld (hl),d
        ld a,(ix+ghodatmod)     ;*** choose ghost sprite
        cp 1
        jr z,ghoshw5
        ld hl,sprdatgct     ;* eaten ghost
        ld c,4
        jr nc,ghoshw4
        ld b,(ix+ghodatcol) ;* normal ghost
        ld hl,sprdatgh1
        ld de,20*24
        inc b
ghoshw2 add hl,de           ;colour select
        djnz ghoshw2
        ld a,(gamdatcnt)
        and 4
        jr z,ghoshw3
        ld de,10*24
ghoshw3 sbc hl,de           ;animation select
        ld c,10
ghoshw4 ld a,(ix+ghodatdir)
        add a
ghoshw7 ld e,a
        add a
        add e
        ld e,a
        ld d,0
        add hl,de           ;direction select
        ex de,hl
        jr ghoshw6
ghoshw5 ld a,(gamdatblu)    ;* blue ghost
        cp 64
        ld hl,sprdatges  
        ld a,(gamdatcnt)
        jr nc,ghoshw8
        bit 3,a
        jr z,ghoshw8
        ld hl,sprdatges+12
ghoshw8 and 4
        rr a
        ld c,10
        jr ghoshw7
ghoshw6 ld l,(ix+ghodatgfx+0)   ;*** set choosen sprite
        ld h,(ix+ghodatgfx+1)
        ld (hl),c
        inc hl
        ld (hl),e
        inc hl
        ld (hl),d
        ret

;### GHODIF -> Calculates the differece area between old and new ghost position
;### Input      IX=dataset, C=new X, B=new Y, IYL=Ylen
;### Output     L=remove X, H=remove Y, E=Xlen, D=Ylen, ZF=1 -> no change
ghodif  push iy
        db #fd:ld h,0
        ld a,(ix+ghodatysc)
        ld (ghodif5+1),a
        ld l,a
        sub b
        ld (ix+ghodatysc),b
        call ghodif1
        ld h,a
        ld d,e
        db #fd:ld l,12
        ld a,(ix+ghodatxsc)
        ld l,a
        sub c
        ld (ix+ghodatxsc),c
        ld b,c
        call ghodif1
        pop bc
        db #fd:dec h:db #fd:inc h
        jr z,ghodif5
        ld l,a
        db #fd:dec h:db #fd:dec h
        ret nz
        ld de,256*1+1
        ret
ghodif5 ld h,0
        ld d,c
        ld e,12
        ret

;A=old-new, L=old pos, B=new pos, IYL=len -> A=pos, E=len, no change -> H=H+1
ghodif1 db #fd:ld e,l
        jr c,ghodif4
        jr nz,ghodif3
        db #fd:inc h
ghodif2 ld a,l
        ret
ghodif3 cp e
        jr nc,ghodif2
        ld e,a
        ld a,b
        db #fd:add l
        ret
ghodif4 neg
        cp e
        jr nc,ghodif2
        ld e,a
        ld a,l
        ret

;### GHOMOV -> Updates the position of one ghost
;### Input  IX=ghost data record
ghomov  ld a,(ix+ghodatxfn)
        or a
        jp nz,ghomov4
        ld a,(ix+ghodatyfn)
        or a
        jp nz,ghomov4
        ld a,(ix+ghodatdir)     ;*** ghost is in the middle of one field -> check, if it could change its moving direction
        ld bc,-fldxln
        ld de,fldxln*2
        ld hl,0-1-fldxln
        cp 1
        jr c,ghomov1
        ld hl,0+1-fldxln
        jr z,ghomov1
        ld bc,-1
        ld de,1*2
        cp 3
        jr z,ghomov1
        ld hl,0-1-fldxln
ghomov1 ld a,(ix+ghodatmap+0)   ;*** test, if surounding fields are walls or not
        db #fd:ld l,a
        ld a,(ix+ghodatmap+1)
        db #fd:ld h,a
        add iy,bc
        ld a,(iy+0)
        cp 3
        jr c,ghomov2                ;at -90 degree no wall -> lets maybe change the direction
        add iy,de
        ld a,(iy+0)
        cp 3
        jr c,ghomov2                ;at +90 degree no wall -> lets maybe change the direction
        ex de,hl
        add iy,de
        ld a,(iy+0)
        cp 3
        jr c,ghomov4                ;in front no wall -> no change
ghomov2 call ghodir             ;*** ghost may change its direction
        ld a,(ix+ghodatdir)
        ld l,a
        xor 1
        ld b,a      ;b=last try (ghost shouldn't walk backward)
        ld a,(gamdatmod)        ;*** ghost may change its direction
        or a
        jr nz,ghomovl
        ld a,r      ;bad luck, that z is destroyed by ld a,r
        jr ghomovk
ghomovl ld a,r
        and 127
        bit 1,(ix+ghodatmod)
        jr z,ghomovj
        cp 32       ;only a small amount of randomness for ghost finding their way back
        jr nc,ghomovf
        jr ghomovk
ghomovj cp (ix+ghodatrn1)
        jr c,ghomovf
        ld c,e
        ld e,d
        ld d,c
        cp (ix+ghodatrn1)
        jr c,ghomovf
ghomovk and 3
        ld e,a
        xor 2
        ld d,a      ;e=first try, d=second try
ghomovf ld a,e      ;take care, that e,d<>b
        cp b
        jr nz,ghomovg
        ld e,l
        jr ghomovh
ghomovg ld a,d
        cp b
        jr nz,ghomovh
        ld d,l
ghomovh ld a,b      ;find third try
        and 2
        ld l,a
        ld a,e          ;test, if b and e are on the same axis
        and 2
        cp l
        ld a,d
        jr z,ghomovi    ;yes -> use the opposite direction of d
        ld a,e          ;no  -> use the opposite direction of e
ghomovi xor 1
        ld c,a      ;c=third try

        ld l,(ix+ghodatmap+0)   ;*** test, which possible new directions can be used
        ld h,(ix+ghodatmap+1)
        ld a,e
        call fldwal
        jr c,ghomov3
        ld a,d
        call fldwal
        jr c,ghomov3
        ld a,c
        call fldwal
        jr c,ghomov3
        ld a,b
ghomov3 ld (ix+ghodatdir),a
ghomov4 call ghocol             ;*** collision detection 1 (before movement)
        ld a,(ix+ghodatmod)     ;*** check, if eaten ghost can be restored
        cp 2
        jr c,ghomovc
        ld hl,256*9+9
        or a
        sbc hl,bc
        jr nz,ghomovc
        xor a
        ld (ix+ghodatmod),a
ghomovc cp 1                    ;*** move ghost in its current direction
        ld bc,256+255
        jr z,ghomovm
        inc b
        dec c
ghomovm call ghomov5
        jp ghocol               ;*** collision detection 2 (after movement)
ghomov5 ld a,(ix+ghodatdir)         ;ix=dataset, c=neg speed, b=pos speed
        push ix
        pop iy
        ld d,fldxln-1
        cp 1
        ld e,c
        jr c,ghomov6
        ld e,b
        jr z,ghomov6
        inc iy
        ld d,fldyln-1
        cp 3
        jr z,ghomov6
        ld e,c                      ;E=dif, D=max, IY=component
ghomov6 ld a,(iy+2)
        add e
        jp m,ghomov8
        cp 4                        ;fine pos is positive
        jr c,ghomova
        ld a,(iy+0)
        inc a
        cp d
        jr nz,ghomov7
        ld a,1
ghomov7 ld (iy+0),a
        call fldadr
        ld a,-4
        jr ghomova
ghomov8 cp -4                       ;fine pos is negative
        jr nc,ghomova
        ld a,(iy+0)
        dec a
        jr nz,ghomov9
        ld a,d
        dec a
ghomov9 ld (iy+0),a
        ld c,e
        call fldadr
        ld a,4
        add c
ghomova ld (iy+2),a
        ret

ghomovb inc a
        and 3
        cp b
        jr z,ghomovb
        ret

;### GHODIR -> calculate direction suggestion
;### Output E=Prio1 direction, D=Prio2 direction
ghodir  ld a,9
        cp (ix+ghodatxps)
        jr nz,ghodir5
        cp (ix+ghodatyps)
        jr nz,ghodir5
        ld a,r
        and 3
        ld e,a
        xor 2
        ld d,a
        ret
ghodir5 ld a,(ix+ghodatmod)
        ld b,a
        cp 2
        ld hl,(pacdatxps)
        jr c,ghodir1
        ld hl,256*9+9       ;HL=x/y position of reference point
ghodir1 ld de,#301          ;calculate directions to reach point
        ld a,l
        sub (ix+ghodatxps)
        jr nc,ghodir2
        neg
        ld e,0
ghodir2 ld c,a
        ld a,h
        sub (ix+ghodatyps)
        jr nc,ghodir3
        neg
        ld d,2
ghodir3 cp c
        jr c,ghodir4
        ld a,d              ;put direction to the most far distance to the front
        ld d,e
        ld e,a
ghodir4 bit 0,b
        ret z
        ld a,d              ;ghost escapes -> turn everything around
        ld d,e
        xor 1
        ld e,a
        ld a,d
        xor 1
        ld d,a
        ret

;### GHOBLU -> make ghosts blue
ghoblu  ld a,(gamdatlev)
        neg
        add 9
        jr c,ghoblu3
        xor a
ghoblu3 add a:add a:add a:add a
        add 64
        ld (gamdatblu),a
        xor a
        ld (gamdateat),a
        ld de,1
ghoblu0 ld hl,ghodatmem+ghodatmod
        ld bc,ghodatlen
        db #fd:ld l,4
ghoblu1 ld a,(hl)
        cp d
        jr nz,ghoblu2
        ld (hl),e
ghoblu2 add hl,bc
        db #fd:dec l
        jr nz,ghoblu1
        ret

;### GHONRM -> make blue ghosts normal
ghonrm  ld de,256*1+0
        jr ghoblu0

;### GHOCOL -> Ghost hits Pacman
;### Input  IX=ghost data record
ghocol  ld a,(gamdatmod)        ;*** collision detection
        or a
        ret z           ;demo mode -> no collisions
        ld c,(ix+ghodatxps)
        ld b,(ix+ghodatyps)
        ld hl,(pacdatxps)
        or a
        sbc hl,bc
        push bc
        call z,ghocol0
        pop bc
        ret
ghocol0 ld a,(ix+ghodatmod)
        cp 1
        jr c,ghocol2
        ret nz                  ;*** eyes -> nothing happens

        push ix
        ld hl,256*1+SND_EATGHOST
        call sndefx
        pop ix

        ld (ix+ghodatmod),3     ;*** blue -> pacman eats ghost
        ld hl,gamdateat
        inc (hl)
        ld b,(hl)
        ld hl,100
ghocol1 add hl,hl
        djnz ghocol1
        ld c,l
        ld b,h
        call pacpoi3
        ld a,1
        ld (gamdatfld),a
        ;..............score sequence animation
        ret
ghocol2 ld a,1                  ;*** normal -> pacman dies
        ld (gamdatsta),a
        ret


;==============================================================================
;### GAME FIELD ROUTINES ######################################################
;==============================================================================

;### FLDMAK -> Generates the field graphic
;### Input  (fldnum)=map number
fldmak  ld a,(configmul)
        or a
        ld hl,fldmap1
        jr z,fldmak4
        ld hl,(fldnum)
        add hl,hl
        ld bc,fldtab
        add hl,bc
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
fldmak4 rst #30
        ld a,(hl)
        inc hl
        ld (gamdatpoi),a
        ld de,fldbuf
        push de
        pop ix
        ld bc,fldxln*fldyln
        ldir
        ld hl,fldgfx1
        ld (hl),0
        inc hl
        db #fd:ld l,fldyln
fldmak1 push hl
        db #fd:ld h,fldxln
fldmak2 push hl
        ld a,(ix+0)
        inc ix
        add a:add a:add a:add a
        ld c,a
        ld a,0
        adc 0
        ld b,a
        ex de,hl
        ld hl,fldspr
        add hl,bc
        ld a,8
fldmak3 ldi:ldi
        ld bc,fldxln*2-2
        ex de,hl
        add hl,bc
        ex de,hl
        dec a
        jr nz,fldmak3
        pop hl
        inc hl:inc hl
        db #fd:dec h
        jr nz,fldmak2
        pop hl
        ld bc,fldxln*2*8
        add hl,bc
        db #fd:dec l
        jr nz,fldmak1
        ret

;### FLDWAL -> tests, if there is a wall in the new direction
;### Input      HL=map pointer, A=direction
;### Output     CF=1 next field is no wall
;### Destroyed  F
fldwal  push hl
        push bc
        call fldwal0
        pop bc
        pop hl
        ret
fldwal0 cp 1
        ld bc,-1
        jr c,fldwal1
        ld bc,1
        jr z,fldwal1
        cp 3
        ld bc,-fldxln
        jr c,fldwal1
        ld bc,fldxln
fldwal1 add hl,bc
        ld b,a
        ld a,(hl)
        cp 3
        ld a,b
        ret

;### FLDADR -> calculates the absolute address inside the map
;### Input      IX=pacman or ghost data record
;### Destroyed  AF,DE,HL
fldadr  ld a,(ix+ghodatyps)
        ld de,fldxln
        call clcm16
        ld e,(ix+ghodatxps)
        ld d,0
        add hl,de
        ld de,fldbuf
        add hl,de
        ld (ix+ghodatmap+0),l
        ld (ix+ghodatmap+1),h
        ret


;==============================================================================
;### SUB-ROUTINES #############################################################
;==============================================================================

SyKernel_MTADDT
        ld c,MSC_KRL_MTADDT
        call SyKernel_Message
        xor a
        cp l
        ld a,h
        ret

SyKernel_Message
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),l
        ld (iy+2),h
        ld (iy+3),e
        ld (iy+4),a
        ld (iy+5),b
        ld a,c
        add 128
        ld (SyKMsgN),a
        db #dd:ld h,1       ;1 is the number of the kernel process
        ld a,(prgprzn)
        db #dd:ld l,a
        rst #10
SyKMsg1 db #dd:ld h,1       ;1 is the number of the kernel process
        ld a,(prgprzn)
        db #dd:ld l,a
        rst #08             ;wait for a kernel message
        db #dd:dec l
        jr nz,SyKMsg1
        ld a,(SyKMsgN)
        cp (iy+0)
        jr nz,SyKMsg1
        ld l,(iy+1)
        ld h,(iy+2)
        ret
SyKMsgN db 0

;### MSGGET -> Message für Programm abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert 
msgget  ld a,(prgprzn)
        db #dd:ld l,a           ;IXL=Rechner-Prozeß-Nummer
        db #dd:ld h,-1
        ld iy,prgmsgb           ;IY=Messagebuffer
        rst #18                 ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        or a
        db #dd:dec l
        ret nz
        ld iy,prgmsgb
        ld a,(iy+0)
        or a
        jp z,prgend
        scf
        ret

;### MSGDSK -> Message für Programm von Desktop-Prozess abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert 
msgdsk  call msgget
        jr nc,msgdsk            ;keine Message
        ld a,(dskprzn)
        db #dd:cp h
        jr nz,msgdsk            ;Message von anderem als Desktop-Prozeß -> ignorieren
        ld a,(prgmsgb)
        ret

;### MSGSND -> Message an Desktop-Prozess senden
;### Eingabe    C=Kommando, B/E/D/L/H=Parameter1/2/3/4/5
msgsnd  ld a,(dskprzn)
msgsnd1 db #dd:ld h,a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),b
        ld (iy+2),e
        ld (iy+3),d
        ld (iy+4),l
        ld (iy+5),h
        rst #10
        ret

;### CLCN32 -> Converts 32bit number into a 10digit ASCII string
;### Eingabe    DE,IX=Wert, IY=Adresse
;### Veraendert AF,BC,DE,HL,IX,IY
clcn32t dw 1,0,     10,0,     100,0,     1000,0,     10000,0
        dw #86a0,1, #4240,#f, #9680,#98, #e100,#5f5, #ca00,#3b9a
clcn32z ds 4

clcn32  ld (clcn32z),ix
        ld (clcn32z+2),de
        ld ix,clcn32t+36
        ld b,9
clcn321 ld a,"0"
        or a
clcn322 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  sbc hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):sbc hl,de:ld (clcn32z+2),hl
        jr c,clcn325
        inc a
        jr clcn322
clcn325 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  add hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):adc hl,de:ld (clcn32z+2),hl
        ld de,-4
        add ix,de
        ld (iy+0),a
        inc iy
clcn323 djnz clcn321
        ld a,(clcn32z)
        add "0"
        ld (iy+0),a
        ret

;### CLCDEZ -> Rechnet Byte in zwei Dezimalziffern um
;### Eingabe    A=Wert
;### Ausgabe    L=10er-Ascii-Ziffer, H=1er-Ascii-Ziffer
;### Veraendert AF
clcdez  ld l,0
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ld a,"0"
        add l
        ld l,a
        ret

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
clcm161 or a
        ret z
        rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        jr clcm161

;### CLCUCS -> Wandelt Klein- in Großbuchstaben um
;### Eingabe    A=Zeichen
;### Ausgabe    A=ucase(Zeichen)
;### Verändert  F
clcucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret


;==============================================================================
;### GAME FIELD ###############################################################
;==============================================================================

fldxln equ 19
fldyln equ 21

fldanz equ 4
fldnum db 0,0
fldtab dw fldmap1,fldmap2,fldmap3,fldmap4

fldmap1 db 146+4    ;number of points + pills
db #03,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#04,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#05
db #0b,#01,#01,#01,#01,#01,#01,#01,#01,#0b,#01,#01,#01,#01,#01,#01,#01,#01,#0b
db #0b,#02,#0d,#0e,#01,#0d,#0c,#0e,#01,#10,#01,#0d,#0c,#0e,#01,#0d,#0e,#02,#0b
db #0b,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0b
db #0b,#01,#0d,#0e,#01,#0f,#01,#0d,#0c,#04,#0c,#0e,#01,#0f,#01,#0d,#0e,#01,#0b
db #0b,#01,#01,#01,#01,#0b,#01,#01,#01,#0b,#01,#01,#01,#0b,#01,#01,#01,#01,#0b
db #08,#0c,#0c,#05,#01,#06,#0c,#0e,#00,#10,#00,#0d,#0c,#07,#01,#03,#0c,#0c,#0a
db #00,#00,#00,#0b,#01,#0b,#00,#00,#00,#00,#00,#00,#00,#0b,#01,#0b,#00,#00,#00
db #0c,#0c,#0c,#0a,#01,#10,#00,#03,#0e,#00,#0d,#05,#00,#10,#01,#08,#0c,#0c,#0c
db #00,#00,#00,#00,#01,#00,#00,#0b,#00,#00,#00,#0b,#00,#00,#01,#00,#00,#00,#00
db #0c,#0c,#0c,#05,#01,#0f,#00,#08,#0c,#0c,#0c,#0a,#00,#0f,#01,#03,#0c,#0c,#0c
db #00,#00,#00,#0b,#01,#0b,#00,#00,#00,#00,#00,#00,#00,#0b,#01,#0b,#00,#00,#00
db #03,#0c,#0c,#0a,#01,#10,#00,#0d,#0c,#04,#0c,#0e,#00,#10,#01,#08,#0c,#0c,#05
db #0b,#01,#01,#01,#01,#01,#01,#01,#01,#0b,#01,#01,#01,#01,#01,#01,#01,#01,#0b
db #0b,#01,#0d,#05,#01,#0d,#0c,#0e,#01,#10,#01,#0d,#0c,#0e,#01,#03,#0e,#01,#0b
db #0b,#02,#01,#0b,#01,#01,#01,#01,#01,#00,#01,#01,#01,#01,#01,#0b,#01,#02,#0b
db #06,#0e,#01,#10,#01,#0f,#01,#0d,#0c,#04,#0c,#0e,#01,#0f,#01,#10,#01,#0d,#07
db #0b,#01,#01,#01,#01,#0b,#01,#01,#01,#0b,#01,#01,#01,#0b,#01,#01,#01,#01,#0b
db #0b,#01,#0d,#0c,#0c,#09,#0c,#0e,#01,#10,#01,#0d,#0c,#09,#0c,#0c,#0e,#01,#0b
db #0b,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0b
db #08,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0a

fldmap2 db 135+4
db #03,#0C,#0C,#0C,#0C,#04,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#04,#0C,#0C,#0C,#0C,#05
db #0B,#01,#01,#01,#01,#0B,#01,#01,#01,#01,#01,#01,#01,#0B,#01,#01,#01,#01,#0B
db #0B,#02,#0D,#0E,#01,#10,#01,#0D,#0C,#0C,#0C,#0E,#01,#10,#01,#0D,#0E,#02,#0B
db #0B,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0B
db #08,#0E,#01,#0F,#01,#0D,#0C,#0E,#01,#0F,#01,#0D,#0C,#0E,#01,#0F,#01,#0D,#0A
db #00,#00,#01,#0B,#01,#01,#01,#01,#01,#0B,#01,#01,#01,#01,#01,#0B,#01,#00,#00
db #0C,#05,#01,#08,#0C,#0E,#00,#0D,#0C,#09,#0C,#0E,#00,#0D,#0C,#0A,#01,#03,#0C
db #00,#0B,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#0B,#00
db #00,#0B,#01,#03,#0C,#0E,#00,#03,#0E,#00,#0D,#05,#00,#0D,#0C,#05,#01,#0B,#00
db #00,#0B,#01,#0B,#00,#00,#00,#0B,#00,#00,#00,#0B,#00,#00,#00,#0B,#01,#0B,#00
db #0C,#0A,#01,#10,#00,#0F,#00,#08,#0C,#0C,#0C,#0A,#00,#0F,#00,#10,#01,#08,#0C
db #00,#00,#01,#00,#00,#0B,#00,#00,#00,#00,#00,#00,#00,#0B,#00,#00,#01,#00,#00
db #0C,#05,#01,#0D,#0C,#09,#0C,#0E,#00,#0F,#00,#0D,#0C,#09,#0C,#0E,#01,#03,#0C
db #00,#0B,#01,#01,#01,#01,#01,#00,#00,#0B,#00,#00,#01,#01,#01,#01,#01,#0B,#00
db #03,#0A,#01,#0D,#0C,#0E,#01,#0D,#0C,#09,#0C,#0E,#01,#0D,#0C,#0E,#01,#08,#05
db #0B,#01,#01,#01,#01,#01,#01,#01,#01,#00,#01,#01,#01,#01,#01,#01,#01,#01,#0B
db #0B,#01,#03,#05,#01,#03,#0C,#0E,#01,#0F,#01,#0D,#0C,#05,#01,#03,#05,#01,#0B
db #0B,#01,#0B,#0B,#01,#0B,#01,#01,#01,#0B,#01,#01,#01,#0B,#01,#0B,#0B,#01,#0B
db #0B,#02,#08,#0A,#01,#10,#01,#0D,#0C,#09,#0C,#0E,#01,#10,#01,#08,#0A,#02,#0B
db #0B,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0B
db #08,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0A

fldmap3 db 147+4
db #0C,#0C,#0C,#0C,#0C,#04,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#04,#0C,#0C,#0C,#0C,#0C
db #00,#00,#00,#00,#00,#0B,#01,#01,#01,#01,#01,#01,#01,#0B,#00,#00,#00,#00,#00
db #03,#0C,#0C,#0E,#00,#10,#01,#0D,#0C,#04,#0C,#0E,#01,#10,#00,#0D,#0C,#0C,#05
db #0B,#02,#01,#01,#01,#01,#01,#01,#01,#0B,#01,#01,#01,#01,#01,#01,#01,#02,#0B
db #0B,#01,#03,#0C,#0C,#0E,#01,#0F,#01,#10,#01,#0F,#01,#0D,#0C,#0C,#05,#01,#0B
db #0B,#01,#0B,#01,#01,#01,#01,#0B,#01,#01,#01,#0B,#01,#01,#01,#01,#0B,#01,#0B
db #0B,#01,#10,#01,#0D,#05,#00,#08,#0C,#0C,#0C,#0A,#00,#03,#0E,#01,#10,#01,#0B
db #0B,#01,#01,#01,#01,#0B,#00,#00,#00,#00,#00,#00,#00,#0B,#01,#01,#01,#01,#0B
db #06,#0C,#0C,#0E,#01,#0B,#00,#03,#0E,#00,#0D,#05,#00,#0B,#01,#0D,#0C,#0C,#07
db #0B,#01,#01,#01,#01,#10,#00,#0B,#00,#00,#00,#0B,#00,#10,#01,#01,#01,#01,#0B
db #0B,#01,#0D,#05,#01,#00,#00,#08,#0C,#0C,#0C,#0A,#00,#00,#01,#03,#0E,#01,#0B
db #0B,#01,#01,#0B,#01,#0F,#00,#00,#00,#00,#00,#00,#00,#0F,#01,#0B,#01,#01,#0B
db #08,#05,#01,#10,#01,#08,#0E,#01,#03,#0C,#05,#01,#0D,#0A,#01,#10,#01,#03,#0A
db #00,#0B,#01,#01,#01,#01,#01,#01,#0B,#00,#0B,#01,#01,#01,#01,#01,#01,#0B,#00
db #0C,#0A,#01,#0D,#0C,#04,#0E,#01,#08,#0C,#0A,#01,#0D,#04,#0C,#0E,#01,#08,#0C
db #00,#00,#01,#01,#01,#0B,#01,#01,#00,#00,#00,#01,#01,#0B,#01,#01,#01,#00,#00
db #03,#0E,#01,#0F,#01,#10,#01,#0D,#0C,#04,#0C,#0E,#01,#10,#01,#0F,#01,#0D,#05
db #0B,#02,#01,#0B,#01,#01,#01,#01,#01,#0B,#01,#01,#01,#01,#01,#0B,#01,#02,#0B
db #0B,#01,#0D,#0A,#01,#0D,#0C,#0E,#01,#10,#01,#0D,#0C,#0E,#01,#08,#0E,#01,#0B
db #0B,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0B
db #08,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0C,#0A

fldmap4 db 159+6
db #03,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0e,#00,#0d,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#05
db #0b,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0b
db #0b,#01,#0f,#02,#0d,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#05,#01,#0b
db #10,#01,#0b,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0b,#01,#10
db #00,#01,#06,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0e,#01,#0b,#01,#00
db #0f,#01,#0b,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#0b,#01,#0f
db #0b,#01,#10,#02,#0d,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0a,#01,#0b
db #0b,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#0b
db #0b,#01,#0f,#01,#0d,#05,#00,#03,#0e,#00,#0d,#05,#00,#03,#0e,#01,#0f,#01,#0b
db #0b,#01,#0b,#01,#01,#0b,#00,#0b,#00,#00,#00,#0b,#00,#0b,#01,#01,#0b,#01,#0b
db #10,#01,#10,#01,#0d,#0a,#00,#08,#0c,#0c,#0c,#0a,#00,#08,#0e,#01,#10,#01,#10
db #00,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#00
db #0f,#01,#0f,#02,#03,#0c,#0c,#0c,#05,#01,#03,#0c,#0c,#0c,#05,#02,#0f,#01,#0f
db #0b,#01,#0b,#01,#0b,#01,#01,#01,#0b,#01,#0b,#01,#01,#01,#0b,#01,#0b,#01,#0b
db #0b,#01,#0b,#01,#0b,#01,#0f,#01,#0b,#01,#0b,#01,#0f,#01,#0b,#01,#0b,#01,#0b
db #0b,#01,#0b,#01,#0b,#01,#0b,#01,#0b,#00,#0b,#01,#0b,#01,#0b,#01,#0b,#01,#0b
db #0b,#01,#0b,#01,#10,#01,#0b,#01,#0b,#01,#0b,#01,#0b,#01,#10,#01,#0b,#01,#0b
db #0b,#01,#0b,#01,#01,#01,#0b,#01,#0b,#01,#0b,#01,#0b,#01,#01,#01,#0b,#01,#0b
db #10,#01,#08,#0c,#0c,#0c,#0a,#02,#10,#01,#10,#02,#08,#0c,#0c,#0c,#0a,#01,#10
db #00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00
db #0d,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0e,#00,#0d,#0c,#0c,#0c,#0c,#0c,#0c,#0c,#0e


fldspr  ;field elements
db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0  ;0 empty
db #F0,#F0,#F0,#F0,#F0,#F0,#E0,#70,#E0,#70,#F0,#F0,#F0,#F0,#F0,#F0  ;1 point
db #F0,#F0,#E0,#70,#C0,#30,#80,#10,#80,#10,#C0,#30,#E0,#70,#F0,#F0  ;2 pill
db #F0,#F0,#F0,#F0,#F0,#0F,#E1,#FF,#D3,#FF,#D3,#CF,#D3,#BC,#D3,#BC  ;3 upper left
db #F0,#F0,#F0,#F0,#0F,#0F,#FF,#FF,#FF,#FF,#3F,#CF,#D3,#BC,#D3,#BC  ;4 upper middle
db #F0,#F0,#F0,#F0,#0F,#F0,#FF,#78,#FF,#BC,#3F,#BC,#D3,#BC,#D3,#BC  ;5 upper right
db #D3,#BC,#D3,#BC,#D3,#CF,#D3,#FF,#D3,#FF,#D3,#CF,#D3,#BC,#D3,#BC  ;6 left middle
db #D3,#BC,#D3,#BC,#3F,#BC,#FF,#BC,#FF,#BC,#3F,#BC,#D3,#BC,#D3,#BC  ;7 right middle
db #D3,#BC,#D3,#BC,#D3,#CF,#D3,#FF,#E1,#FF,#F0,#0F,#F0,#F0,#F0,#F0  ;8 down left
db #D3,#BC,#D3,#BC,#3F,#CF,#FF,#FF,#FF,#FF,#0F,#0F,#F0,#F0,#F0,#F0  ;9 down middle
db #D3,#BC,#D3,#BC,#3F,#BC,#FF,#BC,#FF,#78,#0F,#F0,#F0,#F0,#F0,#F0  ;a down right
db #D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC  ;b vert
db #F0,#F0,#F0,#F0,#0F,#0F,#FF,#FF,#FF,#FF,#0F,#0F,#F0,#F0,#F0,#F0  ;c hori
db #F0,#F0,#F0,#F0,#E1,#0F,#D3,#FF,#D3,#FF,#E1,#0F,#F0,#F0,#F0,#F0  ;d end left
db #F0,#F0,#F0,#F0,#0F,#78,#FF,#BC,#FF,#BC,#0F,#78,#F0,#F0,#F0,#F0  ;e end right
db #F0,#F0,#F0,#F0,#E1,#78,#D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC  ;f end up
db #D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC,#D3,#BC,#E1,#78,#F0,#F0,#F0,#F0  ;g end down

fldbuf  db 0    ;buffer for temporary field
;!!!last label!!!

;==============================================================================
;### D A T A   A R E A ########################################################
;==============================================================================

App_BegData

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #11,#11,#11,#11,#11,#99,#99,#11,#11,#11,#11,#11,#11,#11,#11,#19,#99,#9C,#CC,#99,#91,#11,#11,#11,#11,#11,#19,#9C,#CC,#CC,#CC,#CC,#C9,#91,#11,#11,#11,#11,#9C,#CC,#CC,#CC,#CC,#CC,#CC,#C9,#11,#11
db #11,#19,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#91,#11,#11,#9C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#C9,#11,#11,#9C,#CC,#33,#3C,#CC,#CC,#CC,#CC,#CC,#C9,#91,#19,#CC,#CC,#33,#3C,#CC,#CC,#CC,#CC,#99,#91,#11
db #19,#CC,#CC,#33,#3C,#CC,#CC,#C9,#99,#11,#11,#11,#19,#CC,#CC,#CC,#CC,#CC,#99,#91,#11,#11,#11,#11,#9C,#CC,#CC,#CC,#99,#99,#11,#11,#11,#11,#11,#11,#9C,#CC,#CC,#CC,#91,#11,#1A,#A1,#1A,#A1,#1A,#A1
db #9C,#CC,#CC,#CC,#91,#11,#1A,#A1,#1A,#A1,#1A,#A1,#99,#CC,#CC,#CC,#99,#99,#11,#11,#11,#11,#11,#11,#19,#CC,#CC,#CC,#CC,#CC,#99,#91,#11,#11,#11,#11,#19,#CC,#CC,#CC,#CC,#CC,#CC,#C9,#99,#11,#11,#11
db #19,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#99,#91,#11,#11,#9C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#C9,#91,#11,#9C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#C9,#11,#11,#19,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#91,#11
db #11,#11,#9C,#CC,#CC,#CC,#CC,#CC,#CC,#C9,#11,#11,#11,#11,#19,#9C,#CC,#CC,#CC,#CC,#C9,#91,#11,#11,#11,#11,#11,#19,#99,#CC,#CC,#99,#91,#11,#11,#11,#11,#11,#11,#11,#11,#99,#99,#11,#11,#11,#11,#11

;### MISC
prgwintit   db "PacMan for SymbOS",0

prgtxtok    db "Ok",0
prgtxtcnc   db "Cancel",0
prgtxtyes   db "Yes",0
prgtxtno    db "No",0

txttitsco   db "SCORE",0
txttithig   db "HIGH SCORE",0
txttitliv   db "LIVES",0

txtbutrun   db "Start!",0
txtbutpau   db "Pause",0
txtbutcon   db "Continue!",0
txtbutopt   db "Options...",0

txtgamlev   db "LEVEL ##",0
txtgamrdy   db "GET READY!",0
txtgampau   db "PAUSE",0
txtgamovr   db "GAME OVER",0

txtnumsco   db "   0000000000",0
gamdatsco   ds 4    ;current score
txtnumhig   db "   0000000000",0
gamdathig   ds 4    ;high score

sprpacman   db 24,12,10:dw 24*20+sprdatpac,sprdat0,24*146

sprghost1   db 24,12,10:dw sprdatgh1,sprdat0,24*146
sprghost2   db 24,12,10:dw sprdatgh2,sprdat0,24*146
sprghost3   db 24,12,10:dw sprdatgh3,sprdat0,24*146
sprghost4   db 24,12,10:dw sprdatgh4,sprdat0,24*146

sprfield    db fldxln*2,08,08:dw fldgfx1+1,fldgfx1+0,fldxln*fldyln*8*2

sprremov1   db fldxln*2,16,10:dw fldgfx1+1,fldgfx1+0,fldxln*fldyln*8*2
sprremov2   db fldxln*2,16,10:dw fldgfx1+1,fldgfx1+0,fldxln*fldyln*8*2
sprremov3   db fldxln*2,16,10:dw fldgfx1+1,fldgfx1+0,fldxln*fldyln*8*2
sprremov4   db fldxln*2,16,10:dw fldgfx1+1,fldgfx1+0,fldxln*fldyln*8*2

sprremov5   db fldxln*2,7*8,8:dw 0,fldgfx1+0,fldxln*fldyln*8*2

sprbonus1   db 24,12,12:dw sprdatbon+00,sprdat0,24*146
sprbonus2   db 24,12,12:dw sprdatbon+06,sprdat0,24*146
sprbonus3   db 24,12,12:dw sprdatbon+12,sprdat0,24*146
sprbonus4   db 24,12,12:dw sprdatbon+18,sprdat0,24*146

sprlives    db 24,12,10:dw 24*20+sprdatpac,sprdat0,24*146

;### CONFIG
configtit   db "PacMan Options",0
configtxt1  db "Settings",0
configtxt2  db "Keyboard",0
configtxt3  db "Joystick",0
configtxt4  db "Sound",0
configtxt5  db "Multilevel",0
configtxt6  db "Slow down speed",0
configtxt7  db "About",0
configtxt8  db "Pac-Man 1.1 for SymbOS (build 070704pdt)",0
configtxt9  db "(c)oded 2006-2007 by Prodatron/SymbiosiS",0
configtxta  db "  based on the original game by",0
configtxtb  db "  Iwatani Toru, (c) 1980 by Namco",0

;### 16 COLOUR SPRITES
sprdat0     db 5
sprdatpac
;pac man -> left/right/up/down per line, close>open per column
db #11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11
db #11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11
db #11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11
db #1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#C1,#CC,#CC,#C1
db #1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1
db #1C,#CC,#CC,#C1,#CC,#C1,#1C,#CC,#1C,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1
db #1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#1C,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1
db #11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11
db #11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11
db #11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11
db #11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#C1,#1C,#11,#11,#11,#11,#CC,#CC,#11,#11
db #11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#1C,#C1,#1C,#C1,#11,#11,#1C,#CC,#CC,#C1,#11
db #11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#C1,#1C,#CC,#11,#11,#CC,#CC,#CC,#CC,#11
db #1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#1C,#CC,#C1,#1C,#CC,#C1,#CC,#CC,#C1
db #11,#11,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#11,#11,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1
db #11,#11,#1C,#C1,#CC,#C1,#1C,#CC,#1C,#C1,#11,#11,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1
db #1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#1C,#CC,#C1,#1C,#CC,#C1,#CC,#CC,#C1
db #11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#C1,#1C,#CC,#11
db #11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#1C,#C1,#1C,#C1,#11
db #11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#C1,#1C,#11,#11
db #11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#1C,#11,#11,#C1,#11,#11,#11,#CC,#CC,#11,#11
db #11,#CC,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#CC,#11,#11,#CC,#11,#11,#CC,#11,#11,#1C,#CC,#CC,#C1,#11
db #1C,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#C1,#11,#CC,#11,#11,#CC,#11,#11,#CC,#CC,#CC,#CC,#11
db #11,#11,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#11,#11,#1C,#CC,#C1,#1C,#CC,#C1,#1C,#CC,#C1,#CC,#CC,#C1
db #11,#11,#11,#CC,#CC,#C1,#1C,#CC,#CC,#11,#11,#11,#1C,#CC,#C1,#1C,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1
db #11,#11,#11,#C1,#CC,#C1,#1C,#CC,#1C,#11,#11,#11,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#C1,#1C,#CC,#C1
db #11,#11,#CC,#CC,#CC,#C1,#1C,#CC,#CC,#CC,#11,#11,#1C,#CC,#CC,#1C,#CC,#C1,#1C,#CC,#C1,#1C,#CC,#C1
db #1C,#CC,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#CC,#C1,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#11,#11,#CC,#11
db #11,#CC,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#CC,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#CC,#11,#11,#CC,#11
db #11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#1C,#11,#11,#C1,#11
db #11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#CC,#CC,#11,#11
db #11,#CC,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#CC,#11,#11,#C1,#11,#11,#1C,#11,#11,#1C,#CC,#CC,#C1,#11
db #11,#1C,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#C1,#11,#11,#CC,#11,#11,#CC,#11,#11,#CC,#CC,#CC,#CC,#11
db #11,#11,#1C,#CC,#CC,#C1,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#11,#11,#CC,#C1,#1C,#CC,#C1,#CC,#CC,#C1
db #11,#11,#11,#CC,#CC,#C1,#1C,#CC,#CC,#11,#11,#11,#1C,#CC,#C1,#1C,#CC,#C1,#1C,#CC,#CC,#CC,#CC,#C1
db #11,#11,#11,#C1,#CC,#C1,#1C,#CC,#1C,#11,#11,#11,#1C,#CC,#CC,#CC,#CC,#C1,#1C,#CC,#C1,#1C,#CC,#C1
db #11,#11,#1C,#CC,#CC,#C1,#1C,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#1C,#CC,#C1,#1C,#CC,#11,#11,#CC,#C1
db #11,#1C,#CC,#CC,#CC,#11,#11,#CC,#CC,#CC,#C1,#11,#11,#CC,#CC,#CC,#CC,#11,#11,#CC,#11,#11,#CC,#11
db #11,#CC,#CC,#CC,#C1,#11,#11,#1C,#CC,#CC,#CC,#11,#11,#1C,#CC,#CC,#C1,#11,#11,#C1,#11,#11,#1C,#11
db #11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#CC,#CC,#11,#11,#11,#11,#11,#11,#11,#11
;ghost 1 -> left/right/up/down per line, anim1/anim2 per column
sprdatgh1
db #11,#11,#44,#44,#11,#11,#11,#11,#44,#44,#11,#11,#11,#11,#44,#44,#11,#11,#11,#11,#44,#44,#11,#11
db #11,#14,#44,#44,#41,#11,#11,#14,#44,#44,#41,#11,#11,#14,#44,#44,#41,#11,#11,#14,#44,#44,#41,#11
db #11,#48,#84,#48,#84,#11,#11,#48,#84,#48,#84,#11,#11,#45,#54,#45,#54,#11,#11,#48,#84,#48,#84,#11
db #11,#55,#84,#55,#84,#11,#11,#48,#55,#48,#55,#11,#11,#45,#54,#45,#54,#11,#11,#48,#84,#48,#84,#11
db #14,#55,#84,#55,#84,#41,#14,#48,#55,#48,#55,#41,#14,#48,#84,#48,#84,#41,#14,#45,#54,#45,#54,#41
db #14,#48,#84,#48,#84,#41,#14,#48,#84,#48,#84,#41,#14,#48,#84,#48,#84,#41,#14,#45,#54,#45,#54,#41
db #14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41
db #14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41
db #14,#41,#44,#14,#41,#41,#14,#41,#44,#14,#41,#41,#14,#41,#44,#14,#41,#41,#14,#41,#44,#14,#41,#41
db #14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41
db #11,#11,#44,#44,#11,#11,#11,#11,#44,#44,#11,#11,#11,#11,#44,#44,#11,#11,#11,#11,#44,#44,#11,#11
db #11,#14,#44,#44,#41,#11,#11,#14,#44,#44,#41,#11,#11,#14,#44,#44,#41,#11,#11,#14,#44,#44,#41,#11
db #11,#48,#84,#48,#84,#11,#11,#48,#84,#48,#84,#11,#11,#45,#54,#45,#54,#11,#11,#48,#84,#48,#84,#11
db #11,#55,#84,#55,#84,#11,#11,#48,#55,#48,#55,#11,#11,#45,#54,#45,#54,#11,#11,#48,#84,#48,#84,#11
db #14,#55,#84,#55,#84,#41,#14,#48,#55,#48,#55,#41,#14,#48,#84,#48,#84,#41,#14,#45,#54,#45,#54,#41
db #14,#48,#84,#48,#84,#41,#14,#48,#84,#48,#84,#41,#14,#48,#84,#48,#84,#41,#14,#45,#54,#45,#54,#41
db #14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41
db #14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41,#14,#44,#44,#44,#44,#41
db #14,#14,#41,#44,#14,#41,#14,#14,#41,#44,#14,#41,#14,#14,#41,#44,#14,#41,#14,#14,#41,#44,#14,#41
db #14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41,#14,#11,#41
;ghost 2
sprdatgh2
db #11,#11,#22,#22,#11,#11,#11,#11,#22,#22,#11,#11,#11,#11,#22,#22,#11,#11,#11,#11,#22,#22,#11,#11
db #11,#12,#22,#22,#21,#11,#11,#12,#22,#22,#21,#11,#11,#12,#22,#22,#21,#11,#11,#12,#22,#22,#21,#11
db #11,#28,#82,#28,#82,#11,#11,#28,#82,#28,#82,#11,#11,#25,#52,#25,#52,#11,#11,#28,#82,#28,#82,#11
db #11,#55,#82,#55,#82,#11,#11,#28,#55,#28,#55,#11,#11,#25,#52,#25,#52,#11,#11,#28,#82,#28,#82,#11
db #12,#55,#82,#55,#82,#21,#12,#28,#55,#28,#55,#21,#12,#28,#82,#28,#82,#21,#12,#25,#52,#25,#52,#21
db #12,#28,#82,#28,#82,#21,#12,#28,#82,#28,#82,#21,#12,#28,#82,#28,#82,#21,#12,#25,#52,#25,#52,#21
db #12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21
db #12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21
db #12,#21,#22,#12,#21,#21,#12,#21,#22,#12,#21,#21,#12,#21,#22,#12,#21,#21,#12,#21,#22,#12,#21,#21
db #12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21
db #11,#11,#22,#22,#11,#11,#11,#11,#22,#22,#11,#11,#11,#11,#22,#22,#11,#11,#11,#11,#22,#22,#11,#11
db #11,#12,#22,#22,#21,#11,#11,#12,#22,#22,#21,#11,#11,#12,#22,#22,#21,#11,#11,#12,#22,#22,#21,#11
db #11,#28,#82,#28,#82,#11,#11,#28,#82,#28,#82,#11,#11,#25,#52,#25,#52,#11,#11,#28,#82,#28,#82,#11
db #11,#55,#82,#55,#82,#11,#11,#28,#55,#28,#55,#11,#11,#25,#52,#25,#52,#11,#11,#28,#82,#28,#82,#11
db #12,#55,#82,#55,#82,#21,#12,#28,#55,#28,#55,#21,#12,#28,#82,#28,#82,#21,#12,#25,#52,#25,#52,#21
db #12,#28,#82,#28,#82,#21,#12,#28,#82,#28,#82,#21,#12,#28,#82,#28,#82,#21,#12,#25,#52,#25,#52,#21
db #12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21
db #12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21,#12,#22,#22,#22,#22,#21
db #12,#12,#21,#22,#12,#21,#12,#12,#21,#22,#12,#21,#12,#12,#21,#22,#12,#21,#12,#12,#21,#22,#12,#21
db #12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21,#12,#11,#21
;ghost 3
sprdatgh3
db #11,#11,#EE,#EE,#11,#11,#11,#11,#EE,#EE,#11,#11,#11,#11,#EE,#EE,#11,#11,#11,#11,#EE,#EE,#11,#11
db #11,#1E,#EE,#EE,#E1,#11,#11,#1E,#EE,#EE,#E1,#11,#11,#1E,#EE,#EE,#E1,#11,#11,#1E,#EE,#EE,#E1,#11
db #11,#E8,#8E,#E8,#8E,#11,#11,#E8,#8E,#E8,#8E,#11,#11,#E5,#5E,#E5,#5E,#11,#11,#E8,#8E,#E8,#8E,#11
db #11,#55,#8E,#55,#8E,#11,#11,#E8,#55,#E8,#55,#11,#11,#E5,#5E,#E5,#5E,#11,#11,#E8,#8E,#E8,#8E,#11
db #1E,#55,#8E,#55,#8E,#E1,#1E,#E8,#55,#E8,#55,#E1,#1E,#E8,#8E,#E8,#8E,#E1,#1E,#E5,#5E,#E5,#5E,#E1
db #1E,#E8,#8E,#E8,#8E,#E1,#1E,#E8,#8E,#E8,#8E,#E1,#1E,#E8,#8E,#E8,#8E,#E1,#1E,#E5,#5E,#E5,#5E,#E1
db #1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1
db #1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1
db #1E,#E1,#EE,#1E,#E1,#E1,#1E,#E1,#EE,#1E,#E1,#E1,#1E,#E1,#EE,#1E,#E1,#E1,#1E,#E1,#EE,#1E,#E1,#E1
db #1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1
db #11,#11,#EE,#EE,#11,#11,#11,#11,#EE,#EE,#11,#11,#11,#11,#EE,#EE,#11,#11,#11,#11,#EE,#EE,#11,#11
db #11,#1E,#EE,#EE,#E1,#11,#11,#1E,#EE,#EE,#E1,#11,#11,#1E,#EE,#EE,#E1,#11,#11,#1E,#EE,#EE,#E1,#11
db #11,#E8,#8E,#E8,#8E,#11,#11,#E8,#8E,#E8,#8E,#11,#11,#E5,#5E,#E5,#5E,#11,#11,#E8,#8E,#E8,#8E,#11
db #11,#55,#8E,#55,#8E,#11,#11,#E8,#55,#E8,#55,#11,#11,#E5,#5E,#E5,#5E,#11,#11,#E8,#8E,#E8,#8E,#11
db #1E,#55,#8E,#55,#8E,#E1,#1E,#E8,#55,#E8,#55,#E1,#1E,#E8,#8E,#E8,#8E,#E1,#1E,#E5,#5E,#E5,#5E,#E1
db #1E,#E8,#8E,#E8,#8E,#E1,#1E,#E8,#8E,#E8,#8E,#E1,#1E,#E8,#8E,#E8,#8E,#E1,#1E,#E5,#5E,#E5,#5E,#E1
db #1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1
db #1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1,#1E,#EE,#EE,#EE,#EE,#E1
db #1E,#1E,#E1,#EE,#1E,#E1,#1E,#1E,#E1,#EE,#1E,#E1,#1E,#1E,#E1,#EE,#1E,#E1,#1E,#1E,#E1,#EE,#1E,#E1
db #1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1,#1E,#11,#E1
;ghost 4
sprdatgh4
db #11,#11,#AA,#AA,#11,#11,#11,#11,#AA,#AA,#11,#11,#11,#11,#AA,#AA,#11,#11,#11,#11,#AA,#AA,#11,#11
db #11,#1A,#AA,#AA,#A1,#11,#11,#1A,#AA,#AA,#A1,#11,#11,#1A,#AA,#AA,#A1,#11,#11,#1A,#AA,#AA,#A1,#11
db #11,#A8,#8A,#A8,#8A,#11,#11,#A8,#8A,#A8,#8A,#11,#11,#A5,#5A,#A5,#5A,#11,#11,#A8,#8A,#A8,#8A,#11
db #11,#55,#8A,#55,#8A,#11,#11,#A8,#55,#A8,#55,#11,#11,#A5,#5A,#A5,#5A,#11,#11,#A8,#8A,#A8,#8A,#11
db #1A,#55,#8A,#55,#8A,#A1,#1A,#A8,#55,#A8,#55,#A1,#1A,#A8,#8A,#A8,#8A,#A1,#1A,#A5,#5A,#A5,#5A,#A1
db #1A,#A8,#8A,#A8,#8A,#A1,#1A,#A8,#8A,#A8,#8A,#A1,#1A,#A8,#8A,#A8,#8A,#A1,#1A,#A5,#5A,#A5,#5A,#A1
db #1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1
db #1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1
db #1A,#A1,#AA,#1A,#A1,#A1,#1A,#A1,#AA,#1A,#A1,#A1,#1A,#A1,#AA,#1A,#A1,#A1,#1A,#A1,#AA,#1A,#A1,#A1
db #1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1
db #11,#11,#AA,#AA,#11,#11,#11,#11,#AA,#AA,#11,#11,#11,#11,#AA,#AA,#11,#11,#11,#11,#AA,#AA,#11,#11
db #11,#1A,#AA,#AA,#A1,#11,#11,#1A,#AA,#AA,#A1,#11,#11,#1A,#AA,#AA,#A1,#11,#11,#1A,#AA,#AA,#A1,#11
db #11,#A8,#8A,#A8,#8A,#11,#11,#A8,#8A,#A8,#8A,#11,#11,#A5,#5A,#A5,#5A,#11,#11,#A8,#8A,#A8,#8A,#11
db #11,#55,#8A,#55,#8A,#11,#11,#A8,#55,#A8,#55,#11,#11,#A5,#5A,#A5,#5A,#11,#11,#A8,#8A,#A8,#8A,#11
db #1A,#55,#8A,#55,#8A,#A1,#1A,#A8,#55,#A8,#55,#A1,#1A,#A8,#8A,#A8,#8A,#A1,#1A,#A5,#5A,#A5,#5A,#A1
db #1A,#A8,#8A,#A8,#8A,#A1,#1A,#A8,#8A,#A8,#8A,#A1,#1A,#A8,#8A,#A8,#8A,#A1,#1A,#A5,#5A,#A5,#5A,#A1
db #1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1
db #1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1,#1A,#AA,#AA,#AA,#AA,#A1
db #1A,#1A,#A1,#AA,#1A,#A1,#1A,#1A,#A1,#AA,#1A,#A1,#1A,#1A,#A1,#AA,#1A,#A1,#1A,#1A,#A1,#AA,#1A,#A1
db #1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1,#1A,#11,#A1
;escaping ghost -> blue anim1/anim2, white anim1/anim2
sprdatges
db #11,#11,#77,#77,#11,#11,#11,#11,#77,#77,#11,#11,#11,#11,#88,#88,#11,#11,#11,#11,#88,#88,#11,#11
db #11,#17,#77,#77,#71,#11,#11,#17,#77,#77,#71,#11,#11,#18,#88,#88,#81,#11,#11,#18,#88,#88,#81,#11
db #11,#77,#77,#77,#77,#11,#11,#77,#77,#77,#77,#11,#11,#88,#88,#88,#88,#11,#11,#88,#88,#88,#88,#11
db #11,#78,#87,#78,#87,#11,#11,#78,#87,#78,#87,#11,#11,#8F,#F8,#8F,#F8,#11,#11,#8F,#F8,#8F,#F8,#11
db #17,#78,#87,#78,#87,#71,#17,#78,#87,#78,#87,#71,#18,#8F,#F8,#8F,#F8,#81,#18,#8F,#F8,#8F,#F8,#81
db #17,#77,#77,#77,#77,#71,#17,#77,#77,#77,#77,#71,#18,#88,#88,#88,#88,#81,#18,#88,#88,#88,#88,#81
db #17,#78,#78,#78,#77,#71,#17,#78,#78,#78,#77,#71,#18,#8F,#8F,#8F,#88,#81,#18,#8F,#8F,#8F,#88,#81
db #17,#87,#87,#87,#87,#71,#17,#87,#87,#87,#87,#71,#18,#F8,#F8,#F8,#F8,#81,#18,#F8,#F8,#F8,#F8,#81
db #17,#71,#77,#17,#71,#71,#17,#17,#71,#77,#17,#71,#18,#81,#88,#18,#81,#81,#18,#18,#81,#88,#18,#81
db #17,#11,#71,#17,#11,#71,#17,#11,#71,#17,#11,#71,#18,#11,#81,#18,#11,#81,#18,#11,#81,#18,#11,#81
;eyes of catched ghost -> left/right/up/down
sprdatgct
db #11,#18,#81,#18,#81,#11,#11,#18,#81,#18,#81,#11,#11,#17,#71,#17,#71,#11,#11,#18,#81,#18,#81,#11
db #11,#77,#81,#77,#81,#11,#11,#18,#77,#18,#77,#11,#11,#17,#71,#17,#71,#11,#11,#18,#81,#18,#81,#11
db #11,#77,#81,#77,#81,#11,#11,#18,#77,#18,#77,#11,#11,#18,#81,#18,#81,#11,#11,#17,#71,#17,#71,#11
db #11,#18,#81,#18,#81,#11,#11,#18,#81,#18,#81,#11,#11,#18,#81,#18,#81,#11,#11,#17,#71,#17,#71,#11
;bonus fruits
sprdatbon
db #11,#11,#11,#11,#11,#AA,#11,#11,#11,#A1,#11,#11,#11,#11,#11,#11,#33,#11,#11,#11,#13,#31,#11,#11
db #11,#11,#11,#11,#AA,#AA,#11,#11,#AA,#1A,#A1,#11,#11,#11,#13,#13,#33,#31,#11,#11,#AA,#AA,#11,#11
db #11,#11,#11,#AA,#1A,#11,#11,#FF,#1A,#AA,#1F,#F1,#11,#11,#13,#31,#11,#11,#11,#22,#2A,#A2,#22,#11
db #11,#11,#1A,#11,#1A,#11,#1F,#FF,#F1,#A1,#FF,#FF,#11,#A1,#33,#1A,#AA,#11,#12,#22,#22,#22,#22,#21
db #1F,#FF,#F1,#11,#A1,#11,#1F,#9F,#FF,#1F,#FF,#9F,#1A,#AA,#11,#AA,#AA,#A1,#12,#22,#22,#22,#22,#21
db #FF,#F8,#FF,#1A,#11,#11,#1F,#FF,#9F,#FF,#9F,#FF,#AA,#AA,#AA,#AA,#AA,#AA,#22,#22,#22,#22,#22,#22
db #FF,#FF,#F1,#FF,#FF,#11,#1F,#FF,#FF,#FF,#FF,#FF,#AA,#AA,#AA,#AA,#AA,#AA,#22,#22,#22,#22,#22,#22
db #F8,#FF,#1F,#F8,#FF,#F1,#11,#F9,#FF,#9F,#F9,#F1,#AA,#AA,#AA,#AA,#A8,#AA,#22,#22,#22,#22,#82,#22
db #FF,#8F,#1F,#FF,#FF,#F1,#11,#FF,#FF,#FF,#FF,#F1,#AA,#A8,#AA,#AA,#A8,#AA,#12,#22,#22,#22,#82,#21
db #1F,#FF,#1F,#8F,#FF,#F1,#11,#1F,#9F,#F9,#FF,#11,#1A,#AA,#AA,#AA,#8A,#A1,#12,#22,#22,#28,#22,#21
db #11,#11,#1F,#F8,#8F,#F1,#11,#11,#FF,#FF,#F1,#11,#1A,#AA,#AA,#AA,#AA,#A1,#11,#22,#22,#22,#22,#11
db #11,#11,#11,#FF,#FF,#11,#11,#11,#1F,#F1,#11,#11,#11,#1A,#AA,#AA,#A1,#11,#11,#11,#22,#22,#11,#11

;big game logo
sprlogo     db 32,64,48:dw sprlogo0+1,sprlogo0,32*48
sprlogo0    db 5
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#12,#CC,#C2,#91,#11,#11,#11,#11
db #18,#88,#88,#88,#88,#88,#88,#DD,#11,#11,#11,#11,#11,#11,#18,#88,#E1,#11,#11,#11,#11,#11,#11,#3C,#CC,#CC,#CC,#CC,#C1,#11,#11,#11
db #18,#88,#88,#88,#88,#88,#88,#88,#81,#11,#11,#11,#11,#11,#58,#88,#81,#11,#11,#11,#11,#11,#1C,#CC,#CC,#CC,#CC,#CC,#CC,#C1,#11,#11
db #18,#88,#8D,#DD,#DD,#DD,#D8,#88,#88,#31,#11,#11,#11,#11,#88,#88,#80,#11,#11,#11,#11,#13,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#11,#11
db #18,#88,#31,#11,#11,#11,#11,#D8,#88,#81,#11,#11,#11,#15,#88,#88,#88,#31,#11,#11,#11,#9C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#C1,#11
db #18,#88,#21,#11,#11,#11,#11,#15,#88,#83,#11,#11,#11,#18,#88,#35,#88,#81,#11,#11,#13,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#11
db #18,#88,#D1,#11,#11,#11,#11,#11,#88,#8D,#11,#11,#11,#18,#88,#11,#88,#81,#11,#11,#1C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#21
db #18,#88,#31,#11,#11,#11,#11,#11,#88,#8D,#11,#11,#11,#D8,#88,#11,#88,#8D,#11,#11,#9C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#C1,#11
db #18,#88,#21,#11,#11,#11,#11,#11,#88,#8D,#11,#11,#11,#88,#82,#11,#D8,#88,#11,#11,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#C3,#11,#11,#11
db #18,#88,#D1,#11,#11,#11,#11,#15,#88,#83,#11,#11,#15,#88,#81,#11,#18,#88,#21,#11,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#91,#11,#11,#11,#11
db #18,#88,#31,#11,#11,#11,#11,#D8,#88,#81,#11,#11,#18,#88,#21,#11,#16,#88,#81,#13,#CC,#CC,#CC,#CC,#CC,#C3,#11,#11,#11,#11,#11,#11
db #18,#88,#8D,#DD,#DD,#DD,#68,#88,#88,#31,#11,#11,#58,#88,#11,#11,#11,#88,#8E,#19,#CC,#CC,#CC,#C2,#11,#11,#11,#11,#11,#11,#11,#11
db #18,#88,#88,#88,#88,#88,#88,#88,#81,#11,#11,#11,#88,#83,#11,#11,#11,#D8,#88,#13,#CC,#CC,#CC,#CC,#21,#11,#11,#11,#11,#11,#11,#11
db #18,#88,#88,#88,#88,#88,#88,#8E,#11,#11,#11,#11,#88,#80,#DD,#DD,#DD,#68,#88,#11,#CC,#CC,#CC,#CC,#CC,#C9,#11,#11,#11,#11,#11,#11
db #18,#88,#D1,#11,#11,#11,#11,#11,#11,#11,#11,#15,#88,#88,#88,#88,#88,#88,#88,#01,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#C1,#11,#11,#11,#11
db #18,#88,#21,#11,#11,#11,#11,#11,#11,#11,#11,#18,#88,#88,#88,#88,#88,#88,#88,#81,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#31,#11,#11
db #18,#88,#31,#11,#11,#11,#11,#11,#11,#11,#11,#58,#88,#11,#11,#11,#11,#11,#88,#80,#3C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#21,#11
db #18,#88,#D1,#11,#11,#11,#11,#11,#11,#11,#11,#88,#80,#11,#11,#11,#11,#11,#68,#88,#3C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#21
db #18,#88,#21,#11,#11,#11,#11,#11,#11,#11,#15,#88,#81,#11,#11,#11,#11,#11,#18,#88,#03,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#11
db #18,#88,#31,#11,#11,#11,#11,#11,#11,#11,#18,#88,#01,#11,#11,#11,#11,#11,#16,#88,#81,#9C,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#C1,#11
db #18,#88,#D1,#11,#11,#11,#11,#11,#11,#11,#18,#88,#31,#11,#11,#11,#11,#11,#15,#88,#83,#13,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#CC,#11,#11
db #18,#88,#21,#11,#11,#11,#11,#11,#11,#11,#68,#88,#11,#11,#11,#11,#11,#11,#11,#88,#88,#11,#1C,#CC,#CC,#CC,#CC,#CC,#CC,#21,#11,#11
db #18,#88,#31,#11,#11,#11,#11,#11,#11,#11,#88,#80,#11,#11,#11,#11,#11,#11,#11,#68,#88,#21,#11,#3C,#CC,#CC,#CC,#CC,#C1,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#13,#93,#93,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #1D,#DD,#D3,#11,#11,#11,#11,#11,#11,#DD,#DD,#11,#11,#11,#11,#15,#DD,#11,#11,#11,#11,#11,#5D,#DD,#11,#11,#11,#11,#11,#1D,#DD,#11
db #18,#88,#88,#11,#11,#11,#11,#11,#16,#88,#88,#11,#11,#11,#11,#18,#88,#81,#11,#11,#11,#11,#68,#88,#11,#11,#11,#11,#11,#18,#80,#11
db #18,#88,#88,#11,#11,#11,#11,#11,#18,#88,#88,#11,#11,#11,#11,#68,#88,#81,#11,#11,#11,#11,#68,#88,#01,#11,#11,#11,#11,#18,#88,#11
db #18,#88,#88,#31,#11,#11,#11,#11,#18,#88,#88,#11,#11,#11,#11,#88,#88,#8D,#11,#11,#11,#11,#68,#88,#8D,#11,#11,#11,#11,#18,#80,#11
db #18,#88,#88,#01,#11,#11,#11,#11,#D8,#88,#88,#11,#11,#11,#11,#88,#D8,#88,#11,#11,#11,#11,#68,#88,#88,#31,#11,#11,#11,#18,#88,#11
db #18,#80,#68,#81,#11,#11,#11,#11,#88,#08,#88,#11,#11,#11,#1D,#88,#16,#88,#31,#11,#11,#11,#68,#88,#88,#81,#11,#11,#11,#18,#80,#11
db #18,#80,#18,#8E,#11,#11,#11,#11,#88,#38,#88,#11,#11,#11,#18,#88,#11,#88,#81,#11,#11,#11,#68,#83,#88,#83,#11,#11,#11,#18,#88,#11
db #18,#88,#18,#88,#11,#11,#11,#16,#88,#18,#88,#11,#11,#11,#58,#80,#11,#88,#81,#11,#11,#11,#68,#83,#D8,#88,#11,#11,#11,#18,#80,#11
db #18,#80,#16,#88,#11,#11,#11,#18,#82,#18,#88,#11,#11,#11,#68,#83,#11,#58,#80,#11,#11,#11,#68,#83,#18,#88,#01,#11,#11,#18,#88,#11
db #18,#88,#15,#88,#31,#11,#11,#58,#81,#18,#88,#11,#11,#11,#88,#81,#11,#18,#88,#11,#11,#11,#68,#83,#11,#88,#8D,#11,#11,#18,#80,#11
db #18,#80,#11,#88,#E1,#11,#11,#88,#81,#18,#88,#11,#11,#16,#88,#31,#11,#18,#88,#11,#11,#11,#68,#83,#11,#58,#88,#11,#11,#18,#88,#11
db #18,#88,#11,#88,#81,#11,#11,#88,#01,#18,#88,#11,#11,#18,#88,#11,#11,#1D,#88,#01,#11,#11,#68,#83,#11,#18,#88,#01,#11,#18,#80,#11
db #18,#80,#11,#58,#83,#11,#15,#88,#31,#18,#88,#11,#11,#58,#88,#11,#11,#11,#88,#81,#11,#11,#68,#83,#11,#11,#88,#8D,#11,#18,#88,#11
db #18,#88,#11,#18,#80,#11,#1D,#88,#11,#18,#88,#11,#11,#D8,#88,#88,#88,#88,#88,#8E,#11,#11,#68,#83,#11,#11,#58,#88,#11,#18,#80,#11
db #18,#80,#11,#16,#88,#11,#18,#82,#11,#18,#88,#11,#11,#88,#88,#88,#88,#88,#88,#88,#11,#11,#68,#83,#11,#11,#16,#88,#E1,#18,#88,#11
db #18,#88,#11,#15,#88,#21,#58,#81,#11,#18,#88,#11,#15,#88,#8D,#DD,#DD,#DD,#D8,#88,#E1,#11,#68,#83,#11,#11,#11,#88,#83,#18,#80,#11
db #18,#80,#11,#11,#88,#81,#68,#01,#11,#18,#88,#11,#18,#88,#31,#11,#11,#11,#1D,#88,#81,#11,#68,#83,#11,#11,#11,#D8,#88,#18,#88,#11
db #18,#88,#11,#11,#88,#81,#88,#31,#11,#18,#88,#11,#58,#88,#11,#11,#11,#11,#11,#88,#81,#11,#68,#83,#11,#11,#11,#16,#88,#88,#80,#11
db #18,#80,#11,#11,#D8,#88,#88,#11,#11,#18,#88,#11,#68,#83,#11,#11,#11,#11,#11,#88,#8E,#11,#68,#83,#11,#11,#11,#11,#88,#88,#88,#11
db #18,#88,#11,#11,#18,#88,#88,#11,#11,#18,#88,#11,#88,#81,#11,#11,#11,#11,#11,#58,#88,#11,#68,#83,#11,#11,#11,#11,#D8,#88,#80,#11
db #18,#80,#11,#11,#16,#88,#8D,#11,#11,#18,#88,#11,#88,#81,#11,#11,#11,#11,#11,#18,#88,#E1,#68,#83,#11,#11,#11,#11,#18,#88,#88,#11
db #58,#88,#11,#11,#15,#88,#81,#11,#11,#18,#88,#16,#88,#E1,#11,#11,#11,#11,#11,#1D,#88,#81,#68,#83,#11,#11,#11,#11,#11,#88,#80,#11
db #1D,#D3,#11,#11,#11,#DD,#D1,#11,#11,#1D,#DD,#1D,#DD,#11,#11,#11,#11,#11,#11,#11,#DD,#D1,#5D,#D1,#11,#11,#11,#11,#11,#1D,#D3,#11

fldgfx  db fldxln*2, fldxln*8, fldyln*8
        dw fldgfx1+1,fldgfx1+0,fldxln*fldyln*8*2
fldgfx1 db 0,0
;!!!last label!!!


;==============================================================================
;### T R A N S F E R   A R E A ################################################
;==============================================================================

App_BegTrns
;### PRGPRZS -> stack for main process
        ds 128
prgstk  ds 6*2
        dw prgprz
App_PrcID
prgprzn db 0

;### GAMTIMS -> stack for timer process
        ds 128
gamtims ds 6*2
        dw gamtim
gamtimn db 0

;### Message buffer
App_MsgBuf
prgmsgb ds 14

;### CONFIG WINDOW ############################################################

configwin   dw #1401,4+16,049,035,220,120,0,0,220,120,220,120,220,120,0,configtit,0,0,configgrp,0,0:ds 136+14
configgrp   db 14,0:dw configdat,0,0,256*14+14,0,0,03
configdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 3,configdsc0, 00, 01,220,47,0       ;01=Rahmen "Settings"
dw      00,255*256+18,configrad1a,08, 13, 60, 8,0       ;02=Radiobox "Keyboard"
dw      00,255*256+18,configrad1b,08, 23, 60, 8,0       ;03=Radiobox "Joystick"
dw      00,255*256+17,configchk1, 68, 13, 60, 8,0       ;04=Checkbox "Sound"
dw      00,255*256+17,configchk2, 68, 23, 60, 8,0       ;05=Checkbox "Multilevel"
dw      00,255*256+17,configchk3, 68, 33, 60, 8,0       ;06=Checkbox "Slow down speed"
dw      00,255*256+ 3,configdsc1, 00, 47,220,57,0       ;07=Rahmen "About"
dw      00,255*256+ 8,prgicnbig,  08, 59, 24,24,0       ;08=Grafik PacMan
dw      00,255*256+ 1,configdsc2, 37, 59,184, 8,0       ;09=Beschreibung "About 1"
dw      00,255*256+ 1,configdsc3, 37, 69,184, 8,0       ;10=Beschreibung "About 2"
dw      00,255*256+ 1,configdsc4, 47, 79,184, 8,0       ;11=Beschreibung "About 3"
dw      00,255*256+ 1,configdsc5, 47, 89,184, 8,0       ;12=Beschreibung "About 4"
dw prgcfgc,255*256+16,prgtxtok,   86,104, 48,12,0       ;13="Ok"    -Button

configdsc0  dw configtxt1,2+4
configdsc1  dw configtxt7,2+4
configdsc2  dw configtxt8,2+4
configdsc3  dw configtxt9,2+4
configdsc4  dw configtxta,2+4
configdsc5  dw configtxtb,2+4

configkey   db 0
configrad1k ds 4
configrad1a dw configkey,configtxt2,0*256+2+4,configrad1k
configrad1b dw configkey,configtxt3,1*256+2+4,configrad1k

configsnd   db 0
configchk1  dw configsnd,configtxt4,2+4

configmul   db 1
configchk2  dw configmul,configtxt5,2+4

configspd   db 0
configchk3  dw configspd,configtxt6,2+4

;### MAIN WINDOW ##############################################################

prgwindat dw #1501,0,94,03,fldxln*8+3+64,fldyln*8+2,0,0,fldxln*8+3+64,fldyln*8+2,1,1,1000,1000,prgicnsml,prgwintit,0,0,prgwingrp,0,0:ds 136+14

prgwingrp db 29,0:dw prgwinobj,0,0,256*0+15,0,0,15
prgwinobj
dw     00,255*256+02,3*4+3+16+64,0,0,fldxln*8+3+64,fldyln*8+2,0 ;00=background
dw     00,255*256+00,3          ,fldxln*8+1,1,1,fldyln*8,0      ;01=play field/status display separator line
dw     00,255*256+10,sprlogo   ,fldxln*8+2,2,64,48,0            ;02=big game logo
dw     00,255*256+01,objtitsco ,fldxln*8+4,58,60,8,0            ;03=Title "Score"
dw     00,255*256+01,objtithig ,fldxln*8+4,82,60,8,0            ;04=Title "Highscore"

dw     00,255*256+00,1         ,fldxln*8+04,106,12*5,10,0       ;05=background for the lives
prgwinobj2
dw     00,255*256+10,sprlives  ,fldxln*8+04,106,12,10,0         ;06=Live 1
dw     00,255*256+10,sprlives  ,fldxln*8+16,106,12,10,0         ;07=Live 2
dw     00,255*256+10,sprlives  ,fldxln*8+28,106,12,10,0         ;08=Live 3
dw     00,255*256+10,sprlives  ,fldxln*8+40,106,12,10,0         ;09=Live 4
dw     00,255*256+10,sprlives  ,fldxln*8+52,106,12,10,0         ;10=Live 5
prgwinobj6
dw     00,255*256+10,sprbonus1 ,fldxln*8+04,122,12,12,0         ;11=Bonus
dw     00,255*256+00,0         ,fldxln*8+3,140,62,27,0          ;12=background for the buttons
dw prgcfg ,255*256+16,txtbutopt,fldxln*8+4,154,60,12,0          ;13=Button "Options"
prgwinobj3
dw gamctl,255*256+16,txtbutrun ,fldxln*8+4,141,60,12,0          ;14=Button "Start/Pause/Continue"
dw     00,255*256+10,fldgfx    ,1,1,fldxln*8,fldyln*8,0         ;15=Play field
dw     00,255*256+01,objnumhig ,fldxln*8+4,92,60,8,0            ;16=Number "Highscore"
dw     00,255*256+01,objnumsco ,fldxln*8+4,68,60,8,0            ;17=Number "Score"
prgwinobj5
dw     00,255*256+10,sprfield  ,1,1,8,8,0                       ;18=update changed field
prgwinobj1
dw     00,255*256+10,sprremov1 ,1,1,16,10,0                     ;19=remove Ghost1
dw     00,255*256+10,sprghost1 ,08*8-1,09*8,12,10,0             ;20=show   Ghost1
dw     00,255*256+10,sprremov2 ,1,1,16,10,0                     ;21=remove Ghost2
dw     00,255*256+10,sprghost2 ,09*8-1,09*8,12,10,0             ;22=show   Ghost2
dw     00,255*256+10,sprremov3 ,1,1,16,10,0                     ;23=remove Ghost3
dw     00,255*256+10,sprghost3 ,10*8-1,09*8,12,10,0             ;24=show   Ghost3
dw     00,255*256+10,sprremov4 ,1,1,16,10,0                     ;25=remove Ghost4
dw     00,255*256+10,sprghost4 ,09*8-1,08*8,12,10,0             ;26=show   Ghost4
prgwinobj0
dw     00,255*256+00,1         ,09*8-1,15*8,12,10,0             ;27=remove Pacman
dw     00,255*256+10,sprpacman ,09*8-1,15*8,12,10,0             ;28=show   Pacman
dw     00,255*256+00,1         ,09*8-1,11*8-1,12,12,0           ;29=show/remove Bonus Fruit
prgwinobj4
dw     00,255*256+01,objgammsg ,06*8+1,11*8+1,7*8,8,0           ;30=inbetween event text/restore

objgammsg   dw txtgamlev,4*2+3+128+512
objtitsco   dw txttitsco,4*2+1
objtithig   dw txttithig,4*2+1
objtitliv   dw txttitliv,4*2+1
objnumsco   dw txtnumsco,4*0+1+256
objnumhig   dw txtnumhig,4*0+1+256
