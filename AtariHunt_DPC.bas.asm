; Provided under the CC0 license. See the included LICENSE.txt for details.

 processor 6502
 include "vcs.h"
 include "macro.h"
 include "DPCplus.h"
 include "DPCplusbB.h"
 include "2600basic_variable_redefs.h"
 ORG $400
 RORG $0
 incbin "DPCplus.arm"
     ORG $1000
     RORG $1000
 incbin "custom/bin/custom2.bin"
; assume custom2.bin > 128 bytes
; repeat $80
; .byte 0
; repend
; Provided under the CC0 license. See the included LICENSE.txt for details.
; font equates
.21stcentury = 1
alarmclock = 2     
handwritten = 3    
interrupted = 4    
retroputer = 5    
whimsey = 6
tiny = 7
hex = 8

; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifnconst PXE
 ifconst ROM2k
   ORG $F7AC-8
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 64
       ORG  $10F80-bscode_length
       RORG $1FF80-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif


 ifconst font
   if font == hex
     ORG . - 48
   endif
 endif
 endif

scoretable

 ifconst font
  if font == .21stcentury
    include "score_graphics.asm.21stcentury"
  endif
  if font == alarmclock
    include "score_graphics.asm.alarmclock"
  endif
  if font == handwritten
    include "score_graphics.asm.handwritten"
  endif
  if font == interrupted
    include "score_graphics.asm.interrupted"
  endif
  if font == retroputer
    include "score_graphics.asm.retroputer"
  endif
  if font == whimsey
    include "score_graphics.asm.whimsey"
  endif
  if font == tiny
    include "score_graphics.asm.tiny"
  endif
  if font == hex
    include "score_graphics.asm.hex"
  endif
 else ; default font

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 

       ifnconst DPC_kernel_options
 
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000 

       endif

 endif

 ifnconst PXE
 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 64
       ORG  $10FE0-bscode_length
       RORG $1FFE0-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
 endif
; Provided under the CC0 license. See the included LICENSE.txt for details.

start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ldx #8
 stx playfieldpos
 stx FASTFETCH
 ldx #8
 lda #224
inityloop
 sta player1y,x
 dex
 bpl inityloop

 lda #1
 sta CTRLPF
 lda INTIM
 sta RWRITE0
 lda #0
 STA DF0FRACINC
 STA DF1FRACINC
 STA DF2FRACINC
 STA DF3FRACINC
 STA DF4FRACINC
 STA DF6FRACINC
 lda #<USERSTACK
 STA DF7LOW
 lda #(>USERSTACK) & $0F
 STA DF7HI
 lda #255
 sta CALLFUNCTION ; zero-fill fetcher

   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
drawscreen
     lda #1
     sta CXCLR
     sta COLUBK ; REVENG - don't start with the lastline color

fufu
     lda INTIM
     bmi fufu

     VERTICAL_SYNC

     lda #41+128;was 37 - do more w/c code
     sta TIM64T

     ; adjust for pfpos?

     ; set zero to properly enter C code
     lda #<C_function
     sta DF0LOW
     lda #(>C_function) & $0F
     sta DF0HI
     lda #0
     sta DF0WRITE

     ; REVENG - pass the number of vsprites we want...
     ifnconst dpcspritemax
       ifconst readpaddle
          lda #8
       else
          lda #9
       endif
     else
       lda #dpcspritemax
     endif
     sta DF0WRITE 

     lda player0x
     sta player0xcoll ; detect p0x colls

     ; copy RAM to fetcher for C-code
     lda #<(CcodeData + RAMcopylength)
     sta DF0LOW
     lda #(>(CcodeData + RAMcopylength)) & $0F
     sta DF0HI
     ldx #RAMcopylength-1
copy2fetcherloop
     lda RAMcopybegin,x
     sta DF0PUSH
     dex
     bpl copy2fetcherloop

     lda #255
     sta CALLFUNCTION

     ; copy modified data back (just need first 6 bytes, which is sprite sort data)
     ldx #256-19
copyfromfetcherloop
     lda DF0DATA
     sta RAMcopybegin+19,x
     inx
     bmi copyfromfetcherloop

     jsr kernel_setup
     sta WSYNC
     ldy #$80
     sty HMP0
     sty HMP1
     sty HMM0 
     sty HMM1
     sty HMBL

     ; run possible vblank bB code
     ifconst vblank_bB_code
         jsr vblank_bB_code
     endif

     jsr set_fetchers

     ldx #7
setloopfrac
     lda dffraclow,x
     sta DF0FRACLOW,x
     lda dffrachi,x
     sta DF0FRACHI,x
     dex
     bpl setloopfrac
     ; lda #255
     STx DF5FRACINC ; x=255 right now
     STx DF7FRACINC
     lda DF5FRACDATA ; priming read
     lda DF7FRACDATA ; priming read

     ldx SpriteGfxIndex
     lda _NUSIZ1,x ; top NUSIZ/REFP
     sta NUSIZ1
     sta REFP1

     ;REVENG - allow P0 to wrap at the top
startwrapfix
     lda #255
     sta temp2
     clc
     lda player0y
     adc player0height
     sec
     cmp player0height
     bcc skipwrapfix
     lda #0
     sta temp2
skipwrapfix

     sec
     lda #<P0GFX
     sbc player0y
     sta DF2LOW
     lda #>P0GFX
     ;sbc #0
     sbc temp2
     sta DF2HI
     lda #<(P0GFX-1)
     sta DF2TOP
     sec
     adc player0height
     sta DF2BOT

     ;REVENG - 1/2 of the COLUM0 fix. the rest is in main.c
     lda #<(P0COLOR)
     sta DF0LOW
     sta temp2
     lda #>(P0COLOR)
     sta DF0HI

     ; ball
     lda #<(P1GFX-1)
     clc
     adc bally
     sta DF3TOP
     sec
     adc ballheight
     sta DF3BOT

     ; missile0
     lda temp2
     clc
     adc missile0y
     sta DF0TOP
     sec
     adc missile0height
     sta DF0BOT


fuu
     lda INTIM
     bmi fuu
     sta WSYNC
;     ldy #$80
;     sty HMP0
;     sty HMP1
;     sty HMM0 
;     sty HMM1
;     sty HMBL
; relocated code above prior to vblank, to allow for Cosmic Ark starfield
; and/or skewed players
 sleep 17 

     lda #KERNEL_LINES
     sta TIM64T
     lda #1
     sta VDELBL
     sta VDELP0

     ; missile1
     lda #<(P1COLOR-1)
     clc
     adc missile1y
     sta DF1TOP
     sec
     adc missile1height
     sta DF1BOT

     lda #0
     sta VBLANK
     sta FASTFETCH
     ;sleep 7
     lda #<DF2DATAW         ; REVENG - added so GRP0 is at TOP
     STA GRP0 ; 36 (VDEL)   ; ""
     sleep 2                ; ""

     lda #<DF0FRACDATA
     sta PF1 ; (PF1L)

     ; enter at cycle ??
loop:
     lda #<DF0DATA ;74
     STA COLUP0 ; 1
     lda #<DF1DATA ;3
loop2
     STA COLUP1 ; 6
     lda #<DF3DATA
     STA GRP1 ; 11
     lda #<DF0FLAG
     STA ENAM0 ; 16

     lda #<DF6FRACDATA
     sta COLUBK ; 21
     lda #<DF4FRACDATA
     sta COLUPF ; 26
     lda #<DF1FRACDATA
     sta PF2 ; 31 (PF2L)
loop3
     lda #<DF2DATAW
     STA GRP0 ; 36 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 41 (VDEL)
     ldx #$70 ;in case we get kernel 6
     lda #<DF2FRACDATA ;45
     sta PF2 ; 48
     sty HMP1 ; 51 ; from prev. cycle: $80=nomove
     lda #<DF3FRACDATA ;53
     sta PF1 ; 56
     lda #<DF4DATA ; 58 this is the repos info
     beq repo ;60/61
norepo     ; 60
     tay ; 62
     lda #<DF0DATA ; 64

     ldx INTIM ; 68 timed for 192 lines
     beq exitkernel; 70/71
     sta HMOVE ; 73

     STA COLUP0 ; 0
     lda #<DF1DATA ;2
     STA COLUP1 ;5
     lda #<DF3DATA
     STA GRP1 ; 10
     lda #<DF1FLAG
     STA ENAM1 ; 15
     lda #<DF0FRACDATA
     sta PF1 ; 20 (PF1L)
     lda #<DF1FRACDATA
     sta PF2 ; 25 (PF2L)
     lda #<DF2DATAW
     STA GRP0 ; 30 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 35 (VDEL)
     dey ; 37
     STY DF4PUSH ; 41
     ldy #$80 ; 43 no movement next line
     lda #<DF2FRACDATA ;45
     sta PF2 ; 48
     sty HMP1 ; 51 ; from prev. cycle: $80=nomove
     lda #<DF3FRACDATA ;53
     sta PF1 ; 56
     ifnconst DPC_kernel_options
         ;sleep 8 ; REVENG - timing is off - results in a garbled screen
         sleep 5 ; this is better
     else
         bit DPC_kernel_options
         if (DPC_kernel_options > $3F)
             bmi COLfound
         else
             bpl COLfound
         endif
     endif
     stx temp4 ; +3

getbackearly
     lda #<DF0FRACDATA ; +2
     sta PF1 ; 69 (PF1L) too early?
     JMP loop+$4000 ; 72

     ifconst DPC_kernel_options
COLfound
         lda DF0FRACDATA
         sta PF1 ; 69 (PF1L) too early?
         JMP loop+$4000 ; 72
     endif

repo     
     ldy DF7FRACDATA ; 65
     lda #<DF0FRACDATA ; 67 preload PF1L for next line
     if ((>repo) > (>norepo))
         STA PF1
     else
         STA.w PF1 ; 71 ; sta.w if page doesn't wrap
     endif
     lda #<DF0DATA ;73
     STA COLUP0 ; 0
     lda #<DF1DATA 
     STA COLUP1 ;5
     lda #<DF3DATA
     STA GRP1 ; 10
     lda #<DF1FLAG
     STA ENAM1 ; 15
     ; repos info holds HMMx
     jmp (DF5DATA) ; 20 grabs df6/df7=lo/hi

exitkernel     ; exit the kernel
     jsr scorekernel+$4000 ; 1
exit
     ldx #255
     stx FASTFETCH
     sta WSYNC
     ifconst qtcontroller
        lda qtcontroller
        lsr    ; bit 0 in carry
        lda #4
        ror    ; carry into top of A
     else
        lda #2
     endif ; qtcontroller
     STA VBLANK
     lda #OVERSCAN_LINES
     sta TIM64T
     sec
     lda #KERNEL_LINES
     sbc temp4
     tax
     lsr
     lsr 
     sta temp3 ; div4
     lsr
     lsr
     sta temp2 ; div16
     lsr
     sta temp1 ; div32
     clc
     txa
     adc temp2
     adc temp1
     sec
     sbc temp3
     sta temp4 ; approx line of first pf coll
     RETURN

     ; jmp exit

     ; kernels resp1 23/28/33/38/43/48/53/58/63/68/73

kernel1
     sta RESP1 ; 23
     lda #<DF2DATAW
     STA GRP0 ; 28 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 33
     lda #<DF3FLAG
     STA ENABL ; 38 (VDEL)
     sleep 5
     lda #<DF2FRACDATA ;45
     sta PF2 ; 48
     lda #<DF3FRACDATA ;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 58
     STA REFP1 ; 61
     jmp getbackearly ;64

kernel2
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     sta RESP1 ;28
     lda #<DF1FRACDATA
     STA PF2 ; 33
     lda #<DF3FLAG
     STA ENABL ; 38 (VDEL)
     sleep 5
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1
     STA REFP1
     jmp getbackearly ;64

kernel3
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 30
     sta RESP1 ;33
     lda #<DF3FLAG
     STA ENABL ; 38 (VDEL)
     sleep 5
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1
     STA REFP1
     JMP getbackearly ; 64

kernel4
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 30(VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     sta RESP1 ;38
     sleep 5
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 58
     STA REFP1 ; 61
     JMP getbackearly ; 64

kernel5
     lda #<DF2DATAW
     STA GRP0 ; (VDEL)
     lda #<DF3FLAG
     STA ENABL ; (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     sleep 5
     sta RESP1 ;43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1
     STA REFP1
     JMP getbackearly ; 64

kernel6
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 30 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta RESP1 ;53
     ; do a move right by 15
     sta PF1 ; 56
     stx HMP1 ; 59
     lda #<DF1FRACDATA
     sta PF2 ; 64 (PF2L)
     lda #<DF0FRACDATA
     sta PF1 ; 69 (PF1L) too early?
     lda #<DF0DATA ; 71
     sta HMOVE ; 74 adjust to +15 right

     STA COLUP0 ; 1
     lda #<DF1DATA
     sta COLUP1 ; 6
     lda #<DF3DATA
     STA GRP1 ; 11
     lda #<DF0FLAG
     STA ENAM0 ; 16
     lda #<DF6FRACDATA
     STA COLUBK ; 21
     lda #<DF4FRACDATA
     sta COLUPF ; 26
     sleep 2
     jmp loop3 ; 31

kernel7
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 30 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     sleep 2
     sta RESP1 ;53
     lda #<DF3FRACDATA;55
     sta PF1 ; 58
     sleep 3
     JMP getbackearly ; 64

kernel8
     lda #<DF2DATAW
     STA GRP0 ; (VDEL)
     lda #<DF3FLAG
     STA ENABL ; (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     sleep 2
     sta RESP1 ;58
     sleep 3
     JMP getbackearly ; 64

kernel9
     lda #<DF2DATAW
     STA GRP0 ; (VDEL)
     lda #<DF3FLAG
     STA ENABL ; (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     sleep 5
     lda #<DF0FRACDATA
     sta RESP1 ;63
     sleep 3
     sta PF1 ; 69 (PF1L) too early?
     jmp loop ;72

kernel10
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 30 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     sleep 6
     lda #<DF0FRACDATA
     LDX DF0DATA ; 65
     sta RESP1 ; 68
     STA PF1 ; 71
     lda #<DF1DATA ; 74
     STX COLUP0 ; 0
     jmp loop2 ; 3

kernel11
     lda #<DF2DATAW
     STA GRP0 ; (VDEL)
     lda #<DF3FLAG
     STA ENABL ; (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1
     STA REFP1
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     sleep 3
     lda #<DF1FRACDATA;45
     sta PF2 ; 61
     LDX DF0DATA ; 65

     lda #<DF0FRACDATA ; 67
     sta PF1 ; 70
     sta RESP1 ; 73
     STX COLUP0 ; 0
     lda #<DF1DATA ; 2
     sta COLUP1 ; 5
     lda #<DF3DATA
     STA GRP1 ; 10
     lda #<DF0FLAG
     STA ENAM0 ; 25
     lda #<DF6FRACDATA
     STA COLUBK ; 20
     lda #<DF4FRACDATA
     sta COLUPF ; 25
     sleep 3
     jmp loop3 ; 31

set_fetchers
     lda dflow
     sta DF0LOW
     lda dfhigh
     sta DF0HI

     lda dflow+1
     sta DF1LOW
     lda dfhigh+1
     sta DF1HI

     lda dflow+2
     sta DF2LOW
     lda dfhigh+2
     sta DF2HI

set_fetchers36 ; sets just 3-6
     lda dflow+3
     sta DF3LOW
     lda dfhigh+3
     sta DF3HI

     lda dflow+4
     sta DF4LOW
     lda dfhigh+4
     sta DF4HI

     lda dflow+5
     sta DF5LOW
     lda dfhigh+5
     sta DF5HI

     lda dflow+6
     sta DF6LOW
     lda dfhigh+6
     sta DF6HI

     rts

     ;9d bad
     ; the below isn't quite right
     ;DF0DATA: COLUP0
     ;DF1DATA: COLUP1
     ;DF2DATAW: GRP0
     ;DF3DATA: GRP1 
     ;DF4DATA: 2lk lines until repos/HMP1
     ;DF5DATA: low byte of repo kernels (xpos mod 15)
     ;DF6DATA: High byte of repo kernels (x pos div 15)
     ;DF7DATA: Programmer's stack
     ;DF0FRACDATA: PF1L
     ;DF1FRACDATA: PF2L
     ;DF4FRACDATA: COLUPF
     ;DF2FRACDATA: PF2R
     ;DF3FRACDATA: PF2L
     ;DF5FRACDATA: Sprite NUSIZ1/REFP1 (only during repos)
     ;DF6FRACDATA: COLUBK
     ;DF7FRACDATA: HMP1
     ;DF3FLAG: kernel exit loop ?? (use flags instead?)
     ;DF0FLAG: ENAM0
     ;DF1FLAG: ENAM1 
     ;DF3FLAG: ENABL 

fetcher_address_table
kernello
     .byte <kernel1
     .byte <kernel2
     .byte <kernel3
     .byte <kernel4
     .byte <kernel5
     .byte <kernel6
     .byte <kernel7
     .byte <kernel8
     .byte <kernel9
     .byte <kernel10
     .byte <kernel11
kernelhi
     .byte >kernel1
     .byte >kernel2
     .byte >kernel3
     .byte >kernel4
     .byte >kernel5
     .byte >kernel6
     .byte >kernel7
     .byte >kernel8
     .byte >kernel9
     .byte >kernel10
     .byte >kernel11
dflow     
     .byte <P0COLOR
     .byte <P1COLOR
     .byte <P0GFX
     .byte <P1GFX
     .byte <P1SKIP
     .byte <JUMPTABLELO
     .byte <JUMPTABLEHI
     .byte <USERSTACK
dfhigh
     .byte (>P0COLOR) & $0F
     .byte (>P1COLOR) & $0F
     .byte (>P0GFX) & $0F
     .byte (>P1GFX) & $0F
     .byte (>P1SKIP) & $0F
     .byte (>JUMPTABLELO) & $0F
     .byte (>JUMPTABLEHI) & $0F
     .byte (>USERSTACK) & $0F
dffraclow
     .byte <PF1L
     .byte <PF2L
     .byte <PF1R
     .byte <PF2R
     .byte <PFCOLS
     .byte <NUSIZREFP
     .byte <BKCOLS
     .byte <P1HMP
dffrachi
     .byte (>PF1L) & $0F
     .byte (>PF2L) & $0F
     .byte (>PF1R) & $0F
     .byte (>PF2R) & $0F
     .byte (>PFCOLS) & $0F
     .byte (>NUSIZREFP) & $0F 
     .byte (>BKCOLS) & $0F
     .byte (>P1HMP) & $0F
scorepointer
     .byte <scoretable
     .byte ((>scoretable) & $0f) | (((>scoretable) / 2) & $70)
scoresetup     ; pointers to digit graphics
     .byte <scoredata
     .byte (>scoredata) & $0F
Hmval; 112 wuz first
     .byte 96, 80, 64, 48, 32, 16, 1, 240
Hmval74
     .byte 224, 208, 192, 176, 160, 144, 128
     .byte 96, 80, 64, 48, 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96
     .byte 80, 64, 48, 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80
     .byte 64, 48, 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64
     .byte 48, 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48
     .byte 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32
     .byte 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32, 16
     .byte 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32, 16, 1
     .byte 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32, 16, 1, 240
     .byte 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32, 16, 1, 240, 224, 208, 192
     .byte 176,160,144,128,16,1,240,224
     

kernel_setup
     ;--position P0, top P1, M0, M1, BL
     ldx #0 ; first sprite displayed
     lda SpriteGfxIndex,x
     tax
     lda player1x,x
     cmp #160
     bcc nostorep1
     cmp #208
     bcs ksadjustdown
     ; 160-208: minus 160
     ;add 160 is like minus 96
     ; so minus 64
     sbc #63 ;cc
ksadjustdown
     ; 209-255: add 160 
     adc #159 ; cs
     sta player1x,x
nostorep1
     sta WSYNC
     ldx #4
     sta topP1x ; cache top p1
HorPosLoop
     lda player0x,X
     sec
DivideLoop
     sbc #15
     bcs DivideLoop
     sleep 4
     sta RESP0,X
     sta WSYNC
     dex ;2
     bpl HorPosLoop ;4/5

     ldy player0x ; 7
     lda Hmval,y ; 11
     sta HMP0 ; 14

     ldy player0x+1 
     lda Hmval,y
     sta HMP0+1 ; 24

     ldy player0x+2 
     lda Hmval,y
     sta HMP0+2 ; 34

     ldy player0x+3
     lda Hmval,y
     sta HMP0+3 ; 44

     ldy player0x+4 
     lda Hmval,y
     sta HMP0+4 ; 54

     sta WSYNC
     sta HMOVE

myrts
     rts


pfsetup     
     
     sty temp1 
     sta temp2
     stx temp3
     ldx #3
pfsetupp
     lda dffraclow,x
     sta DF0LOW,x
     lda dffrachi,x
     sta DF0HI,x 
     lda temp2
     sta PARAMETER
     lda temp3
     sta PARAMETER
     stx PARAMETER
     sty PARAMETER 
     LDA #1
     sta CALLFUNCTION
     clc
     lda temp2
     adc temp1
     sta temp2
     lda temp3
     adc #0
     sta temp3
     dex
     bpl pfsetupp
     RETURN


scorekernel
     ifconst minikernel
         ;; disable fast fetch, call the minikernel, and re-enable fast fetch
         lda #255
         sta FASTFETCH
         jsr minikernel
         lda #0
         sta.w FASTFETCH
     endif
     ldx scorecolor
     stx COLUP0
     stx COLUP1
     ldx #0
     STx PF1
     stx REFP0
     stx REFP1
     STx GRP0
     STx GRP1
     STx PF2
     stx HMCLR
     stx ENAM0
     stx ENAM1
     stx ENABL


     ifconst pfscore
         lda pfscorecolor
         sta COLUPF
     endif

     ifconst noscore
         ldx #10
noscoreloop
         sta WSYNC
         dex
         bpl noscoreloop
         rts
     else

     sta HMCLR
     ldx #$f0
     stx HMP0

     ; set up fetchers 0-5 to handle score digits
     ldx #<(scoredata)
     stx DF6LOW
     ldx #(>(scoredata)) & $0F
     stx DF6HI
     ldx #<(scoredata+8)
     stx DF0LOW
     ldx #(>(scoredata+8)) & $0F
     stx DF0HI
     ldx #<(scoredata+16)
     stx DF1LOW
     ; cycle 0??
     ldx #(>(scoredata+16)) & $0F
     stx DF1HI
     ldx #<(scoredata+24)
     stx DF2LOW
     ldx #(>(scoredata+24)) & $0F
     stx DF2HI

     sta WSYNC
     ldx #0
     STx GRP0
     STx GRP1 ; seems to be needed because of vdel

     ldx #<(scoredata+32)
     stx DF3LOW
     ldx #(>(scoredata+32)) & $0F
     stx DF3HI
     ldx #<(scoredata+40)
     stx DF4LOW
     ldx #(>(scoredata+40)) & $0F
     stx DF4HI

     LDY #7
     LDx #$03
     STY VDELP0
     STA RESP0
     STA RESP1
     sty temp1

     STx NUSIZ0
     STx NUSIZ1
     STx VDELP1
     ldx #<(scoredata+48)
     stx DF5LOW
     ldx #(>(scoredata+48)) & $0F
     stx DF5HI
     STA.w HMOVE ; cycle 73 ?
scoreloop
     lda #<DF6DATA ;59
     sta COLUP0 ;62
     sta COLUP1 ;65
     lda #<DF1DATA;75
     sta GRP0 ;2
     lda #<DF0DATA ;4
     sta GRP1 ;7
     lda #<DF3DATA ;9
     sta GRP0 ;12

     ; REVENG - rearranged to correct pf write timing and A register overwrite
     ifconst pfscore
         lda pfscore1
         sta PF1
     else
         sleep 6
     endif
     sleep 5 
     ldx DF2DATA;16
     ldy DF5DATA;20
     lda #<DF4DATA;22 

     stx GRP1;40
     sty GRP0;43
     sta GRP1;46
     sta GRP0;49
     ifconst pfscore
         lda pfscore2
         sta PF1
     else
         sleep 6
     endif
     ; sleep 2 ;57
     sleep 6
     dec temp1;70
     bpl scoreloop;72/73
     LDx #0 
     stx PF1
     STx GRP0
     STx GRP1
     STx VDELP0
     STx VDELP1;do we need these
     STx NUSIZ0
     STx NUSIZ1

     rts

     
     endif ; noscore
game
.
 ;;line 1;; 

.
 ;;line 2;; 

.
 ;;line 3;; 

.
 ;;line 4;; 

.
 ;;line 5;; 

.L00 ;;line 6;;  dim _P1_L_R = n

.L01 ;;line 7;;  dim _P1_U_D = o

.L02 ;;line 8;;  dim _P0_L_R = player0x.a

.L03 ;;line 9;;  dim _P0_U_D = player0y.b

.
 ;;line 10;; 

.
 ;;line 11;; 

.
 ;;line 12;; 

.
 ;;line 13;; 

.
 ;;line 14;; 

.L04 ;;line 15;;  dim _Bit0_Reset_Restrainer = c

.L05 ;;line 16;;  dim _Bit1_FireB_Restrainer = c

.L06 ;;line 17;;  dim _Splash_Active = q

.L07 ;;line 18;;  dim _Bit0_Bird_Dead = d

.L08 ;;line 19;;  dim _Bit1_Bird_Falling = d

.L09 ;;line 20;;  dim _Bit2_Dog_Show = d

.
 ;;line 21;; 

.
 ;;line 22;; 

.L010 ;;line 23;;  dim _bird_counter = e

.L011 ;;line 24;;  dim _wait_counter = f

.L012 ;;line 25;;  dim _bulletcounter = g

.L013 ;;line 26;;  dim _Master_Counter = h

.L014 ;;line 27;;  dim _Frame_Counter = i

.L015 ;;line 28;;  dim _Frame_Counter_dead = j

.L016 ;;line 29;;  dim _flight_pattern = k

.L017 ;;line 30;;  dim _dog_timer = l

.L018 ;;line 31;;  dim _dog_frame = m

.L019 ;;line 32;;  dim _bird_dir = p

.L020 ;;line 33;;  dim _Splash_Blink = var0

.L021 ;;line 34;;  dim _Round = var1

.L022 ;;line 35;;  dim _Shots_Remaining = var2

.L023 ;;line 36;;  dim _Shots_Fired = r

.L024 ;;line 37;;  dim _Round_Hits = var4

.L025 ;;line 38;;  dim _Game_Over = var5

.L026 ;;line 39;;  dim _Just_Started = var6

.L027 ;;line 40;;  dim _Accuracy = var7

.L028 ;;line 41;;  dim _Splash_P1_L_R = s

.L029 ;;line 42;;  dim _Splash_P1_U_D = t

.L030 ;;line 43;;  dim _Splash_P1_Pattern = u

.L031 ;;line 44;;  dim _Splash_P1_Dir = v

.
 ;;line 45;; 

.
 ;;line 46;; 

.
 ;;line 47;; 

.
 ;;line 48;; 

.L032 ;;line 49;;  dim rand16 = z

.L033 ;;line 50;;  dim _Bit7_Splash_Seen = rand16

.
 ;;line 51;; 

.
 ;;line 52;; 

.
 ;;line 53;; 

.
 ;;line 54;; 

.
 ;;line 55;; 

.L034 ;;line 56;;  set kernel DPC + 

.L035 ;;line 57;;  set kernel_options pfcolors

.
 ;;line 58;; 

.
 ;;line 59;; 

.
 ;;line 60;; 

.
 ;;line 61;; 

.
 ;;line 62;; 

.L036 ;;line 63;;  set tv ntsc

.
 ;;line 64;; 

.
 ;;line 65;; 

.
 ;;line 66;; 

.
 ;;line 67;; 

.
 ;;line 68;; 

.
 ;;line 69;; 

.
 ;;line 70;; 

.
 ;;line 71;; 

.
 ;;line 72;; 

.
 ;;line 73;; 

.
 ;;line 74;; 

.
 ;;line 75;; 

.L037 ;;line 76;;  const _00 = $00

.L038 ;;line 77;;  const _02 = $02

.L039 ;;line 78;;  const _04 = $04

.L040 ;;line 79;;  const _06 = $06

.L041 ;;line 80;;  const _08 = $08

.L042 ;;line 81;;  const _0A = $0A

.L043 ;;line 82;;  const _0C = $0C

.L044 ;;line 83;;  const _0E = $0E

.L045 ;;line 84;;  const _10 = $10

.L046 ;;line 85;;  const _12 = $12

.L047 ;;line 86;;  const _14 = $14

.L048 ;;line 87;;  const _16 = $16

.L049 ;;line 88;;  const _18 = $18

.L050 ;;line 89;;  const _1A = $1A

.L051 ;;line 90;;  const _1C = $1C

.L052 ;;line 91;;  const _1E = $1E

.L053 ;;line 92;;  const _20 = $20

.L054 ;;line 93;;  const _22 = $22

.L055 ;;line 94;;  const _24 = $24

.L056 ;;line 95;;  const _26 = $26

.L057 ;;line 96;;  const _28 = $28

.L058 ;;line 97;;  const _2A = $2A

.L059 ;;line 98;;  const _2C = $2C

.L060 ;;line 99;;  const _2E = $2E

.L061 ;;line 100;;  const _30 = $30

.L062 ;;line 101;;  const _32 = $32

.L063 ;;line 102;;  const _34 = $34

.L064 ;;line 103;;  const _36 = $36

.L065 ;;line 104;;  const _38 = $38

.L066 ;;line 105;;  const _3A = $3A

.L067 ;;line 106;;  const _3C = $3C

.L068 ;;line 107;;  const _3E = $3E

.L069 ;;line 108;;  const _40 = $40

.L070 ;;line 109;;  const _42 = $42

.L071 ;;line 110;;  const _44 = $44

.L072 ;;line 111;;  const _46 = $46

.L073 ;;line 112;;  const _48 = $48

.L074 ;;line 113;;  const _4A = $4A

.L075 ;;line 114;;  const _4C = $4C

.L076 ;;line 115;;  const _4E = $4E

.L077 ;;line 116;;  const _50 = $50

.L078 ;;line 117;;  const _52 = $52

.L079 ;;line 118;;  const _54 = $54

.L080 ;;line 119;;  const _56 = $56

.L081 ;;line 120;;  const _58 = $58

.L082 ;;line 121;;  const _5A = $5A

.L083 ;;line 122;;  const _5C = $5C

.L084 ;;line 123;;  const _5E = $5E

.L085 ;;line 124;;  const _60 = $60

.L086 ;;line 125;;  const _62 = $62

.L087 ;;line 126;;  const _64 = $64

.L088 ;;line 127;;  const _66 = $66

.L089 ;;line 128;;  const _68 = $68

.L090 ;;line 129;;  const _6A = $6A

.L091 ;;line 130;;  const _6C = $6C

.L092 ;;line 131;;  const _6E = $6E

.L093 ;;line 132;;  const _70 = $70

.L094 ;;line 133;;  const _72 = $72

.L095 ;;line 134;;  const _74 = $74

.L096 ;;line 135;;  const _76 = $76

.L097 ;;line 136;;  const _78 = $78

.L098 ;;line 137;;  const _7A = $7A

.L099 ;;line 138;;  const _7C = $7C

.L0100 ;;line 139;;  const _7E = $7E

.L0101 ;;line 140;;  const _80 = $80

.L0102 ;;line 141;;  const _82 = $82

.L0103 ;;line 142;;  const _84 = $84

.L0104 ;;line 143;;  const _86 = $86

.L0105 ;;line 144;;  const _88 = $88

.L0106 ;;line 145;;  const _8A = $8A

.L0107 ;;line 146;;  const _8C = $8C

.L0108 ;;line 147;;  const _8E = $8E

.L0109 ;;line 148;;  const _90 = $90

.L0110 ;;line 149;;  const _92 = $92

.L0111 ;;line 150;;  const _94 = $94

.L0112 ;;line 151;;  const _96 = $96

.L0113 ;;line 152;;  const _98 = $98

.L0114 ;;line 153;;  const _9A = $9A

.L0115 ;;line 154;;  const _9C = $9C

.L0116 ;;line 155;;  const _9E = $9E

.L0117 ;;line 156;;  const _A0 = $A0

.L0118 ;;line 157;;  const _A2 = $A2

.L0119 ;;line 158;;  const _A4 = $A4

.L0120 ;;line 159;;  const _A6 = $A6

.L0121 ;;line 160;;  const _A8 = $A8

.L0122 ;;line 161;;  const _AA = $AA

.L0123 ;;line 162;;  const _AC = $AC

.L0124 ;;line 163;;  const _AE = $AE

.L0125 ;;line 164;;  const _B0 = $B0

.L0126 ;;line 165;;  const _B2 = $B2

.L0127 ;;line 166;;  const _B4 = $B4

.L0128 ;;line 167;;  const _B6 = $B6

.L0129 ;;line 168;;  const _B8 = $B8

.L0130 ;;line 169;;  const _BA = $BA

.L0131 ;;line 170;;  const _BC = $BC

.L0132 ;;line 171;;  const _BE = $BE

.L0133 ;;line 172;;  const _C0 = $C0

.L0134 ;;line 173;;  const _C2 = $C2

.L0135 ;;line 174;;  const _C4 = $C4

.L0136 ;;line 175;;  const _C6 = $C6

.L0137 ;;line 176;;  const _C8 = $C8

.L0138 ;;line 177;;  const _CA = $CA

.L0139 ;;line 178;;  const _CC = $CC

.L0140 ;;line 179;;  const _CE = $CE

.L0141 ;;line 180;;  const _D0 = $D0

.L0142 ;;line 181;;  const _D2 = $D2

.L0143 ;;line 182;;  const _D4 = $D4

.L0144 ;;line 183;;  const _D6 = $D6

.L0145 ;;line 184;;  const _D8 = $D8

.L0146 ;;line 185;;  const _DA = $DA

.L0147 ;;line 186;;  const _DC = $DC

.L0148 ;;line 187;;  const _DE = $DE

.L0149 ;;line 188;;  const _E0 = $E0

.L0150 ;;line 189;;  const _E2 = $E2

.L0151 ;;line 190;;  const _E4 = $E4

.L0152 ;;line 191;;  const _E6 = $E6

.L0153 ;;line 192;;  const _E8 = $E8

.L0154 ;;line 193;;  const _EA = $EA

.L0155 ;;line 194;;  const _EC = $EC

.L0156 ;;line 195;;  const _EE = $EE

.L0157 ;;line 196;;  const _F0 = $F0

.L0158 ;;line 197;;  const _F2 = $F2

.L0159 ;;line 198;;  const _F4 = $F4

.L0160 ;;line 199;;  const _F6 = $F6

.L0161 ;;line 200;;  const _F8 = $F8

.L0162 ;;line 201;;  const _FA = $FA

.L0163 ;;line 202;;  const _FC = $FC

.L0164 ;;line 203;;  const _FE = $FE

.
 ;;line 204;; 

.
 ;;line 205;; 

.
 ;;line 206;; 

.
 ;;line 207;; 

.
 ;;line 208;; 

.
 ;;line 209;; 

.
 ;;line 210;; 

.
 ;;line 211;; 

.
 ;;line 212;; 

.L0165 ;;line 213;;  const _P_Edge_Top = 0

.L0166 ;;line 214;;  const _P_Edge_Bottom = 170

.L0167 ;;line 215;;  const _P_Edge_Left = 1

.L0168 ;;line 216;;  const _P_Edge_Right = 153

.L0169 ;;line 217;;  const _Shots_Per_Round = 10

.L0170 ;;line 218;;  const _Min_Hits_To_Pass = 7

.L0171 ;;line 219;;  const _Ammo_Row = 170

.
 ;;line 220;; 

.
 ;;line 221;; 

.L0172 ;;line 222;;  _Bit1_Bird_Falling{1} = 0

	LDA _Bit1_Bird_Falling
	AND #253
	STA _Bit1_Bird_Falling
.
 ;;line 223;; 

.L0173 ;;line 224;;  _Bit0_Bird_Dead{0} = 0

	LDA _Bit0_Bird_Dead
	AND #254
	STA _Bit0_Bird_Dead
.
 ;;line 225;; 

.
 ;;line 226;; 

.
 ;;line 227;; 

.
 ;;line 228;; 

.
 ;;line 229;; 

.
 ;;line 230;; 

.
 ;;line 231;; 

.L0174 ;;line 232;;  _Bit7_Splash_Seen{7} = 0

	LDA _Bit7_Splash_Seen
	AND #127
	STA _Bit7_Splash_Seen
.L0175 ;;line 233;;  goto __Bank_2 bank2

 sta temp7
 lda #>(.__Bank_2-1)
 pha
 lda #<(.__Bank_2-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #2
 jmp BS_jsr
.
 ;;line 234;; 

.
 ;;line 235;; 

.
 ;;line 236;; 

.L0176 ;;line 237;;  bank 2

 if ECHO1
 echo "    ",[(start_bank1 - *)]d , "bytes of ROM space left in bank 1")
 endif
ECHO1 = 1
 ORG $1FF4-bscode_length
 RORG $1FF4-bscode_length
start_bank1 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $1FFC
 RORG $1FFC
 .word (start_bank1 & $ffff)
 .word (start_bank1 & $ffff)
 ORG $2000
 RORG $3000
HMdiv
  .byte 0, 0, 0, 0, 0, 0, 0
  .byte 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2
  .byte 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
  .byte 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4
  .byte 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5
  .byte 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6
  .byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7
  .byte 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8
  .byte 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9
  .byte 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10
  .byte 10,10,10,10,10,10,0,0,0
.L0177 ;;line 238;;  temp1 = temp1

	LDA temp1
	STA temp1
.
 ;;line 239;; 

.
 ;;line 240;; 

.
 ;;line 241;; 

.__Bank_2
 ;;line 242;; __Bank_2

.
 ;;line 243;; 

.
 ;;line 244;; 

.__Start_Restart
 ;;line 245;; __Start_Restart

.
 ;;line 246;; 

.
 ;;line 247;; 

.
 ;;line 248;; 

.
 ;;line 249;; 

.
 ;;line 250;; 

.
 ;;line 251;; 

.L0178 ;;line 252;;  AUDV0 = 0  :  AUDV1 = 0

	LDA #0
	STA AUDV0
	STA AUDV1
.
 ;;line 253;; 

.
 ;;line 254;; 

.
 ;;line 255;; 

.
 ;;line 256;; 

.
 ;;line 257;; 

.
 ;;line 258;; 

.
 ;;line 259;; 

.
 ;;line 260;; 

.L0179 ;;line 261;;  a = 0  :  b = 0  :  c = 0  :  d = 0  :  e = 0  :  f = 0  :  g = 0  :  h = 0  :  i = 0

	LDA #0
	STA a
	STA b
	STA c
	STA d
	STA e
	STA f
	STA g
	STA h
	STA i
.L0180 ;;line 262;;  j = 0  :  k = 0  :  l = 0  :  m = 0  :  n = 0  :  o = 0  :  p = 0  :  q = 0  :  r = 0

	LDA #0
	STA j
	STA k
	STA l
	STA m
	STA n
	STA o
	STA p
	STA q
	STA r
.L0181 ;;line 263;;  s = 0  :  t = 0  :  u = 0  :  v = 0  :  w = 0  :  x = 0  :  y = 0

	LDA #0
	STA s
	STA t
	STA u
	STA v
	STA w
	STA x
	STA y
.L0182 ;;line 264;;  var0 = 0  :  var1 = 0  :  var2 = 0  :  var3 = 0  :  var4 = 0

	LDA #0
	STA var0
	STA var1
	STA var2
	STA var3
	STA var4
.L0183 ;;line 265;;  var5 = 0  :  var6 = 0  :  var7 = 0  :  var8 = 0

	LDA #0
	STA var5
	STA var6
	STA var7
	STA var8
.
 ;;line 266;; 

.L0184 ;;line 267;;  _Round = 1

	LDA #1
	STA _Round
.L0185 ;;line 268;;  _Shots_Remaining = _Shots_Per_Round

	LDA #_Shots_Per_Round
	STA _Shots_Remaining
.L0186 ;;line 269;;  _Shots_Fired = 0

	LDA #0
	STA _Shots_Fired
.L0187 ;;line 270;;  _Round_Hits = 0

	LDA #0
	STA _Round_Hits
.L0188 ;;line 271;;  _Game_Over = 0

	LDA #0
	STA _Game_Over
.L0189 ;;line 272;;  _Just_Started = 1

	LDA #1
	STA _Just_Started
.L0190 ;;line 273;;  score = 0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ;;line 274;; 

.L0191 ;;line 275;;  _Splash_Active = 1

	LDA #1
	STA _Splash_Active
.L0192 ;;line 276;;  if _Bit7_Splash_Seen{7} then _Splash_Active = 0

	BIT _Bit7_Splash_Seen
	BPL .skipL0192
.condpart0
	LDA #0
	STA _Splash_Active
.skipL0192
.
 ;;line 277;; 

.
 ;;line 278;; 

.
 ;;line 279;; 

.
 ;;line 280;; 

.
 ;;line 281;; 

.
 ;;line 282;; 

.L0193 ;;line 283;;  player1x = 0  :  player1y = 90

	LDA #0
	STA player1x
	LDA #90
	STA player1y
.L0194 ;;line 284;;  _P1_L_R = 0  :  _P1_U_D = 90

	LDA #0
	STA _P1_L_R
	LDA #90
	STA _P1_U_D
.
 ;;line 285;; 

.
 ;;line 286;; 

.
 ;;line 287;; 

.
 ;;line 288;; 

.
 ;;line 289;; 

.L0195 ;;line 290;;  player0x = 45  :  player0y = 53

	LDA #45
	STA player0x
	LDA #53
	STA player0y
.
 ;;line 291;; 

.
 ;;line 292;; 

.
 ;;line 293;; 

.
 ;;line 294;; 

.L0196 ;;line 295;;  missile0height = 2

	LDA #2
	STA missile0height
.
 ;;line 296;; 

.
 ;;line 297;; 

.
 ;;line 298;; 

.
 ;;line 299;; 

.
 ;;line 300;; 

.L0197 ;;line 301;;  COLUPF = $3A

	LDA #$3A
	STA COLUPF
.
 ;;line 302;; 

.
 ;;line 303;; 

.
 ;;line 304;; 

.
 ;;line 305;; 

.
 ;;line 306;; 

.
 ;;line 307;; 

.L0198 ;;line 308;;  COLUBK = $84

	LDA #$84
	STA COLUBK
.
 ;;line 309;; 

.
 ;;line 310;; 

.
 ;;line 311;; 

.
 ;;line 312;; 

.
 ;;line 313;; 

.
 ;;line 314;; 

.
 ;;line 315;; 

.
 ;;line 316;; 

.
 ;;line 317;; 

.L0199 ;;line 318;;  _Bit0_Reset_Restrainer{0} = 1

	LDA _Bit0_Reset_Restrainer
	ORA #1
	STA _Bit0_Reset_Restrainer
.
 ;;line 319;; 

.
 ;;line 320;; 

.
 ;;line 321;; 

.
 ;;line 322;; 

.
 ;;line 323;; 

.
 ;;line 324;; 

.L0200 ;;line 325;;  player0:

	LDX #<playerL0200_0
	STX player0pointerlo
	LDA #((>playerL0200_0) & $0f) | (((>playerL0200_0) / 2) & $70)
	STA player0pointerhi
	LDA #10
	STA player0height
.
 ;;line 337;; 

.
 ;;line 338;; 

.
 ;;line 339;; 

.
 ;;line 340;; 

.
 ;;line 341;; 

.
 ;;line 342;; 

.L0201 ;;line 343;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0201_1
	STX DF0WRITE
	LDA #((>playerL0201_1) & $0f) | (((>playerL0201_1) / 2) & $70)
	STA DF0WRITE
	LDA #11
	STA player1height
.
 ;;line 356;; 

.L0202 ;;line 357;;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL0202_1
	STX DF0WRITE
	LDA #((>playercolorL0202_1) & $0f) | (((>playercolorL0202_1) / 2) & $70)
	STA DF0WRITE
.
 ;;line 370;; 

.
 ;;line 371;; 

.
 ;;line 372;; 

.
 ;;line 373;; 

.
 ;;line 374;; 

.
 ;;line 375;; 

.L0203 ;;line 376;;  playfield:

 ldy #176
	LDA #<PF_data1
	LDX #((>PF_data1) & $0f) | (((>PF_data1) / 2) & $70)
 sta temp7
 lda #>(ret_point1-1)
 pha
 lda #<(ret_point1-1)
 pha
 lda #>(pfsetup-1)
 pha
 lda #<(pfsetup-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #1
 jmp BS_jsr
ret_point1
.
 ;;line 554;; 

.L0204 ;;line 555;;  pfcolors:

	LDA #<PFCOLS
	STA DF0LOW
	LDA #(>PFCOLS) & $0F
	STA DF0HI
	LDA #<playfieldcolorL0204
	STA PARAMETER
	LDA #((>playfieldcolorL0204) & $0f) | (((>playfieldcolorL0204) / 2) & $70)
	STA PARAMETER
	LDA #0
	STA PARAMETER
	LDA #88
	STA PARAMETER
	LDA #1
	STA CALLFUNCTION
.
 ;;line 645;; 

.L0205 ;;line 646;;  bkcolors:

	LDA #<BKCOLS
	STA DF0LOW
	LDA #(>BKCOLS) & $0F
	STA DF0HI
	LDA #<backgroundcolorL0205
	STA PARAMETER
	LDA #((>backgroundcolorL0205) & $0f) | (((>backgroundcolorL0205) / 2) & $70)
	STA PARAMETER
	LDA #0
	STA PARAMETER
	LDA #88
	STA PARAMETER
	LDA #1
	STA CALLFUNCTION
.
 ;;line 736;; 

.
 ;;line 737;; 

.
 ;;line 738;; 

.
 ;;line 739;; 

.
 ;;line 740;; 

.L0206 ;;line 741;;  scorecolor = _F8

	LDA #_F8
	STA scorecolor
.
 ;;line 742;; 

.
 ;;line 743;; 

.
 ;;line 744;; 

.
 ;;line 745;; 

.
 ;;line 746;; 

.
 ;;line 747;; 

.
 ;;line 748;; 

.__Main_Loop
 ;;line 749;; __Main_Loop

.
 ;;line 750;; 

.L0207 ;;line 751;;  scorecolor = _F8

	LDA #_F8
	STA scorecolor
.
 ;;line 752;; 

.
 ;;line 753;; 

.
 ;;line 754;; 

.
 ;;line 755;; 

.
 ;;line 756;; 

.L0208 ;;line 757;;  COLUP0 = 0

	LDA #0
	STA COLUP0
.L0209 ;;line 758;;  COLUP1 = $C8

	LDA #$C8
	STA COLUP1
.
 ;;line 759;; 

.
 ;;line 760;; 

.
 ;;line 761;; 

.L0210 ;;line 762;;  NUSIZ0 = $20

	LDA #$20
	STA NUSIZ0
.
 ;;line 763;; 

.
 ;;line 764;; 

.
 ;;line 765;; 

.
 ;;line 766;; 

.
 ;;line 767;; 

.L0211 ;;line 768;;  if _Splash_Active then goto __Splash_Screen

	LDA _Splash_Active
	BEQ .skipL0211
.condpart1
 jmp .__Splash_Screen

.skipL0211
.
 ;;line 769;; 

.L0212 ;;line 770;;  if _Game_Over then goto __Game_Over

	LDA _Game_Over
	BEQ .skipL0212
.condpart2
 jmp .__Game_Over

.skipL0212
.L0213 ;;line 771;;  if _Shots_Remaining = 0  &&  !_Bit0_Bird_Dead{0}  &&  !_Bit1_Bird_Falling{0}  &&  !_Bit2_Dog_Show{2} then goto __Round_End_Check

	LDA _Shots_Remaining
	CMP #0
     BNE .skipL0213
.condpart3
	LDA _Bit0_Bird_Dead
	LSR
	BCS .skip3then
.condpart4
	LDA _Bit1_Bird_Falling
	LSR
	BCS .skip4then
.condpart5
	LDA _Bit2_Dog_Show
	AND #4
	BNE .skip5then
.condpart6
 jmp .__Round_End_Check

.skip5then
.skip4then
.skip3then
.skipL0213
.
 ;;line 772;; 

.L0214 ;;line 773;;  if _Just_Started then goto __Just_Started_Check

	LDA _Just_Started
	BEQ .skipL0214
.condpart7
 jmp .__Just_Started_Check

.skipL0214
.
 ;;line 774;; 

.__Fire_Button_Check
 ;;line 775;; __Fire_Button_Check

.
 ;;line 776;; 

.
 ;;line 777;; 

.
 ;;line 778;; 

.
 ;;line 779;; 

.
 ;;line 780;; 

.
 ;;line 781;; 

.
 ;;line 782;; 

.
 ;;line 783;; 

.
 ;;line 784;; 

.L0215 ;;line 785;;  if !joy0fire then AUDV0 = 0  :  _Bit1_FireB_Restrainer{1} = 0  :  goto __Skip_Joy0_Fire

 bit INPT4
	BPL .skipL0215
.condpart8
	LDA #0
	STA AUDV0
	LDA _Bit1_FireB_Restrainer
	AND #253
	STA _Bit1_FireB_Restrainer
 jmp .__Skip_Joy0_Fire

.skipL0215
.
 ;;line 786;; 

.
 ;;line 787;; 

.
 ;;line 788;; 

.
 ;;line 789;; 

.
 ;;line 790;; 

.L0216 ;;line 791;;  if _Bit1_FireB_Restrainer{1} then goto __Skip_Joy0_Fire

	LDA _Bit1_FireB_Restrainer
	AND #2
	BEQ .skipL0216
.condpart9
 jmp .__Skip_Joy0_Fire

.skipL0216
.
 ;;line 792;; 

.
 ;;line 793;; 

.
 ;;line 794;; 

.
 ;;line 795;; 

.
 ;;line 796;; 

.L0217 ;;line 797;;  _Bit1_FireB_Restrainer{1} = 1

	LDA _Bit1_FireB_Restrainer
	ORA #2
	STA _Bit1_FireB_Restrainer
.
 ;;line 798;; 

.L0218 ;;line 799;;  if _Shots_Remaining = 0 then goto __Skip_Joy0_Fire

	LDA _Shots_Remaining
	CMP #0
     BNE .skipL0218
.condpart10
 jmp .__Skip_Joy0_Fire

.skipL0218
.L0219 ;;line 800;;  _Shots_Remaining = _Shots_Remaining  -  1

	DEC _Shots_Remaining
.L0220 ;;line 801;;  _Shots_Fired = _Shots_Fired  +  1

	INC _Shots_Fired
.
 ;;line 802;; 

.L0221 ;;line 803;;  AUDC0 = 8  :  AUDF0 = 10  :  AUDV0 = 8

	LDA #8
	STA AUDC0
	LDA #10
	STA AUDF0
	LDA #8
	STA AUDV0
.
 ;;line 804;; 

.
 ;;line 805;; 

.
 ;;line 806;; 

.L0222 ;;line 807;;  missile0x =  ( player0x  +  3 ) 

; complex statement detected
	LDA player0x
	CLC
	ADC #3
	STA missile0x
.L0223 ;;line 808;;  missile0y =  ( player0y  +  3 ) 

; complex statement detected
	LDA player0y
	CLC
	ADC #3
	STA missile0y
.
 ;;line 809;; 

.
 ;;line 810;; 

.L0224 ;;line 811;;  if collision(player1,missile0) then _Bit0_Bird_Dead{0} = 1  :  goto __dead_bird

	bit 	CXM0P
	BPL .skipL0224
.condpart11
	LDA _Bit0_Bird_Dead
	ORA #1
	STA _Bit0_Bird_Dead
 jmp .__dead_bird

.skipL0224
.
 ;;line 812;; 

.L0225 ;;line 813;;  if _Bit0_Bird_Dead{0} then goto __Skip_Hitbox

	LDA _Bit0_Bird_Dead
	LSR
	BCC .skipL0225
.condpart12
 jmp .__Skip_Hitbox

.skipL0225
.L0226 ;;line 814;;  if player1x  <  missile0x  -  4 then goto __Skip_Hitbox

; complex condition detected
	LDA missile0x
	SEC
	SBC #4
  PHA
  TSX
  PLA
	LDA player1x
	CMP  1,x
     BCS .skipL0226
.condpart13
 jmp .__Skip_Hitbox

.skipL0226
.L0227 ;;line 815;;  if player1x  >  missile0x  +  4 then goto __Skip_Hitbox

; complex condition detected
	LDA missile0x
	CLC
	ADC #4
	CMP player1x
     BCS .skipL0227
.condpart14
 jmp .__Skip_Hitbox

.skipL0227
.L0228 ;;line 816;;  if player1y  <  missile0y  -  4 then goto __Skip_Hitbox

; complex condition detected
	LDA missile0y
	SEC
	SBC #4
  PHA
  TSX
  PLA
	LDA player1y
	CMP  1,x
     BCS .skipL0228
.condpart15
 jmp .__Skip_Hitbox

.skipL0228
.L0229 ;;line 817;;  if player1y  >  missile0y  +  4 then goto __Skip_Hitbox

; complex condition detected
	LDA missile0y
	CLC
	ADC #4
	CMP player1y
     BCS .skipL0229
.condpart16
 jmp .__Skip_Hitbox

.skipL0229
.
 ;;line 818;; 

.L0230 ;;line 819;;  _Bit0_Bird_Dead{0} = 1  :  goto __dead_bird

	LDA _Bit0_Bird_Dead
	ORA #1
	STA _Bit0_Bird_Dead
 jmp .__dead_bird

.
 ;;line 820;; 

.__Skip_Hitbox
 ;;line 821;; __Skip_Hitbox

.
 ;;line 822;; 

.L0231 ;;line 823;;  _bulletcounter = 2

	LDA #2
	STA _bulletcounter
.
 ;;line 824;; 

.
 ;;line 825;; 

.
 ;;line 826;; 

.__Skip_Joy0_Fire
 ;;line 827;; __Skip_Joy0_Fire

.
 ;;line 828;; 

.
 ;;line 829;; 

.L0232 ;;line 830;;  if _bulletcounter  >  0 then _bulletcounter = _bulletcounter  -  1

	LDA #0
	CMP _bulletcounter
     BCS .skipL0232
.condpart17
	DEC _bulletcounter
.skipL0232
.
 ;;line 831;; 

.L0233 ;;line 832;;  if _bulletcounter = 0 then missile0x = 160  :  missile0y = 200

	LDA _bulletcounter
	CMP #0
     BNE .skipL0233
.condpart18
	LDA #160
	STA missile0x
	LDA #200
	STA missile0y
.skipL0233
.
 ;;line 833;; 

.L0234 ;;line 834;;  goto __After_Fire_Check

 jmp .__After_Fire_Check

.
 ;;line 835;; 

.__Just_Started_Check
 ;;line 836;; __Just_Started_Check

.
 ;;line 837;; 

.L0235 ;;line 838;;  if joy0fire then _Bit1_FireB_Restrainer{1} = 1  :  goto __Skip_Joy0_Fire

 bit INPT4
	BMI .skipL0235
.condpart19
	LDA _Bit1_FireB_Restrainer
	ORA #2
	STA _Bit1_FireB_Restrainer
 jmp .__Skip_Joy0_Fire

.skipL0235
.L0236 ;;line 839;;  _Just_Started = 0

	LDA #0
	STA _Just_Started
.L0237 ;;line 840;;  goto __Fire_Button_Check

 jmp .__Fire_Button_Check

.
 ;;line 841;; 

.__After_Fire_Check
 ;;line 842;; __After_Fire_Check

.
 ;;line 843;; 

.
 ;;line 844;; 

.
 ;;line 845;; 

.
 ;;line 846;; 

.
 ;;line 847;; 

.
 ;;line 848;; 

.
 ;;line 849;; 

.
 ;;line 850;; 

.L0238 ;;line 851;;  if !joy0up then goto __Skip_Joy0_Up

 lda #$10
 bit SWCHA
	BEQ .skipL0238
.condpart20
 jmp .__Skip_Joy0_Up

.skipL0238
.
 ;;line 852;; 

.
 ;;line 853;; 

.
 ;;line 854;; 

.
 ;;line 855;; 

.L0239 ;;line 856;;  if _P0_U_D  <=  _P_Edge_Top then goto __Skip_Joy0_Up

	LDA #_P_Edge_Top
	CMP _P0_U_D
     BCC .skipL0239
.condpart21
 jmp .__Skip_Joy0_Up

.skipL0239
.
 ;;line 857;; 

.
 ;;line 858;; 

.
 ;;line 859;; 

.
 ;;line 860;; 

.L0240 ;;line 861;;  _P0_U_D = _P0_U_D  -  1.00

	LDA b
	SEC 
	SBC #0
	STA b
	LDA _P0_U_D
	SBC #1
	STA _P0_U_D
.
 ;;line 862;; 

.
 ;;line 863;; 

.
 ;;line 864;; 

.
 ;;line 865;; 

.L0241 ;;line 866;;  if _P0_U_D  <=  _P_Edge_Top then goto __Skip_Joy0_Up

	LDA #_P_Edge_Top
	CMP _P0_U_D
     BCC .skipL0241
.condpart22
 jmp .__Skip_Joy0_Up

.skipL0241
.
 ;;line 867;; 

.
 ;;line 868;; 

.__Skip_Joy0_Up
 ;;line 869;; __Skip_Joy0_Up

.
 ;;line 870;; 

.
 ;;line 871;; 

.
 ;;line 872;; 

.
 ;;line 873;; 

.
 ;;line 874;; 

.
 ;;line 875;; 

.
 ;;line 876;; 

.
 ;;line 877;; 

.L0242 ;;line 878;;  if !joy0down then goto __Skip_Joy0_Down

 lda #$20
 bit SWCHA
	BEQ .skipL0242
.condpart23
 jmp .__Skip_Joy0_Down

.skipL0242
.
 ;;line 879;; 

.
 ;;line 880;; 

.
 ;;line 881;; 

.
 ;;line 882;; 

.L0243 ;;line 883;;  if _P0_U_D  >=  _P_Edge_Bottom then goto __Skip_Joy0_Down

	LDA _P0_U_D
	CMP #_P_Edge_Bottom
     BCC .skipL0243
.condpart24
 jmp .__Skip_Joy0_Down

.skipL0243
.
 ;;line 884;; 

.
 ;;line 885;; 

.
 ;;line 886;; 

.
 ;;line 887;; 

.L0244 ;;line 888;;  _P0_U_D = _P0_U_D  +  1.00

	LDA b
	CLC 
	ADC #0
	STA b
	LDA _P0_U_D
	ADC #1
	STA _P0_U_D
.
 ;;line 889;; 

.
 ;;line 890;; 

.
 ;;line 891;; 

.
 ;;line 892;; 

.L0245 ;;line 893;;  if _P0_U_D  >=  _P_Edge_Bottom then goto __Skip_Joy0_Down

	LDA _P0_U_D
	CMP #_P_Edge_Bottom
     BCC .skipL0245
.condpart25
 jmp .__Skip_Joy0_Down

.skipL0245
.
 ;;line 894;; 

.
 ;;line 895;; 

.__Skip_Joy0_Down
 ;;line 896;; __Skip_Joy0_Down

.
 ;;line 897;; 

.
 ;;line 898;; 

.
 ;;line 899;; 

.
 ;;line 900;; 

.
 ;;line 901;; 

.
 ;;line 902;; 

.
 ;;line 903;; 

.
 ;;line 904;; 

.L0246 ;;line 905;;  if !joy0left then goto __Skip_Joy0_Left

 bit SWCHA
	BVC .skipL0246
.condpart26
 jmp .__Skip_Joy0_Left

.skipL0246
.
 ;;line 906;; 

.
 ;;line 907;; 

.
 ;;line 908;; 

.
 ;;line 909;; 

.L0247 ;;line 910;;  if _P0_L_R  <=  _P_Edge_Left then goto __Skip_Joy0_Left

	LDA #_P_Edge_Left
	CMP _P0_L_R
     BCC .skipL0247
.condpart27
 jmp .__Skip_Joy0_Left

.skipL0247
.
 ;;line 911;; 

.
 ;;line 912;; 

.
 ;;line 913;; 

.
 ;;line 914;; 

.
 ;;line 915;; 

.L0248 ;;line 916;;  _P0_L_R = _P0_L_R  -  1.00

	LDA a
	SEC 
	SBC #0
	STA a
	LDA _P0_L_R
	SBC #1
	STA _P0_L_R
.
 ;;line 917;; 

.
 ;;line 918;; 

.
 ;;line 919;; 

.
 ;;line 920;; 

.L0249 ;;line 921;;  if _P0_L_R  <=  _P_Edge_Left then goto __Skip_Joy0_Left

	LDA #_P_Edge_Left
	CMP _P0_L_R
     BCC .skipL0249
.condpart28
 jmp .__Skip_Joy0_Left

.skipL0249
.
 ;;line 922;; 

.
 ;;line 923;; 

.
 ;;line 924;; 

.
 ;;line 925;; 

.__Skip_Joy0_Left
 ;;line 926;; __Skip_Joy0_Left

.
 ;;line 927;; 

.
 ;;line 928;; 

.
 ;;line 929;; 

.
 ;;line 930;; 

.
 ;;line 931;; 

.
 ;;line 932;; 

.
 ;;line 933;; 

.
 ;;line 934;; 

.L0250 ;;line 935;;  if !joy0right then goto __Skip_Joy0_Right

 bit SWCHA
	BPL .skipL0250
.condpart29
 jmp .__Skip_Joy0_Right

.skipL0250
.
 ;;line 936;; 

.
 ;;line 937;; 

.
 ;;line 938;; 

.
 ;;line 939;; 

.L0251 ;;line 940;;  if _P0_L_R  >=  _P_Edge_Right then goto __Skip_Joy0_Right

	LDA _P0_L_R
	CMP #_P_Edge_Right
     BCC .skipL0251
.condpart30
 jmp .__Skip_Joy0_Right

.skipL0251
.
 ;;line 941;; 

.
 ;;line 942;; 

.
 ;;line 943;; 

.
 ;;line 944;; 

.
 ;;line 945;; 

.
 ;;line 946;; 

.
 ;;line 947;; 

.L0252 ;;line 948;;  _P0_L_R = _P0_L_R  +  1.00

	LDA a
	CLC 
	ADC #0
	STA a
	LDA _P0_L_R
	ADC #1
	STA _P0_L_R
.
 ;;line 949;; 

.
 ;;line 950;; 

.
 ;;line 951;; 

.
 ;;line 952;; 

.L0253 ;;line 953;;  if _P0_L_R  >=  _P_Edge_Right then goto __Skip_Joy0_Right

	LDA _P0_L_R
	CMP #_P_Edge_Right
     BCC .skipL0253
.condpart31
 jmp .__Skip_Joy0_Right

.skipL0253
.
 ;;line 954;; 

.__Skip_Joy0_Right
 ;;line 955;; __Skip_Joy0_Right

.
 ;;line 956;; 

.
 ;;line 957;; 

.
 ;;line 958;; 

.
 ;;line 959;; 

.L0254 ;;line 960;;  if _Bit2_Dog_Show{2} then goto __dog_show

	LDA _Bit2_Dog_Show
	AND #4
	BEQ .skipL0254
.condpart32
 jmp .__dog_show

.skipL0254
.
 ;;line 961;; 

.
 ;;line 962;; 

.
 ;;line 963;; 

.
 ;;line 964;; 

.
 ;;line 965;; 

.L0255 ;;line 966;;  if _P1_L_R  >  _P_Edge_Right then goto __Skip_Flight

	LDA #_P_Edge_Right
	CMP _P1_L_R
     BCS .skipL0255
.condpart33
 jmp .__Skip_Flight

.skipL0255
.L0256 ;;line 967;;  if _P1_L_R  <  _P_Edge_Left then goto __Skip_Flight

	LDA _P1_L_R
	CMP #_P_Edge_Left
     BCS .skipL0256
.condpart34
 jmp .__Skip_Flight

.skipL0256
.
 ;;line 968;; 

.
 ;;line 969;; 

.
 ;;line 970;; 

.
 ;;line 971;; 

.
 ;;line 972;; 

.
 ;;line 973;; 

.
 ;;line 974;; 

.L0257 ;;line 975;;  _Master_Counter = _Master_Counter  +  1

	INC _Master_Counter
.
 ;;line 976;; 

.L0258 ;;line 977;;  if _Master_Counter  <  4 then goto __Skip_Frame_Counter

	LDA _Master_Counter
	CMP #4
     BCS .skipL0258
.condpart35
 jmp .__Skip_Frame_Counter

.skipL0258
.
 ;;line 978;; 

.L0259 ;;line 979;;  _Frame_Counter = _Frame_Counter  +  1  :  _Master_Counter = 0

	INC _Frame_Counter
	LDA #0
	STA _Master_Counter
.
 ;;line 980;; 

.L0260 ;;line 981;;  if _Frame_Counter = 4 then _Frame_Counter = 0

	LDA _Frame_Counter
	CMP #4
     BNE .skipL0260
.condpart36
	LDA #0
	STA _Frame_Counter
.skipL0260
.
 ;;line 982;; 

.__Skip_Frame_Counter
 ;;line 983;; __Skip_Frame_Counter

.
 ;;line 984;; 

.
 ;;line 985;; 

.
 ;;line 986;; 

.
 ;;line 987;; 

.L0261 ;;line 988;;  if !_Bit0_Bird_Dead{0} then on _Frame_Counter goto __Frame0 __Frame1 __Frame1 __Frame2

	LDA _Bit0_Bird_Dead
	LSR
	BCS .skipL0261
.condpart37
	LDX _Frame_Counter
	LDA .37thenjumptablehi,x
	PHA
	LDA .37thenjumptablelo,x
	PHA
	RTS
.37thenjumptablehi
	.byte >(.__Frame0-1)
	.byte >(.__Frame1-1)
	.byte >(.__Frame1-1)
	.byte >(.__Frame2-1)
.37thenjumptablelo
	.byte <(.__Frame0-1)
	.byte <(.__Frame1-1)
	.byte <(.__Frame1-1)
	.byte <(.__Frame2-1)
.skipL0261
.L0262 ;;line 989;;  if _Bit0_Bird_Dead{0} then on _Frame_Counter goto __Frame_Dead_1 __Frame_Dead_1 __Frame_Dead_2 __Frame_Dead_2

	LDA _Bit0_Bird_Dead
	LSR
	BCC .skipL0262
.condpart38
	LDX _Frame_Counter
	LDA .38thenjumptablehi,x
	PHA
	LDA .38thenjumptablelo,x
	PHA
	RTS
.38thenjumptablehi
	.byte >(.__Frame_Dead_1-1)
	.byte >(.__Frame_Dead_1-1)
	.byte >(.__Frame_Dead_2-1)
	.byte >(.__Frame_Dead_2-1)
.38thenjumptablelo
	.byte <(.__Frame_Dead_1-1)
	.byte <(.__Frame_Dead_1-1)
	.byte <(.__Frame_Dead_2-1)
	.byte <(.__Frame_Dead_2-1)
.skipL0262
.
 ;;line 990;; 

.__end_flying
 ;;line 991;; __end_flying

.
 ;;line 992;; 

.L0263 ;;line 993;;  player1height = 16

	LDA #16
	STA player1height
.
 ;;line 994;; 

.
 ;;line 995;; 

.
 ;;line 996;; 

.
 ;;line 997;; 

.L0264 ;;line 998;;  if !_Bit0_Bird_Dead{0} then goto __flying_bird

	LDA _Bit0_Bird_Dead
	LSR
	BCS .skipL0264
.condpart39
 jmp .__flying_bird

.skipL0264
.
 ;;line 999;; 

.L0265 ;;line 1000;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1001;; 

.__dead_bird
 ;;line 1002;; __dead_bird

.
 ;;line 1003;; 

.L0266 ;;line 1004;;  _Bit1_Bird_Falling{0} = 1

	LDA _Bit1_Bird_Falling
	ORA #1
	STA _Bit1_Bird_Falling
.L0267 ;;line 1005;;  _Bit0_Bird_Dead{0} = 1

	LDA _Bit0_Bird_Dead
	ORA #1
	STA _Bit0_Bird_Dead
.L0268 ;;line 1006;;  _Bit2_Dog_Show{2} = 0

	LDA _Bit2_Dog_Show
	AND #251
	STA _Bit2_Dog_Show
.L0269 ;;line 1007;;  _dog_timer = 0

	LDA #0
	STA _dog_timer
.L0270 ;;line 1008;;  _Frame_Counter = 0

	LDA #0
	STA _Frame_Counter
.L0271 ;;line 1009;;  _P0_U_D = _P_Edge_Bottom

	LDA #0
	STA b
	LDA #_P_Edge_Bottom
	STA _P0_U_D
.L0272 ;;line 1010;;  player0y = _P0_U_D

	LDA _P0_U_D
	STA player0y
.L0273 ;;line 1011;;  _Round_Hits = _Round_Hits  +  1

	INC _Round_Hits
.L0274 ;;line 1012;;  score = score  +  1

	SED
	CLC
	LDA score+2
	ADC #$01
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
.L0275 ;;line 1013;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1014;; 

.__dog_show
 ;;line 1015;; __dog_show

.
 ;;line 1016;; 

.L0276 ;;line 1017;;  _dog_timer = _dog_timer  +  1

	INC _dog_timer
.
 ;;line 1018;; 

.L0277 ;;line 1019;;  if _dog_timer  <  16 then _P1_U_D = 84  -  _dog_timer

	LDA _dog_timer
	CMP #16
     BCS .skipL0277
.condpart40
	LDA #84
	SEC
	SBC _dog_timer
	STA _P1_U_D
.skipL0277
.L0278 ;;line 1020;;  if _dog_timer  >=  16 then _P1_U_D = 68

	LDA _dog_timer
	CMP #16
     BCC .skipL0278
.condpart41
	LDA #68
	STA _P1_U_D
.skipL0278
.
 ;;line 1021;; 

.L0279 ;;line 1022;;  player1x = _P1_L_R  :  player1y = _P1_U_D

	LDA _P1_L_R
	STA player1x
	LDA _P1_U_D
	STA player1y
.
 ;;line 1023;; 

.L0280 ;;line 1024;;  if _dog_timer  >=  180 then _Bit2_Dog_Show{2} = 0  :  _bulletcounter = 0  :  missile0x = 160  :  missile0y = 200  :  AUDV0 = 0  :  player0height = 8  :  player0:

	LDA _dog_timer
	CMP #180
     BCC .skipL0280
.condpart42
	LDA _Bit2_Dog_Show
	AND #251
	STA _Bit2_Dog_Show
	LDA #0
	STA _bulletcounter
	LDA #160
	STA missile0x
	LDA #200
	STA missile0y
	LDA #0
	STA AUDV0
	LDA #8
	STA player0height
	LDX #<player42then_0
	STX player0pointerlo
	LDA #((>player42then_0) & $0f) | (((>player42then_0) / 2) & $70)
	STA player0pointerhi
	LDA #8
	STA player0height
.skipL0280
.
 ;;line 1034;; 

.L0281 ;;line 1035;;  goto __Dog_Frame0

 jmp .__Dog_Frame0

.
 ;;line 1036;; 

.
 ;;line 1037;; 

.
 ;;line 1038;; 

.__Skip_Flight
 ;;line 1039;; __Skip_Flight

.
 ;;line 1040;; 

.L0282 ;;line 1041;;  player1x = 200  :  player1y = 200

	LDA #200
	STA player1x
	STA player1y
.L0283 ;;line 1042;;  _P1_L_R = 200  :  _P1_U_D = 200

	LDA #200
	STA _P1_L_R
	STA _P1_U_D
.L0284 ;;line 1043;;  _wait_counter = _wait_counter  +  1

	INC _wait_counter
.
 ;;line 1044;; 

.L0285 ;;line 1045;;  if _wait_counter = 60 then goto __bird_spawn

	LDA _wait_counter
	CMP #60
     BNE .skipL0285
.condpart43
 jmp .__bird_spawn

.skipL0285
.
 ;;line 1046;; 

.L0286 ;;line 1047;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1048;; 

.__bird_spawn
 ;;line 1049;; __bird_spawn

.
 ;;line 1050;; 

.L0287 ;;line 1051;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0287_1
	STX DF0WRITE
	LDA #((>playerL0287_1) & $0f) | (((>playerL0287_1) / 2) & $70)
	STA DF0WRITE
	LDA #11
	STA player1height
.
 ;;line 1064;; 

.L0288 ;;line 1065;;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL0288_1
	STX DF0WRITE
	LDA #((>playercolorL0288_1) & $0f) | (((>playercolorL0288_1) / 2) & $70)
	STA DF0WRITE
.
 ;;line 1078;; 

.L0289 ;;line 1079;;  _Bit0_Bird_Dead{0} = 0

	LDA _Bit0_Bird_Dead
	AND #254
	STA _Bit0_Bird_Dead
.L0290 ;;line 1080;;  _Bit1_Bird_Falling{0} = 0

	LDA _Bit1_Bird_Falling
	AND #254
	STA _Bit1_Bird_Falling
.L0291 ;;line 1081;;  _Bit2_Dog_Show{2} = 0

	LDA _Bit2_Dog_Show
	AND #251
	STA _Bit2_Dog_Show
.L0292 ;;line 1082;;  _wait_counter = 0

	LDA #0
	STA _wait_counter
.L0293 ;;line 1083;;  _flight_pattern = rand  &  3

        lda rand
	AND #3
	STA _flight_pattern
.L0294 ;;line 1084;;  _bird_dir = rand  &  1

        lda rand
	AND #1
	STA _bird_dir
.
 ;;line 1085;; 

.L0295 ;;line 1086;;  if _bird_dir then player1x = _P_Edge_Left  +  2  :  _P1_L_R = _P_Edge_Left  +  2

	LDA _bird_dir
	BEQ .skipL0295
.condpart44
	LDA #_P_Edge_Left
	CLC
	ADC #2
	STA player1x
	LDA #_P_Edge_Left
	CLC
	ADC #2
	STA _P1_L_R
.skipL0295
.L0296 ;;line 1087;;  if !_bird_dir then player1x = _P_Edge_Right  -  2  :  _P1_L_R = _P_Edge_Right  -  2

	LDA _bird_dir
	BNE .skipL0296
.condpart45
	LDA #_P_Edge_Right
	SEC
	SBC #2
	STA player1x
	LDA #_P_Edge_Right
	SEC
	SBC #2
	STA _P1_L_R
.skipL0296
.L0297 ;;line 1088;;  player1y = 90

	LDA #90
	STA player1y
.L0298 ;;line 1089;;  _P1_U_D = 90

	LDA #90
	STA _P1_U_D
.
 ;;line 1090;; 

.
 ;;line 1091;; 

.
 ;;line 1092;; 

.
 ;;line 1093;; 

.
 ;;line 1094;; 

.__clear_missile
 ;;line 1095;; __clear_missile

.
 ;;line 1096;; 

.
 ;;line 1097;; 

.
 ;;line 1098;; 

.L0299 ;;line 1099;;  _bulletcounter = 0

	LDA #0
	STA _bulletcounter
.L0300 ;;line 1100;;  missile0x = 160  :  missile0y = 200

	LDA #160
	STA missile0x
	LDA #200
	STA missile0y
.L0301 ;;line 1101;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1102;; 

.__Round_End_Check
 ;;line 1103;; __Round_End_Check

.
 ;;line 1104;; 

.L0302 ;;line 1105;;  score = _Round_Hits  *  10

	LDA #$047
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.L0303 ;;line 1106;;  AUDV0 = 0  :  AUDV1 = 0

	LDA #0
	STA AUDV0
	STA AUDV1
.
 ;;line 1107;; 

.L0304 ;;line 1108;;  drawscreen

 sta temp7
 lda #>(ret_point2-1)
 pha
 lda #<(ret_point2-1)
 pha
 lda #>(drawscreen-1)
 pha
 lda #<(drawscreen-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #1
 jmp BS_jsr
ret_point2
.
 ;;line 1109;; 

.L0305 ;;line 1110;;  if !joy0fire then _Bit1_FireB_Restrainer{1} = 0  :  goto __Round_End_Check

 bit INPT4
	BPL .skipL0305
.condpart46
	LDA _Bit1_FireB_Restrainer
	AND #253
	STA _Bit1_FireB_Restrainer
 jmp .__Round_End_Check

.skipL0305
.L0306 ;;line 1111;;  if _Bit1_FireB_Restrainer{1} then goto __Round_End_Check

	LDA _Bit1_FireB_Restrainer
	AND #2
	BEQ .skipL0306
.condpart47
 jmp .__Round_End_Check

.skipL0306
.L0307 ;;line 1112;;  _Bit1_FireB_Restrainer{1} = 1

	LDA _Bit1_FireB_Restrainer
	ORA #2
	STA _Bit1_FireB_Restrainer
.
 ;;line 1113;; 

.L0308 ;;line 1114;;  if _Round_Hits  <  _Min_Hits_To_Pass then _Game_Over = 1  :  goto __Game_Over

	LDA _Round_Hits
	CMP #_Min_Hits_To_Pass
     BCS .skipL0308
.condpart48
	LDA #1
	STA _Game_Over
 jmp .__Game_Over

.skipL0308
.
 ;;line 1115;; 

.L0309 ;;line 1116;;  goto __Next_Round

 jmp .__Next_Round

.
 ;;line 1117;; 

.__Next_Round
 ;;line 1118;; __Next_Round

.
 ;;line 1119;; 

.L0310 ;;line 1120;;  _Round = _Round  +  1

	INC _Round
.L0311 ;;line 1121;;  _Shots_Remaining = _Shots_Per_Round

	LDA #_Shots_Per_Round
	STA _Shots_Remaining
.L0312 ;;line 1122;;  _Shots_Fired = 0

	LDA #0
	STA _Shots_Fired
.L0313 ;;line 1123;;  _Round_Hits = 0

	LDA #0
	STA _Round_Hits
.L0314 ;;line 1124;;  score = 0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.L0315 ;;line 1125;;  goto __bird_spawn

 jmp .__bird_spawn

.
 ;;line 1126;; 

.__Game_Over
 ;;line 1127;; __Game_Over

.
 ;;line 1128;; 

.L0316 ;;line 1129;;  AUDV0 = 0  :  AUDV1 = 0

	LDA #0
	STA AUDV0
	STA AUDV1
.L0317 ;;line 1130;;  COLUBK = $46

	LDA #$46
	STA COLUBK
.L0318 ;;line 1131;;  COLUPF = $0E

	LDA #$0E
	STA COLUPF
.L0319 ;;line 1132;;  player0x = 200  :  player0y = 200

	LDA #200
	STA player0x
	STA player0y
.L0320 ;;line 1133;;  player1x = 200  :  player1y = 200

	LDA #200
	STA player1x
	STA player1y
.L0321 ;;line 1134;;  missile0x = 200  :  missile0y = 200

	LDA #200
	STA missile0x
	STA missile0y
.
 ;;line 1135;; 

.L0322 ;;line 1136;;  pfpixel 4 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #4
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0323 ;;line 1137;;  pfpixel 6 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #6
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0324 ;;line 1138;;  pfpixel 8 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #8
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0325 ;;line 1139;;  pfpixel 10 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #10
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0326 ;;line 1140;;  pfpixel 12 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #12
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0327 ;;line 1141;;  pfpixel 14 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #14
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0328 ;;line 1142;;  pfpixel 16 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #16
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0329 ;;line 1143;;  pfpixel 18 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #18
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0330 ;;line 1144;;  pfpixel 20 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #20
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0331 ;;line 1145;;  pfpixel 22 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #22
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.
 ;;line 1146;; 

.L0332 ;;line 1147;;  drawscreen

 sta temp7
 lda #>(ret_point3-1)
 pha
 lda #<(ret_point3-1)
 pha
 lda #>(drawscreen-1)
 pha
 lda #<(drawscreen-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #1
 jmp BS_jsr
ret_point3
.
 ;;line 1148;; 

.L0333 ;;line 1149;;  if !switchreset then goto __Game_Over

 lda #1
 bit SWCHB
	BEQ .skipL0333
.condpart49
 jmp .__Game_Over

.skipL0333
.L0334 ;;line 1150;;  goto __Start_Restart

 jmp .__Start_Restart

.
 ;;line 1151;; 

.__Splash_Screen
 ;;line 1152;; __Splash_Screen

.
 ;;line 1153;; 

.L0335 ;;line 1154;;  scorecolor = _F8

	LDA #_F8
	STA scorecolor
.L0336 ;;line 1155;;  score = 0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ;;line 1156;; 

.
 ;;line 1157;; 

.L0337 ;;line 1158;;  COLUBK = $8C

	LDA #$8C
	STA COLUBK
.L0338 ;;line 1159;;  COLUP0 = $0E

	LDA #$0E
	STA COLUP0
.L0339 ;;line 1160;;  COLUP1 = $0E

	LDA #$0E
	STA COLUP1
.L0340 ;;line 1161;;  COLUPF = $0E

	LDA #$0E
	STA COLUPF
.
 ;;line 1162;; 

.L0341 ;;line 1163;;  missile0height = 4

	LDA #4
	STA missile0height
.L0342 ;;line 1164;;  player1height = 16

	LDA #16
	STA player1height
.L0343 ;;line 1165;;  player0height = 16

	LDA #16
	STA player0height
.L0344 ;;line 1166;;  missile0x = 200  :  missile0y = 200

	LDA #200
	STA missile0x
	STA missile0y
.
 ;;line 1167;; 

.L0345 ;;line 1168;;  _Splash_Blink = _Splash_Blink  +  1

	INC _Splash_Blink
.L0346 ;;line 1169;;  if _Splash_Blink = 1 then _flight_pattern = rand  &  3  :  _bird_dir = rand  &  1  :  _P1_L_R = _P_Edge_Left  +  10  :  _P1_U_D = 50  :  _Splash_P1_Pattern = rand  &  1  :  _Splash_P1_Dir = rand  &  1  :  _Splash_P1_L_R = _P_Edge_Right  -  10  :  _Splash_P1_U_D = 70

	LDA _Splash_Blink
	CMP #1
     BNE .skipL0346
.condpart50
        lda rand
	AND #3
	STA _flight_pattern
        lda rand
	AND #1
	STA _bird_dir
	LDA #_P_Edge_Left
	CLC
	ADC #10
	STA _P1_L_R
	LDA #50
	STA _P1_U_D
        lda rand
	AND #1
	STA _Splash_P1_Pattern
        lda rand
	AND #1
	STA _Splash_P1_Dir
	LDA #_P_Edge_Right
	SEC
	SBC #10
	STA _Splash_P1_L_R
	LDA #70
	STA _Splash_P1_U_D
.skipL0346
.
 ;;line 1170;; 

.L0347 ;;line 1171;;  _Master_Counter = _Master_Counter  +  1

	INC _Master_Counter
.L0348 ;;line 1172;;  if _Master_Counter  <  4 then goto __Splash_Skip_Move

	LDA _Master_Counter
	CMP #4
     BCS .skipL0348
.condpart51
 jmp .__Splash_Skip_Move

.skipL0348
.L0349 ;;line 1173;;  _Frame_Counter = _Frame_Counter  +  1  :  _Master_Counter = 0

	INC _Frame_Counter
	LDA #0
	STA _Master_Counter
.L0350 ;;line 1174;;  if _Frame_Counter = 4 then _Frame_Counter = 0

	LDA _Frame_Counter
	CMP #4
     BNE .skipL0350
.condpart52
	LDA #0
	STA _Frame_Counter
.skipL0350
.
 ;;line 1175;; 

.L0351 ;;line 1176;;  _bird_counter = _bird_counter  +  1

	INC _bird_counter
.L0352 ;;line 1177;;  if _bird_counter = 60 then _bird_counter = 0

	LDA _bird_counter
	CMP #60
     BNE .skipL0352
.condpart53
	LDA #0
	STA _bird_counter
.skipL0352
.
 ;;line 1178;; 

.L0353 ;;line 1179;;  if _bird_dir then _P1_L_R = _P1_L_R  +  1 else _P1_L_R = _P1_L_R  -  1

	LDA _bird_dir
	BEQ .skipL0353
.condpart54
	INC _P1_L_R
 jmp .skipelse0
.skipL0353
	DEC _P1_L_R
.skipelse0
.L0354 ;;line 1180;;  if _flight_pattern = 0  &&  _Frame_Counter = 0 then _P1_U_D = _P1_U_D  -  1

	LDA _flight_pattern
	CMP #0
     BNE .skipL0354
.condpart55
	LDA _Frame_Counter
	CMP #0
     BNE .skip55then
.condpart56
	DEC _P1_U_D
.skip55then
.skipL0354
.L0355 ;;line 1181;;  if _flight_pattern = 1  &&  _bird_counter  <  30  &&   ( _Frame_Counter  &  1 )  then _P1_U_D = _P1_U_D  -  1

	LDA _flight_pattern
	CMP #1
     BNE .skipL0355
.condpart57
	LDA _bird_counter
	CMP #30
     BCS .skip57then
.condpart58
; complex statement detected
	LDA _Frame_Counter
	AND #1
	BEQ .skip58then
.condpart59
	DEC _P1_U_D
.skip58then
.skip57then
.skipL0355
.L0356 ;;line 1182;;  if _flight_pattern = 1  &&  _bird_counter  >=  30  &&   ( _Frame_Counter  &  1 )  then _P1_U_D = _P1_U_D  +  1

	LDA _flight_pattern
	CMP #1
     BNE .skipL0356
.condpart60
	LDA _bird_counter
	CMP #30
     BCC .skip60then
.condpart61
; complex statement detected
	LDA _Frame_Counter
	AND #1
	BEQ .skip61then
.condpart62
	INC _P1_U_D
.skip61then
.skip60then
.skipL0356
.L0357 ;;line 1183;;  if _flight_pattern = 2  &&  _P1_U_D  <  70  &&   ( _Frame_Counter  &  1 )  then _P1_U_D = _P1_U_D  +  1

	LDA _flight_pattern
	CMP #2
     BNE .skipL0357
.condpart63
	LDA _P1_U_D
	CMP #70
     BCS .skip63then
.condpart64
; complex statement detected
	LDA _Frame_Counter
	AND #1
	BEQ .skip64then
.condpart65
	INC _P1_U_D
.skip64then
.skip63then
.skipL0357
.L0358 ;;line 1184;;  if _flight_pattern = 2  &&  _P1_U_D  >  40  &&   ( _Frame_Counter  &  1 )  then _P1_U_D = _P1_U_D  -  1

	LDA _flight_pattern
	CMP #2
     BNE .skipL0358
.condpart66
	LDA #40
	CMP _P1_U_D
     BCS .skip66then
.condpart67
; complex statement detected
	LDA _Frame_Counter
	AND #1
	BEQ .skip67then
.condpart68
	DEC _P1_U_D
.skip67then
.skip66then
.skipL0358
.L0359 ;;line 1185;;  if _flight_pattern = 3  &&   ( rand  &  1 )  then _P1_U_D = _P1_U_D  +  1 else if _flight_pattern = 3 then _P1_U_D = _P1_U_D  -  1

	LDA _flight_pattern
	CMP #3
     BNE .skipL0359
.condpart69
; complex statement detected
        lda rand
	AND #1
	BEQ .skip69then
.condpart70
	INC _P1_U_D
 jmp .skipelse1
.skip69then
.skipL0359
	LDA _flight_pattern
	CMP #3
     BNE .skipelse
.condpart71
	DEC _P1_U_D
.skipelse
.skipelse1
.
 ;;line 1186;; 

.L0360 ;;line 1187;;  if _Splash_P1_Dir then _Splash_P1_L_R = _Splash_P1_L_R  +  1 else _Splash_P1_L_R = _Splash_P1_L_R  -  1

	LDA _Splash_P1_Dir
	BEQ .skipL0360
.condpart72
	INC _Splash_P1_L_R
 jmp .skipelse2
.skipL0360
	DEC _Splash_P1_L_R
.skipelse2
.L0361 ;;line 1188;;  if _Splash_P1_Pattern  &&  _Frame_Counter = 0 then _Splash_P1_U_D = _Splash_P1_U_D  +  1

	LDA _Splash_P1_Pattern
	BEQ .skipL0361
.condpart73
	LDA _Frame_Counter
	CMP #0
     BNE .skip73then
.condpart74
	INC _Splash_P1_U_D
.skip73then
.skipL0361
.
 ;;line 1189;; 

.
 ;;line 1190;; 

.__Splash_Skip_Move
 ;;line 1191;; __Splash_Skip_Move

.
 ;;line 1192;; 

.L0362 ;;line 1193;;  if _P1_L_R  >  _P_Edge_Right then _bird_dir = 0  :  _flight_pattern = rand  &  3  :  _bird_counter = 0

	LDA #_P_Edge_Right
	CMP _P1_L_R
     BCS .skipL0362
.condpart75
	LDA #0
	STA _bird_dir
        lda rand
	AND #3
	STA _flight_pattern
	LDA #0
	STA _bird_counter
.skipL0362
.L0363 ;;line 1194;;  if _P1_L_R  <  _P_Edge_Left then _bird_dir = 1  :  _flight_pattern = rand  &  3  :  _bird_counter = 0

	LDA _P1_L_R
	CMP #_P_Edge_Left
     BCS .skipL0363
.condpart76
	LDA #1
	STA _bird_dir
        lda rand
	AND #3
	STA _flight_pattern
	LDA #0
	STA _bird_counter
.skipL0363
.L0364 ;;line 1195;;  if _Splash_P1_L_R  >  _P_Edge_Right then _Splash_P1_Dir = 0

	LDA #_P_Edge_Right
	CMP _Splash_P1_L_R
     BCS .skipL0364
.condpart77
	LDA #0
	STA _Splash_P1_Dir
.skipL0364
.L0365 ;;line 1196;;  if _Splash_P1_L_R  <  _P_Edge_Left then _Splash_P1_Dir = 1

	LDA _Splash_P1_L_R
	CMP #_P_Edge_Left
     BCS .skipL0365
.condpart78
	LDA #1
	STA _Splash_P1_Dir
.skipL0365
.
 ;;line 1197;; 

.L0366 ;;line 1198;;  player1x = _P1_L_R

	LDA _P1_L_R
	STA player1x
.L0367 ;;line 1199;;  player1y = _P1_U_D

	LDA _P1_U_D
	STA player1y
.
 ;;line 1200;; 

.L0368 ;;line 1201;;  player0x = _Splash_P1_L_R

	LDA _Splash_P1_L_R
	STA player0x
.L0369 ;;line 1202;;  player0y = _Splash_P1_U_D

	LDA _Splash_P1_U_D
	STA player0y
.
 ;;line 1203;; 

.__Splash_Gfx
 ;;line 1204;; __Splash_Gfx

.L0370 ;;line 1205;;  if _Splash_Blink  &  8 then goto __Splash_Bird_Frame2

	LDA _Splash_Blink
	AND #8
     BEQ .skipL0370
.condpart79
 jmp .__Splash_Bird_Frame2

.skipL0370
.
 ;;line 1206;; 

.__Splash_Bird_Frame1
 ;;line 1207;; __Splash_Bird_Frame1

.L0371 ;;line 1208;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0371_1
	STX DF0WRITE
	LDA #((>playerL0371_1) & $0f) | (((>playerL0371_1) / 2) & $70)
	STA DF0WRITE
	LDA #16
	STA player1height
.L0372 ;;line 1226;;  player0:

	LDX #<playerL0372_0
	STX player0pointerlo
	LDA #((>playerL0372_0) & $0f) | (((>playerL0372_0) / 2) & $70)
	STA player0pointerhi
	LDA #16
	STA player0height
.L0373 ;;line 1244;;  goto __Splash_Draw

 jmp .__Splash_Draw

.
 ;;line 1245;; 

.__Splash_Bird_Frame2
 ;;line 1246;; __Splash_Bird_Frame2

.L0374 ;;line 1247;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0374_1
	STX DF0WRITE
	LDA #((>playerL0374_1) & $0f) | (((>playerL0374_1) / 2) & $70)
	STA DF0WRITE
	LDA #16
	STA player1height
.L0375 ;;line 1265;;  player0:

	LDX #<playerL0375_0
	STX player0pointerlo
	LDA #((>playerL0375_0) & $0f) | (((>playerL0375_0) / 2) & $70)
	STA player0pointerhi
	LDA #16
	STA player0height
.
 ;;line 1283;; 

.__Splash_Draw
 ;;line 1284;; __Splash_Draw

.
 ;;line 1285;; 

.L0376 ;;line 1286;;  drawscreen

 sta temp7
 lda #>(ret_point4-1)
 pha
 lda #<(ret_point4-1)
 pha
 lda #>(drawscreen-1)
 pha
 lda #<(drawscreen-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #1
 jmp BS_jsr
ret_point4
.
 ;;line 1287;; 

.L0377 ;;line 1288;;  if joy0fire then _Bit7_Splash_Seen{7} = 1  :  _Bit1_FireB_Restrainer{1} = 1  :  _Splash_Active = 0  :  goto __Restore_Sprites

 bit INPT4
	BMI .skipL0377
.condpart80
	LDA _Bit7_Splash_Seen
	ORA #128
	STA _Bit7_Splash_Seen
	LDA _Bit1_FireB_Restrainer
	ORA #2
	STA _Bit1_FireB_Restrainer
	LDA #0
	STA _Splash_Active
 jmp .__Restore_Sprites

.skipL0377
.
 ;;line 1289;; 

.L0378 ;;line 1290;;  goto __Splash_Screen

 jmp .__Splash_Screen

.
 ;;line 1291;; 

.__Restore_Sprites
 ;;line 1292;; __Restore_Sprites

.
 ;;line 1293;; 

.L0379 ;;line 1294;;  player0:

	LDX #<playerL0379_0
	STX player0pointerlo
	LDA #((>playerL0379_0) & $0f) | (((>playerL0379_0) / 2) & $70)
	STA player0pointerhi
	LDA #8
	STA player0height
.
 ;;line 1304;; 

.L0380 ;;line 1305;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0380_1
	STX DF0WRITE
	LDA #((>playerL0380_1) & $0f) | (((>playerL0380_1) / 2) & $70)
	STA DF0WRITE
	LDA #11
	STA player1height
.
 ;;line 1318;; 

.L0381 ;;line 1319;;  goto __Start_Restart

 jmp .__Start_Restart

.
 ;;line 1320;; 

.__flying_bird
 ;;line 1321;; __flying_bird

.L0382 ;;line 1322;;  _bird_counter = _bird_counter  +  1

	INC _bird_counter
.L0383 ;;line 1323;;  if _bird_counter = 60 then _bird_counter = 0

	LDA _bird_counter
	CMP #60
     BNE .skipL0383
.condpart81
	LDA #0
	STA _bird_counter
.skipL0383
.L0384 ;;line 1324;;  on _flight_pattern goto __pattern0 __pattern1 __pattern2 __pattern3

	LDX _flight_pattern
	LDA .L0384jumptablehi,x
	PHA
	LDA .L0384jumptablelo,x
	PHA
	RTS
.L0384jumptablehi
	.byte >(.__pattern0-1)
	.byte >(.__pattern1-1)
	.byte >(.__pattern2-1)
	.byte >(.__pattern3-1)
.L0384jumptablelo
	.byte <(.__pattern0-1)
	.byte <(.__pattern1-1)
	.byte <(.__pattern2-1)
	.byte <(.__pattern3-1)
.
 ;;line 1325;; 

.__pattern0
 ;;line 1326;; __pattern0

.L0385 ;;line 1327;;  if _bird_dir then _P1_L_R = _P1_L_R  +  1 else _P1_L_R = _P1_L_R  -  1

	LDA _bird_dir
	BEQ .skipL0385
.condpart82
	INC _P1_L_R
 jmp .skipelse3
.skipL0385
	DEC _P1_L_R
.skipelse3
.L0386 ;;line 1328;;  if _bird_counter  &  3 = 0 then _P1_U_D = _P1_U_D  -  1

; complex condition detected
	LDA _bird_counter
	AND #3
	CMP #0
     BNE .skipL0386
.condpart83
	DEC _P1_U_D
.skipL0386
.L0387 ;;line 1329;;  player1x = _P1_L_R  :  player1y = _P1_U_D

	LDA _P1_L_R
	STA player1x
	LDA _P1_U_D
	STA player1y
.L0388 ;;line 1330;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1331;; 

.__pattern1
 ;;line 1332;; __pattern1

.L0389 ;;line 1333;;  if _bird_dir then _P1_L_R = _P1_L_R  +  1 else _P1_L_R = _P1_L_R  -  1

	LDA _bird_dir
	BEQ .skipL0389
.condpart84
	INC _P1_L_R
 jmp .skipelse4
.skipL0389
	DEC _P1_L_R
.skipelse4
.L0390 ;;line 1334;;  if _bird_counter  <  30  &&   ( _bird_counter  &  1 )  then _P1_U_D = _P1_U_D  -  1

	LDA _bird_counter
	CMP #30
     BCS .skipL0390
.condpart85
; complex statement detected
	LDA _bird_counter
	AND #1
	BEQ .skip85then
.condpart86
	DEC _P1_U_D
.skip85then
.skipL0390
.L0391 ;;line 1335;;  if _bird_counter  >=  30  &&   ( _bird_counter  &  1 )  then _P1_U_D = _P1_U_D  +  1

	LDA _bird_counter
	CMP #30
     BCC .skipL0391
.condpart87
; complex statement detected
	LDA _bird_counter
	AND #1
	BEQ .skip87then
.condpart88
	INC _P1_U_D
.skip87then
.skipL0391
.L0392 ;;line 1336;;  player1x = _P1_L_R  :  player1y = _P1_U_D

	LDA _P1_L_R
	STA player1x
	LDA _P1_U_D
	STA player1y
.L0393 ;;line 1337;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1338;; 

.__pattern2
 ;;line 1339;; __pattern2

.L0394 ;;line 1340;;  if _bird_dir then _P1_L_R = _P1_L_R  +  1 else _P1_L_R = _P1_L_R  -  1

	LDA _bird_dir
	BEQ .skipL0394
.condpart89
	INC _P1_L_R
 jmp .skipelse5
.skipL0394
	DEC _P1_L_R
.skipelse5
.L0395 ;;line 1341;;  if _P1_U_D  <  player0y  &&   ( _bird_counter  &  1 )  then _P1_U_D = _P1_U_D  +  1

	LDA _P1_U_D
	CMP player0y
     BCS .skipL0395
.condpart90
; complex statement detected
	LDA _bird_counter
	AND #1
	BEQ .skip90then
.condpart91
	INC _P1_U_D
.skip90then
.skipL0395
.L0396 ;;line 1342;;  if _P1_U_D  >  player0y  &&   ( _bird_counter  &  1 )  then _P1_U_D = _P1_U_D  -  1

	LDA player0y
	CMP _P1_U_D
     BCS .skipL0396
.condpart92
; complex statement detected
	LDA _bird_counter
	AND #1
	BEQ .skip92then
.condpart93
	DEC _P1_U_D
.skip92then
.skipL0396
.L0397 ;;line 1343;;  player1x = _P1_L_R  :  player1y = _P1_U_D

	LDA _P1_L_R
	STA player1x
	LDA _P1_U_D
	STA player1y
.L0398 ;;line 1344;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1345;; 

.__pattern3
 ;;line 1346;; __pattern3

.L0399 ;;line 1347;;  if _bird_dir then _P1_L_R = _P1_L_R  +  1 else _P1_L_R = _P1_L_R  -  1

	LDA _bird_dir
	BEQ .skipL0399
.condpart94
	INC _P1_L_R
 jmp .skipelse6
.skipL0399
	DEC _P1_L_R
.skipelse6
.L0400 ;;line 1348;;  if rand  &  1 then _P1_U_D = _P1_U_D  +  1

	LDA rand
	AND #1
     BEQ .skipL0400
.condpart95
	INC _P1_U_D
.skipL0400
.L0401 ;;line 1349;;  if ! ( rand  &  1 )  then _P1_U_D = _P1_U_D  -  1

; complex statement detected
        lda rand
	AND #1
	BNE .skipL0401
.condpart96
	DEC _P1_U_D
.skipL0401
.L0402 ;;line 1350;;  player1x = _P1_L_R  :  player1y = _P1_U_D

	LDA _P1_L_R
	STA player1x
	LDA _P1_U_D
	STA player1y
.L0403 ;;line 1351;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1352;; 

.
 ;;line 1353;; 

.__falling_bird
 ;;line 1354;; __falling_bird

.
 ;;line 1355;; 

.L0404 ;;line 1356;;  _bird_counter = _bird_counter  +  1

	INC _bird_counter
.L0405 ;;line 1357;;  if _bird_counter  &  1 then _P1_U_D = _P1_U_D  +  1

	LDA _bird_counter
	AND #1
     BEQ .skipL0405
.condpart97
	INC _P1_U_D
.skipL0405
.L0406 ;;line 1358;;  player1x = _P1_L_R  :  player1y = _P1_U_D

	LDA _P1_L_R
	STA player1x
	LDA _P1_U_D
	STA player1y
.L0407 ;;line 1359;;  if _P1_U_D  >=  160 then _Bit1_Bird_Falling{0} = 0  :  _Bit2_Dog_Show{2} = 1  :  _dog_timer = 0  :  _dog_frame = 0  :  _P1_L_R = 76  :  _P1_U_D = 84  :  player1x = _P1_L_R  :  player1y = _P1_U_D  :  goto __exit_flight_sub

	LDA _P1_U_D
	CMP #160
     BCC .skipL0407
.condpart98
	LDA _Bit1_Bird_Falling
	AND #254
	STA _Bit1_Bird_Falling
	LDA _Bit2_Dog_Show
	ORA #4
	STA _Bit2_Dog_Show
	LDA #0
	STA _dog_timer
	STA _dog_frame
	LDA #76
	STA _P1_L_R
	LDA #84
	STA _P1_U_D
	LDA _P1_L_R
	STA player1x
	LDA _P1_U_D
	STA player1y
 jmp .__exit_flight_sub

.skipL0407
.
 ;;line 1360;; 

.L0408 ;;line 1361;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1362;; 

.__exit_flight_sub
 ;;line 1363;; __exit_flight_sub

.
 ;;line 1364;; 

.
 ;;line 1365;; 

.
 ;;line 1366;; 

.
 ;;line 1367;; 

.L0409 ;;line 1368;;  pfpixel 4 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #4
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0410 ;;line 1369;;  pfpixel 6 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #6
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0411 ;;line 1370;;  pfpixel 8 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #8
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0412 ;;line 1371;;  pfpixel 10 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #10
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0413 ;;line 1372;;  pfpixel 12 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #12
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0414 ;;line 1373;;  pfpixel 14 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #14
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0415 ;;line 1374;;  pfpixel 16 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #16
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0416 ;;line 1375;;  pfpixel 18 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #18
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0417 ;;line 1376;;  pfpixel 20 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #20
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.L0418 ;;line 1377;;  pfpixel 22 _Ammo_Row off

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #13
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #22
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.
 ;;line 1378;; 

.L0419 ;;line 1379;;  if _Shots_Remaining  >  0 then pfpixel 4 _Ammo_Row on

	LDA #0
	CMP _Shots_Remaining
     BCS .skipL0419
.condpart99
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #4
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0419
.L0420 ;;line 1380;;  if _Shots_Remaining  >  1 then pfpixel 6 _Ammo_Row on

	LDA #1
	CMP _Shots_Remaining
     BCS .skipL0420
.condpart100
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #6
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0420
.L0421 ;;line 1381;;  if _Shots_Remaining  >  2 then pfpixel 8 _Ammo_Row on

	LDA #2
	CMP _Shots_Remaining
     BCS .skipL0421
.condpart101
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #8
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0421
.L0422 ;;line 1382;;  if _Shots_Remaining  >  3 then pfpixel 10 _Ammo_Row on

	LDA #3
	CMP _Shots_Remaining
     BCS .skipL0422
.condpart102
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #10
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0422
.L0423 ;;line 1383;;  if _Shots_Remaining  >  4 then pfpixel 12 _Ammo_Row on

	LDA #4
	CMP _Shots_Remaining
     BCS .skipL0423
.condpart103
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #12
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0423
.L0424 ;;line 1384;;  if _Shots_Remaining  >  5 then pfpixel 14 _Ammo_Row on

	LDA #5
	CMP _Shots_Remaining
     BCS .skipL0424
.condpart104
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #14
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0424
.L0425 ;;line 1385;;  if _Shots_Remaining  >  6 then pfpixel 16 _Ammo_Row on

	LDA #6
	CMP _Shots_Remaining
     BCS .skipL0425
.condpart105
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #16
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0425
.L0426 ;;line 1386;;  if _Shots_Remaining  >  7 then pfpixel 18 _Ammo_Row on

	LDA #7
	CMP _Shots_Remaining
     BCS .skipL0426
.condpart106
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #18
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0426
.L0427 ;;line 1387;;  if _Shots_Remaining  >  8 then pfpixel 20 _Ammo_Row on

	LDA #8
	CMP _Shots_Remaining
     BCS .skipL0427
.condpart107
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #20
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0427
.L0428 ;;line 1388;;  if _Shots_Remaining  >  9 then pfpixel 22 _Ammo_Row on

	LDA #9
	CMP _Shots_Remaining
     BCS .skipL0428
.condpart108
	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
	LDX #12
	STX DF0WRITE
	STX DF0WRITE
	LDY #_Ammo_Row
	STY DF0WRITE
	LDA #22
	STA DF0WRITE
	lda #255
	sta CALLFUNCTION
.skipL0428
.
 ;;line 1389;; 

.
 ;;line 1390;; 

.
 ;;line 1391;; 

.
 ;;line 1392;; 

.
 ;;line 1393;; 

.L0429 ;;line 1394;;  DF6FRACINC = 255

	LDA #255
	STA DF6FRACINC
.L0430 ;;line 1395;;  DF4FRACINC = 255

	LDA #255
	STA DF4FRACINC
.
 ;;line 1396;; 

.L0431 ;;line 1397;;  DF0FRACINC = 255

	LDA #255
	STA DF0FRACINC
.L0432 ;;line 1398;;  DF1FRACINC = 255

	LDA #255
	STA DF1FRACINC
.L0433 ;;line 1399;;  DF2FRACINC = 255

	LDA #255
	STA DF2FRACINC
.L0434 ;;line 1400;;  DF3FRACINC = 255

	LDA #255
	STA DF3FRACINC
.
 ;;line 1401;; 

.L0435 ;;line 1402;;  drawscreen

 sta temp7
 lda #>(ret_point5-1)
 pha
 lda #<(ret_point5-1)
 pha
 lda #>(drawscreen-1)
 pha
 lda #<(drawscreen-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #1
 jmp BS_jsr
ret_point5
.
 ;;line 1403;; 

.
 ;;line 1404;; 

.
 ;;line 1405;; 

.
 ;;line 1406;; 

.
 ;;line 1407;; 

.
 ;;line 1408;; 

.
 ;;line 1409;; 

.
 ;;line 1410;; 

.
 ;;line 1411;; 

.
 ;;line 1412;; 

.
 ;;line 1413;; 

.
 ;;line 1414;; 

.
 ;;line 1415;; 

.
 ;;line 1416;; 

.L0436 ;;line 1417;;  if !switchreset then _Bit0_Reset_Restrainer{0} = 0  :  goto __Main_Loop

 lda #1
 bit SWCHB
	BEQ .skipL0436
.condpart109
	LDA _Bit0_Reset_Restrainer
	AND #254
	STA _Bit0_Reset_Restrainer
 jmp .__Main_Loop

.skipL0436
.
 ;;line 1418;; 

.
 ;;line 1419;; 

.
 ;;line 1420;; 

.
 ;;line 1421;; 

.
 ;;line 1422;; 

.L0437 ;;line 1423;;  if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

	LDA _Bit0_Reset_Restrainer
	LSR
	BCC .skipL0437
.condpart110
 jmp .__Main_Loop

.skipL0437
.
 ;;line 1424;; 

.
 ;;line 1425;; 

.
 ;;line 1426;; 

.
 ;;line 1427;; 

.L0438 ;;line 1428;;  goto __Start_Restart

 jmp .__Start_Restart

.
 ;;line 1429;; 

.
 ;;line 1430;; 

.__Frame0
 ;;line 1431;; __Frame0

.
 ;;line 1432;; 

.L0439 ;;line 1433;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0439_1
	STX DF0WRITE
	LDA #((>playerL0439_1) & $0f) | (((>playerL0439_1) / 2) & $70)
	STA DF0WRITE
	LDA #17
	STA player1height
.L0440 ;;line 1452;;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL0440_1
	STX DF0WRITE
	LDA #((>playercolorL0440_1) & $0f) | (((>playercolorL0440_1) / 2) & $70)
	STA DF0WRITE
.L0441 ;;line 1470;;  goto __end_flying

 jmp .__end_flying

.
 ;;line 1471;; 

.__Dog_Frame0
 ;;line 1472;; __Dog_Frame0

.L0442 ;;line 1473;;  player0height = 8

	LDA #8
	STA player0height
.L0443 ;;line 1474;;  player1height = 8

	LDA #8
	STA player1height
.L0444 ;;line 1475;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0444_1
	STX DF0WRITE
	LDA #((>playerL0444_1) & $0f) | (((>playerL0444_1) / 2) & $70)
	STA DF0WRITE
	LDA #26
	STA player1height
.L0445 ;;line 1503;;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL0445_1
	STX DF0WRITE
	LDA #((>playercolorL0445_1) & $0f) | (((>playercolorL0445_1) / 2) & $70)
	STA DF0WRITE
.L0446 ;;line 1531;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 1532;; 

.__Frame2
 ;;line 1533;; __Frame2

.
 ;;line 1534;; 

.L0447 ;;line 1535;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0447_1
	STX DF0WRITE
	LDA #((>playerL0447_1) & $0f) | (((>playerL0447_1) / 2) & $70)
	STA DF0WRITE
	LDA #16
	STA player1height
.
 ;;line 1553;; 

.L0448 ;;line 1554;;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL0448_1
	STX DF0WRITE
	LDA #((>playercolorL0448_1) & $0f) | (((>playercolorL0448_1) / 2) & $70)
	STA DF0WRITE
.
 ;;line 1572;; 

.
 ;;line 1573;; 

.L0449 ;;line 1574;;  goto __end_flying

 jmp .__end_flying

.
 ;;line 1575;; 

.__Frame1
 ;;line 1576;; __Frame1

.
 ;;line 1577;; 

.L0450 ;;line 1578;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0450_1
	STX DF0WRITE
	LDA #((>playerL0450_1) & $0f) | (((>playerL0450_1) / 2) & $70)
	STA DF0WRITE
	LDA #16
	STA player1height
.L0451 ;;line 1596;;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL0451_1
	STX DF0WRITE
	LDA #((>playercolorL0451_1) & $0f) | (((>playercolorL0451_1) / 2) & $70)
	STA DF0WRITE
.L0452 ;;line 1614;;  goto __end_flying

 jmp .__end_flying

.
 ;;line 1615;; 

.
 ;;line 1616;; 

.__Frame_Dead_1
 ;;line 1617;; __Frame_Dead_1

.
 ;;line 1618;; 

.L0453 ;;line 1619;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0453_1
	STX DF0WRITE
	LDA #((>playerL0453_1) & $0f) | (((>playerL0453_1) / 2) & $70)
	STA DF0WRITE
	LDA #16
	STA player1height
.L0454 ;;line 1637;;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL0454_1
	STX DF0WRITE
	LDA #((>playercolorL0454_1) & $0f) | (((>playercolorL0454_1) / 2) & $70)
	STA DF0WRITE
.L0455 ;;line 1655;;  goto __falling_bird

 jmp .__falling_bird

.
 ;;line 1656;; 

.__Frame_Dead_2
 ;;line 1657;; __Frame_Dead_2

.L0456 ;;line 1658;;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL0456_1
	STX DF0WRITE
	LDA #((>playerL0456_1) & $0f) | (((>playerL0456_1) / 2) & $70)
	STA DF0WRITE
	LDA #16
	STA player1height
.L0457 ;;line 1676;;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL0457_1
	STX DF0WRITE
	LDA #((>playercolorL0457_1) & $0f) | (((>playercolorL0457_1) / 2) & $70)
	STA DF0WRITE
.L0458 ;;line 1694;;  goto __falling_bird

 jmp .__falling_bird

.
 ;;line 1695;; 

.
 ;;line 1696;; 

.L0459 ;;line 1697;;  bank 3

 if ECHO2
 echo "    ",[(start_bank2 - *)]d , "bytes of ROM space left in bank 2")
 endif
ECHO2 = 1
 ORG $2FF4-bscode_length
 RORG $3FF4-bscode_length
start_bank2 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $2FFC
 RORG $3FFC
 .word (start_bank2 & $ffff)
 .word (start_bank2 & $ffff)
 ORG $3000
 RORG $5000
 repeat 129
 .byte 0
 repend
.L0460 ;;line 1698;;  temp1 = temp1

	LDA temp1
	STA temp1
.
 ;;line 1699;; 

.
 ;;line 1700;; 

.
 ;;line 1701;; 

.L0461 ;;line 1702;;  bank 4

 if ECHO3
 echo "    ",[(start_bank3 - *)]d , "bytes of ROM space left in bank 3")
 endif
ECHO3 = 1
 ORG $3FF4-bscode_length
 RORG $5FF4-bscode_length
start_bank3 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $3FFC
 RORG $5FFC
 .word (start_bank3 & $ffff)
 .word (start_bank3 & $ffff)
 ORG $4000
 RORG $7000
 repeat 129
 .byte 0
 repend
.L0462 ;;line 1703;;  temp1 = temp1

	LDA temp1
	STA temp1
.
 ;;line 1704;; 

.
 ;;line 1705;; 

.
 ;;line 1706;; 

.L0463 ;;line 1707;;  bank 5

 if ECHO4
 echo "    ",[(start_bank4 - *)]d , "bytes of ROM space left in bank 4")
 endif
ECHO4 = 1
 ORG $4FF4-bscode_length
 RORG $7FF4-bscode_length
start_bank4 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $4FFC
 RORG $7FFC
 .word (start_bank4 & $ffff)
 .word (start_bank4 & $ffff)
 ORG $5000
 RORG $9000
 repeat 129
 .byte 0
 repend
.L0464 ;;line 1708;;  temp1 = temp1

	LDA temp1
	STA temp1
.
 ;;line 1709;; 

.
 ;;line 1710;; 

.
 ;;line 1711;; 

.L0465 ;;line 1712;;  bank 6

 if ECHO5
 echo "    ",[(start_bank5 - *)]d , "bytes of ROM space left in bank 5")
 endif
ECHO5 = 1
 ORG $5FF4-bscode_length
 RORG $9FF4-bscode_length
start_bank5 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $5FFC
 RORG $9FFC
 .word (start_bank5 & $ffff)
 .word (start_bank5 & $ffff)
 ORG $6000
 RORG $B000
 repeat 129
 .byte 0
 repend
.L0466 ;;line 1713;;  temp1 = temp1

	LDA temp1
	STA temp1
 if ECHO6
 echo "    ",[(start_bank6 - *)]d , "bytes of ROM space left in bank 6")
 endif
ECHO6 = 1
 ORG $6FF4-bscode_length
 RORG $BFF4-bscode_length
start_bank6 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $6FFC
 RORG $BFFC
 .word (start_bank6 & $ffff)
 .word (start_bank6 & $ffff)
 ORG $7000
 RORG $D000
 repeat 129
 .byte 0
 repend
; Provided under the CC0 license. See the included LICENSE.txt for details.

;----------------------------------------
; Display Data
;----------------------------------------
; The Display Data bank is copied into RAM when DPC+ initializes the cartridge.
; This allows us to manipulate the data during run-time, but have a known
; starting state when the Atari is first turned on.
;
; Unlike normal Atari VCS/2600 sprite definitions, the sprite data in the
; Display Data bank is stored right-side-up.
;
;----------------------------------------

Zeros32:
SOUND_OFF = (* & $1fff)/32
DisplayDataDigitBlank:
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        
;	align 32
;Zeros32:
;SOUND_OFF = (* & $1fff)/32
;	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0    
	
SINE_WAVE = (* & $1fff)/32
	.byte 3,3,3,4,4,5,5,5
	.byte 5,5,5,5,4,4,3,3
	.byte 3,2,2,1,1,0,0,0
	.byte 0,0,0,0,1,1,2,2 

	align 32
TRIANGLE_WAVE = (* & $1fff)/32
	.byte 0,0,1,1,1,2,2,2
	.byte 3,3,3,4,4,4,5,5
	.byte 5,5,4,4,4,3,3,3
	.byte 2,2,2,1,1,1,0,0
	
 	align 32
SAWTOOTH_WAVE = (* & $1fff)/32
	.byte 0,0,0,0,1,1,1,1
	.byte 1,1,2,2,2,2,2,2
	.byte 3,3,3,3,3,3,4,4
	.byte 4,4,4,4,5,5,5,5
	
	align 32
SQUARE_WAVE_VOL5 = (* & $1fff)/32
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 5,5,5,5,5,5,5,5
	.byte 5,5,5,5,5,5,5,5

	align 32
SQUARE_WAVE_VOL4 = (* & $1fff)/32
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 4,4,4,4,4,4,4,4
	.byte 4,4,4,4,4,4,4,4

	align 32
SQUARE_WAVE_VOL3 = (* & $1fff)/32
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 3,3,3,3,3,3,3,3
	.byte 3,3,3,3,3,3,3,3
	
	align 32
NOISE_WAVE = (* & $1fff)/32
	.byte  7, 1, 9,10, 2, 8, 8,14
	.byte  3,13, 8, 5,12, 2, 3, 7
	.byte  7, 1, 8, 4,15, 1,13, 5
	.byte  8, 5,11, 6, 8, 7, 9, 2

; low and high byte of address table (for ROMdata array in C)
        .byte <fetcher_address_table
        .byte ((>fetcher_address_table) & $0f) | (((>fetcher_address_table) / 2) & $70)
 .byte 0
 .byte 0
FETCHER_BEGIN
 .byte 16
 .byte 16
 .byte 16
 .byte 16 ; to zero-fill on boot
;bB.asm
; bB.asm file is split here
playerL0200_0
	.byte    
	.byte    %00011000
	.byte    %00011000
	.byte    %00100100
	.byte    %11000011
	.byte    %11000011
	.byte    %00100100
	.byte    %00011000
	.byte    %00011000
	.byte    
playerL0201_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %01111100
	.byte  %11111111
	.byte  %00111010
	.byte  %00011000
	.byte  %00011000
	.byte  %00001000
	.byte  %00000000
	.byte  %00000000
playercolorL0202_1
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
PF_data1
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00001000
	.byte %00001000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011100
	.byte %00011100
	.byte %00011100
	.byte %00111100
	.byte %00111100
	.byte %00111110
	.byte %00111110
	.byte %00111110
	.byte %00011110
	.byte %00111110
	.byte %00111110
	.byte %00111110
	.byte %00100110
	.byte %00111100
	.byte %00011100
	.byte %00000000
	.byte %01000001
	.byte %00000001
	.byte %10000001
	.byte %11110001
	.byte %00011001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01100000
	.byte %00100000
	.byte %00100000
	.byte %00110100
	.byte %01100100
	.byte %00000000
	.byte %00000111
	.byte %00000011
	.byte %00000111
	.byte %10010100
	.byte %10010000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00110001
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00100000
	.byte %00110100
	.byte %00011100
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10000000
	.byte %10000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01100000
	.byte %00111001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01000000
	.byte %01000000
	.byte %01000000
	.byte %01000000
	.byte %01001000
	.byte %01001100
	.byte %01000100
	.byte %00100000
	.byte %00100001
	.byte %00100001
	.byte %00101001
	.byte %11101100
	.byte %00101000
	.byte %00101000
	.byte %00000000
	.byte %00000011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00111010
	.byte %11100110
	.byte %10000110
	.byte %11000010
	.byte %01000010
	.byte %00000011
	.byte %00000001
	.byte %00000000
	.byte %00111100
	.byte %01111000
	.byte %00001000
	.byte %00000100
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %01100001
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00111111
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000010
	.byte %00000010
	.byte %00000010
	.byte %00000010
	.byte %00000010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01010000
	.byte %10010001
	.byte %00000101
	.byte %00000101
	.byte %00000101
	.byte %00010101
	.byte %11110110
	.byte %00111010
	.byte %00101110
	.byte %00100010
	.byte %00000100
	.byte %00000100
	.byte %10000100
	.byte %11000100
	.byte %00000110
	.byte %00100010
	.byte %00110010
	.byte %00011011
	.byte %00000011
	.byte %00000011
	.byte %00000010
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00100000
	.byte %00100000
	.byte %00001110
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00011000
	.byte %00000000
	.byte %00000011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %11000000
	.byte %01000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00111010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01100100
	.byte %11100100
	.byte %11100100
	.byte %11100100
	.byte %11101100
	.byte %11101110
	.byte %11101110
	.byte %11101110
	.byte %11101110
	.byte %11101100
	.byte %01001100
	.byte %01000100
	.byte %01000100
	.byte %01000100
	.byte %01000100
	.byte %01101100
	.byte %00101000
	.byte %00101000
	.byte %00101000
	.byte %00101010
	.byte %00101010
	.byte %00101011
	.byte %00101011
	.byte %00101011
	.byte %00101011
	.byte %00101011
	.byte %00101011
	.byte %00111011
	.byte %00010011
	.byte %00010011
	.byte %00010011
	.byte %00010011
	.byte %00010011
	.byte %00010011
	.byte %00010011
	.byte %01010011
	.byte %01010011
	.byte %01010011
	.byte %01010010
	.byte %01010010
	.byte %01010010
	.byte %01010010
	.byte %01010010
	.byte %01010100
	.byte %01010100
	.byte %00110100
	.byte %00110100
	.byte %00111100
	.byte %00111000
	.byte %00111000
	.byte %00111000
	.byte %00111000
	.byte %00111000
	.byte %00111000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00001000
	.byte %00001000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00010000
	.byte %00011000
	.byte %10011000
	.byte %00011000
	.byte %00010000
	.byte %00000010
	.byte %00000010
	.byte %01000010
	.byte %01000010
	.byte %00000000
	.byte %01000011
	.byte %01010010
	.byte %00010010
	.byte %01111010
	.byte %00001111
	.byte %10000001
	.byte %10000111
	.byte %00000110
	.byte %00000110
	.byte %00000010
	.byte %00111000
	.byte %00000001
	.byte %10010101
	.byte %00000000
	.byte %10100001
	.byte %10100001
	.byte %00100101
	.byte %00100101
	.byte %00100001
	.byte %00110001
	.byte %00110111
	.byte %00100000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000110
	.byte %10000000
	.byte %00000000
	.byte %00010000
	.byte %00010001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00011001
	.byte %00110000
	.byte %00000000
	.byte %01000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10000000
	.byte %00110000
	.byte %00010110
	.byte %00000110
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
playfieldcolorL0204
	.byte  _F8
	.byte  _F8
	.byte  _D6
	.byte  _D6
	.byte  _D6
	.byte  _D6
	.byte  _D6
	.byte  _D6
	.byte  _CE
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _C8
	.byte  _C8
	.byte  _C8
	.byte  _CA
	.byte  _C4
	.byte  _C8
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _C4
	.byte  _C4
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _C8
	.byte  _C8
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _CA
	.byte  _C8
	.byte  _C8
	.byte  _C8
	.byte  _CA
	.byte  _C8
	.byte  _C8
	.byte  _CA
	.byte  _CA
	.byte  _C4
	.byte  _D6
	.byte  _D6
	.byte  _C8
	.byte  _D4
	.byte  _D4
	.byte  _D2
	.byte  _D2
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _D4
	.byte  _F6
	.byte  _F0
	.byte  _F0
	.byte  _F6
	.byte  _F6
	.byte  _F6
	.byte  _F6
	.byte  _F0
	.byte  _F6
	.byte  _F6
	.byte  _F0
	.byte  _F6
	.byte  _FE
	.byte  _F6
	.byte  _F0
	.byte  _F0
	.byte  _FE
	.byte  _FE
	.byte  _F0
	.byte  _F0
	.byte  _F0
	.byte  _F0
backgroundcolorL0205
	.byte  _90
	.byte  _90
	.byte  _90
	.byte  _94
	.byte  _96
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _98
	.byte  _A8
	.byte  _AC
	.byte  _9E
	.byte  _9E
	.byte  _FE
	.byte  _FE
	.byte  _FC
	.byte  _FA
	.byte  _FA
	.byte  _FC
	.byte  _F8
	.byte  _FA
	.byte  _FA
	.byte  _F8
	.byte  _F8
	.byte  _D4
	.byte  _F6
	.byte  _F8
	.byte  _F4
	.byte  _F4
	.byte  _F6
	.byte  _FA
	.byte  _F4
	.byte  _D4
	.byte  _F4
	.byte  _F6
	.byte  _F8
	.byte  _D4
	.byte  _F4
	.byte  _F8
	.byte  _F4
	.byte  _D4
	.byte  _F8
	.byte  _F8
	.byte  _FE
	.byte  _FE
	.byte  _FE
	.byte  _FE
	.byte  _FE
	.byte  _FE
	.byte  _FE
	.byte  _FE
player42then_0
	.byte    %00011000
	.byte    %00011000
	.byte    %00100100
	.byte    %11000011
	.byte    %11000011
	.byte    %00100100
	.byte    %00011000
	.byte    %00011000
playerL0287_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %01111100
	.byte  %11111111
	.byte  %00111010
	.byte  %00011000
	.byte  %00011000
	.byte  %00001000
	.byte  %00000000
	.byte  %00000000
playercolorL0288_1
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
playerL0371_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %01111100
	.byte  %11111111
	.byte  %00000110
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
playerL0372_0
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %01111100
	.byte  %11111111
	.byte  %00000110
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
playerL0374_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %01111100
	.byte  %11111111
	.byte  %00111010
	.byte  %00011000
	.byte  %00011000
	.byte  %00001000
	.byte  %00000000
	.byte  %00000000
playerL0375_0
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %01111100
	.byte  %11111111
	.byte  %00111010
	.byte  %00011000
	.byte  %00011000
	.byte  %00001000
	.byte  %00000000
	.byte  %00000000
playerL0379_0
	.byte    %00011000
	.byte    %00011000
	.byte    %00100100
	.byte    %11000011
	.byte    %11000011
	.byte    %00100100
	.byte    %00011000
	.byte    %00011000
playerL0380_1
	.byte    %00000000
	.byte    %00000000
	.byte    %00111000
	.byte    %01111100
	.byte    %11111111
	.byte    %00111010
	.byte    %00011000
	.byte    %00011000
	.byte    %00001000
	.byte    %00000000
	.byte    %00000000
playerL0439_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00001000
	.byte  %00001000
	.byte  %00011000
	.byte  %00111000
	.byte  %00111000
	.byte  %11111111
	.byte  %00000110
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte 
playercolorL0440_1
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
playerL0444_1
	.byte    %00111100 
	.byte     %01111110
	.byte     %11011011 
	.byte     %11111111
	.byte     %10000001 
	.byte     %01111110 
	.byte     %01000010 
	.byte     %01011010 
	.byte     %01011010
	.byte     %00111100 
	.byte     %00011000 
	.byte     %00111100 
	.byte     %01111110 
	.byte     %11111111 
	.byte     %11111111
	.byte     %11111111
	.byte     %11111111
	.byte     %01111110
	.byte     %01111110
	.byte     %01100110 
	.byte     %01100110
	.byte     %01100110
	.byte     %01100110
	.byte     %01100110
	.byte     %11100111 
	.byte     %11100111
playercolorL0445_1
	.byte    $34 
	.byte    $34
	.byte    $0E 
	.byte    $34
	.byte    $32 
	.byte    $0E 
	.byte    $00 
	.byte    $44 
	.byte    $44
	.byte    $0E 
	.byte    $34 
	.byte    $84 
	.byte    $34 
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $34
	.byte    $0E 
	.byte    $0E
playerL0447_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %01111100
	.byte  %11111111
	.byte  %00111010
	.byte  %00011000
	.byte  %00011000
	.byte  %00001000
	.byte  %00000000
	.byte  %00000000
playercolorL0448_1
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
playerL0450_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %01111100
	.byte  %11111111
	.byte  %00000110
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
playercolorL0451_1
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
playerL0453_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00011000
	.byte  %00011100
	.byte  %00001000
	.byte  %00001000
	.byte  %00011100
	.byte  %00111100
	.byte  %00101100
	.byte  %01101110
	.byte  %00001000
	.byte  %00010100
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
playercolorL0454_1
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
playerL0456_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00011000
	.byte  %00111000
	.byte  %00010000
	.byte  %00010000
	.byte  %00111000
	.byte  %00111100
	.byte  %00110100
	.byte  %01110110
	.byte  %00010000
	.byte  %00101000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
playercolorL0457_1
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
	.byte    _00
 if ECHOFIRST
       echo "    ",[(DPC_graphics_end - *)]d , "bytes of ROM space left in graphics bank")
 endif 
ECHOFIRST = 1
 
 
; Provided under the CC0 license. See the included LICENSE.txt for details.

       ORG $7FF4-bscode_length
       RORG $DFF4-bscode_length
DPC_graphics_end

; Provided under the CC0 license. See the included LICENSE.txt for details.

; every bank has this stuff at the same place
; this code can switch to/from any bank at any entry point
; and can preserve register values
; note: lines not starting with a space are not placed in all banks
;
; line below tells the compiler how long this is - do not remove
;size=32

begin_bscode
 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha

BS_return
 pha
 txa
 pha
 tsx

 if bankswitch != 64
   lda 4,x ; get high byte of return address

   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif

BS_jsr
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
; Provided under the CC0 license. See the included LICENSE.txt for details.

 org $8000
 rorg $1000
; Provided under the CC0 license. See the included LICENSE.txt for details.

; 1K Frequency Table.
; Fred Quimby, Darrell Spice Jr, Chris Walton 2010
;
; The 1K Frequency Table can contain up to 256 frequency values
;
; Table entries are defined as 2^32*freq/20000
;
; If User ARM code is being used, then the last 512 bytes of the frequency
; table will no longer be available, reducing the number of frequencies you can
; use to 128.

; piano key frequencies (s = sharp)

.freq_table_start

			DC.L 0
A0  = (* & $3ff)/4
			DC.L 5905580
			
A0s = (* & $3ff)/4
			DC.L 6256744

B0  = (* & $3ff)/4	
			DC.L 6628789
			
C1  = (* & $3ff)/4
			DC.L 7022958
			
C1s = (* & $3ff)/4			
			DC.L 7440565
			
D1  = (* & $3ff)/4			
			DC.L 7883004
			
D1s = (* & $3ff)/4		
			DC.L 8351751
			
E1  = (* & $3ff)/4			
			DC.L 8848372
			
F1  = (* & $3ff)/4			
			DC.L 9374524
			
F1s = (* & $3ff)/4			
			DC.L 9931962
			
G1  = (* & $3ff)/4			
			DC.L 10522547
			
G1s = (* & $3ff)/4			
			DC.L 11148251
			
A1  = (* & $3ff)/4			
			DC.L 11811160
			
A1s = (* & $3ff)/4			
			DC.L 12513488
			
B1  = (* & $3ff)/4			
			DC.L 13257579
			
C2  = (* & $3ff)/4			
			DC.L 14045916
			
C2s = (* & $3ff)/4			
			DC.L 14881129
			
D2  = (* & $3ff)/4			
			DC.L 15766007
			
D2s = (* & $3ff)/4			
			DC.L 16703503
			
E2  = (* & $3ff)/4			
			DC.L 17696745
			
F2  = (* & $3ff)/4			
			DC.L 18749048
			
F2s = (* & $3ff)/4			
			DC.L 19863924
			
G2  = (* & $3ff)/4			
			DC.L 21045095
			
G2s = (* & $3ff)/4			
			DC.L 22296501
			
A2  = (* & $3ff)/4			
			DC.L 23622320
			
A2s = (* & $3ff)/4			
			DC.L 25026976
			
B2  = (* & $3ff)/4			
			DC.L 26515158
			
C3  = (* & $3ff)/4			
			DC.L 28091831
			
C3s = (* & $3ff)/4			
			DC.L 29762258
			
D3  = (* & $3ff)/4			
			DC.L 31532014
			
D3s = (* & $3ff)/4			
			DC.L 33407005
			
E3  = (* & $3ff)/4			
			DC.L 35393489
			
F3  = (* & $3ff)/4			
			DC.L 37498096
			
F3s = (* & $3ff)/4			
			DC.L 39727849
			
G3  = (* & $3ff)/4			
			DC.L 42090189
			
G3s = (* & $3ff)/4			
			DC.L 44593002
			
A3  = (* & $3ff)/4			
			DC.L 47244640
			
A3s = (* & $3ff)/4			
			DC.L 50053953
			
B3  = (* & $3ff)/4			
			DC.L 53030316
			
C4  = (* & $3ff)/4			
			DC.L 56183662
			
C4s = (* & $3ff)/4			
			DC.L 59524517
			
D4  = (* & $3ff)/4			
			DC.L 63064029
			
D4s = (* & $3ff)/4			
			DC.L 66814011
			
E4  = (* & $3ff)/4			
			DC.L 70786979
			
F4  = (* & $3ff)/4			
			DC.L 74996192
			
F4s = (* & $3ff)/4			
			DC.L 79455697
			
G4  = (* & $3ff)/4			
			DC.L 84180379
			
G4s = (* & $3ff)/4			
			DC.L 89186005
			
A4  = (* & $3ff)/4			
			DC.L 94489281
			
A4s = (* & $3ff)/4			
			DC.L 100107906
			
B4  = (* & $3ff)/4			
			DC.L 106060631
			
C5  = (* & $3ff)/4			
			DC.L 112367325
			
C5s = (* & $3ff)/4			
			DC.L 119049034
			
D5  = (* & $3ff)/4			
			DC.L 126128057
			
D5s = (* & $3ff)/4			
			DC.L 133628022
			
E5  = (* & $3ff)/4			
			DC.L 141573958
			
F5  = (* & $3ff)/4			
			DC.L 149992383
			
F5s = (* & $3ff)/4			
			DC.L 158911395
			
G5  = (* & $3ff)/4			
			DC.L 168360758
			
G5s = (* & $3ff)/4			
			DC.L 178372009
			
A5  = (* & $3ff)/4			
			DC.L 188978561
			
A5s = (* & $3ff)/4			
			DC.L 200215811
			
B5  = (* & $3ff)/4			
			DC.L 212121263
			
C6  = (* & $3ff)/4			
			DC.L 224734649
			
C6s = (* & $3ff)/4			
			DC.L 238098067
			
D6  = (* & $3ff)/4			
			DC.L 252256115
			
D6s = (* & $3ff)/4			
			DC.L 267256044
			
E6  = (* & $3ff)/4			
			DC.L 283147915
			
F6  = (* & $3ff)/4			
			DC.L 299984767
			
F6s = (* & $3ff)/4			
			DC.L 317822789
			
G6  = (* & $3ff)/4			
			DC.L 336721516
			
G6s = (* & $3ff)/4			
			DC.L 356744019
			
A6  = (* & $3ff)/4			
			DC.L 377957122
			
A6s = (* & $3ff)/4			
			DC.L 400431622
			
B6  = (* & $3ff)/4			
			DC.L 424242525
			
C7  = (* & $3ff)/4			
			DC.L 449469299
			
C7s = (* & $3ff)/4			
			DC.L 476196134
			
D7  = (* & $3ff)/4			
			DC.L 504512230
			
D7s = (* & $3ff)/4			
			DC.L 534512088
			
E7  = (* & $3ff)/4			
			DC.L 566295831
			
F7  = (* & $3ff)/4			
			DC.L 599969533
			
F7s = (* & $3ff)/4			
			DC.L 635645578
			
G7  = (* & $3ff)/4			
			DC.L 673443031
			
G7s = (* & $3ff)/4			
			DC.L 713488038
			
A7  = (* & $3ff)/4			
			DC.L 755914244
			
A7s = (* & $3ff)/4			
			DC.L 800863244
			
B7  = (* & $3ff)/4			
			DC.L 848485051
			
C8  = (* & $3ff)/4			
			DC.L 898938597

;values for 89-255 may go here 

	if (* <= $1400)
	  ds ($1400-*) ; pad out remaining space in frequency table
	else
	  echo "FATAL ERROR - Frequency table exceeds 1K"
	  err
	endif
