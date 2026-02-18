; Disassembly of ~/MissleCommand/missle_cmd.bin
; Disassembled Sat Feb 14 14:41:30 2026
; Using Stella 7.0
;
; ROM properties name : Missile Command (Trakball) (2002) (Thomas Jentzsch)
; ROM properties MD5  : 8ce9126066f2ddd5173e9f1f9ce1494e
; Bankswitch type     : 4K* (4K) 
;
; Legend: *  = CODE not yet run (tentative code)
;         D  = DATA directive (referenced in some way)
;         G  = GFX directive, shown as '#' (stored in player, missile, ball)
;         P  = PGFX directive, shown as '*' (stored in playfield)
;         C  = COL directive, shown as color constants (stored in player color)
;         CP = PCOL directive, shown as color constants (stored in playfield color)
;         CB = BCOL directive, shown as color constants (stored in background color)
;         A  = AUD directive (stored in audio registers)
;         i  = indexed accessed only
;         c  = used by code executed in RAM
;         s  = used by stack
;         !  = page crossed, 1 cycle penalty

    processor 6502
    include "vcs.h"

;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

BLACK            = $00
YELLOW           = $10
BROWN            = $20
ORANGE           = $30
RED              = $40
MAUVE            = $50
VIOLET           = $60
PURPLE           = $70
BLUE             = $80
BLUE_CYAN        = $90
CYAN             = $a0
CYAN_GREEN       = $b0
GREEN            = $c0
GREEN_YELLOW     = $d0
GREEN_BEIGE      = $e0
BEIGE            = $f0


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------




;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80
ram_81          = $81
ram_82          = $82
ram_83          = $83
ram_84          = $84
ram_85          = $85
ram_86          = $86
ram_87          = $87
ram_88          = $88
ram_89          = $89
ram_8A          = $8a
ram_8B          = $8b
ram_8C          = $8c
ram_8D          = $8d
ram_8E          = $8e
ram_8F          = $8f
ram_90          = $90
ram_91          = $91
ram_92          = $92
ram_93          = $93
ram_94          = $94
ram_95          = $95
ram_96          = $96
ram_97          = $97
ram_98          = $98
ram_99          = $99
ram_9A          = $9a
ram_9B          = $9b
ram_9C          = $9c
;                 $9d  (i)
ram_9E          = $9e
;                 $9f  (i)
;                 $a0  (i)
ram_A1          = $a1
ram_A2          = $a2
ram_A3          = $a3
;                 $a4  (i)
;                 $a5  (i)
ram_A6          = $a6
;                 $a7  (i)
;                 $a8  (i)
ram_A9          = $a9
ram_AA          = $aa
ram_AB          = $ab
;                 $ac  (i)
ram_AD          = $ad
;                 $ae  (i)
ram_AF          = $af
;                 $b0  (i)
ram_B1          = $b1
;                 $b2  (i)
ram_B3          = $b3
;                 $b4  (i)
ram_B5          = $b5
;                 $b6  (i)
;                 $b7  (i)
ram_B8          = $b8
;                 $b9  (i)
;                 $ba  (i)
ram_BB          = $bb
;                 $bc  (i)
;                 $bd  (i)
ram_BE          = $be
;                 $bf  (i)
;                 $c0  (i)
ram_C1          = $c1
;                 $c2  (i)
;                 $c3  (i)
ram_C4          = $c4
;                 $c5  (i)
;                 $c6  (i)
ram_C7          = $c7
;                 $c8  (i)
;                 $c9  (i)
ram_CA          = $ca
ram_CB          = $cb
ram_CC          = $cc
ram_CD          = $cd
ram_CE          = $ce
ram_CF          = $cf
ram_D0          = $d0
ram_D1          = $d1
;                 $d2  (i)
ram_D3          = $d3
;                 $d4  (i)
ram_D5          = $d5
;                 $d6  (i)
ram_D7          = $d7
ram_D8          = $d8
ram_D9          = $d9
ram_DA          = $da
ram_DB          = $db
ram_DC          = $dc
ram_DD          = $dd
ram_DE          = $de
ram_DF          = $df
ram_E0          = $e0
ram_E1          = $e1
ram_E2          = $e2
ram_E3          = $e3
ram_E4          = $e4
ram_E5          = $e5
ram_E6          = $e6
ram_E7          = $e7
ram_E8          = $e8
ram_E9          = $e9
ram_EA          = $ea
ram_EB          = $eb
ram_EC          = $ec
ram_ED          = $ed
ram_EE          = $ee
ram_EF          = $ef
;                 $f0  (i)
ram_F1          = $f1
;                 $f2  (i)
ram_F3          = $f3
;                 $f4  (i)
ram_F5          = $f5
ram_F6          = $f6
ram_F7          = $f7
ram_F8          = $f8
ram_F9          = $f9

ram_FB          = $fb
;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f039


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $f000

Lf000
    ldx     #$04                    ;2   =   2
Lf002
    lda     #$02                    ;2        
    cpx     #$02                    ;2        
    bcs     Lf011                   ;2/3      
    lsr                             ;2        
    ldy     ram_A1                  ;3        
    cpy     #$f0                    ;2        
    bne     Lf011                   ;2/3      
    lda     #$fc                    ;2   =  17
Lf011
    clc                             ;2        
    adc     ram_86,x                ;4        
    ldy     #$02                    ;2        
    sec                             ;2   =  10
Lf017
    iny                             ;2        
    sbc     #$0f                    ;2        
    bcs     Lf017                   ;2/3      
    eor     #$ff                    ;2        
    sbc     #$06                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     WSYNC                   ;3   =  21
;---------------------------------------
Lf026
    dey                             ;2        
    bpl     Lf026                   ;2/3      
    sta     RESP0,x                 ;4        
    sta     HMP0,x                  ;4        
    lda     ram_A1                  ;3        
    bne     Lf034                   ;2/3      
    dex                             ;2        
    bpl     Lf002                   ;2/3 =  21
Lf034
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    rts                             ;6   =   9
    
Start
    cld                             ;2        
    ldx     #$00                    ;2        
    stx     SWACNT                  ;4        
    txs                             ;2        
    pha                             ;3        
    txa                             ;2   =  15
Lf042
    pha                             ;3        
    inx                             ;2        
    bne     Lf042                   ;2/3      
    lda     #$87                    ;2        
    sta     ram_DB                  ;3        
    lda     #$01                    ;2        
    sta     ram_E9                  ;3   =  17
Lf04e
    lda     #$02                    ;2        
    sta     VBLANK                  ;3        
    sta     VSYNC                   ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lsr                             ;2        
    sta     VSYNC                   ;3        
    lda     #$30                    ;2        
    sta     TIM64T                  ;4        
    inc     ram_80                  ;5        
    bne     Lf074                   ;2/3      
    inc     ram_E8                  ;5        
    lda     ram_DB                  ;3        
    and     ram_F8                  ;3        
    bpl     Lf074                   ;2/3      
    lda     ram_F5                  ;3         *
    eor     #$01                    ;2         *
    sta     ram_F5                  ;3   =  39 *
Lf074
    lda     ram_DC                  ;3        
    beq     Lf07f                   ;2/3      
    dec     ram_DC                  ;5         *
    bne     Lf07f                   ;2/3       *
    jsr     Lfca5                   ;6   =  18 *
Lf07f
    bit     ram_DB                  ;3        
    bpl     Lf087                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_DE                  ;3   =  10
Lf087
    lda     ram_DE                  ;3        
    cmp     #$e8                    ;2        
    bne     Lf0ba                   ;2/3      
    lda     ram_E0                  ;3        
    bne     Lf0ad                   ;2/3      
    lda     #$07                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_DD                  ;3        
    bne     Lf0a0                   ;2/3      
    jsr     Lfca5                   ;6        
    lda     ram_DD                  ;3        
    beq     Lf0b0                   ;2/3 =  33
Lf0a0
    lda     #$04                    ;2        
    sta     ram_DF                  ;3        
    dec     ram_DD                  ;5        
    lda     #$05                    ;2        
    ldy     #$00                    ;2        
    jsr     Lfcc1                   ;6   =  20
Lf0ad
    jmp     Lf13e                   ;3   =   3
    
Lf0b0
    lda     #$e0                    ;2        
    sta     ram_DE                  ;3        
    lda     #$00                    ;2        
    sta     ram_DF                  ;3        
    sta     ram_E0                  ;3   =  13
Lf0ba
    lda     ram_DE                  ;3        
    cmp     #$e0                    ;2        
    bne     Lf0fb                   ;2/3      
    lda     ram_80                  ;3        
    and     #$0f                    ;2        
    bne     Lf13e                   ;2/3!     
    ldy     ram_E0                  ;3        
    lda     #$25                    ;2   =  19
Lf0ca
    cpy     #$06                    ;2        
    beq     Lf0e9                   ;2/3      
    ldx     Lfdff,y                 ;4        
    iny                             ;2        
    cmp     ram_A9,x                ;4        
    bne     Lf0ca                   ;2/3      
    lda     #$5b                    ;2        
    sta     ram_A9,x                ;4        
    sty     ram_E0                  ;3        
    lda     #$00                    ;2        
    ldy     #$01                    ;2        
    jsr     Lfcc1                   ;6        
    lda     #$05                    ;2        
    sta     ram_DF                  ;3        
    bne     Lf13e                   ;2/3!=  42
Lf0e9
    lda     #$a8                    ;2        
    sta     ram_DE                  ;3        
    lda     #$04                    ;2        
    sta     ram_E0                  ;3        
    lda     #$01                    ;2        
    sta     ram_DF                  ;3        
    jsr     Lfd81                   ;6        
    jmp     Lf470                   ;3   =  24
    
Lf0fb
    lda     ram_DF                  ;3        
    cmp     #$06                    ;2        
    beq     Lf13e                   ;2/3      
    bit     ram_DE                  ;3        
    bpl     Lf13e                   ;2/3      
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    bne     Lf13e                   ;2/3      
    dec     ram_DE                  ;5        
    bmi     Lf112                   ;2/3      
    jmp     Lf1df                   ;3   =  29
    
Lf112
    lda     ram_DE                  ;3        
    cmp     #$a0                    ;2        
    bne     Lf13e                   ;2/3      
    lda     #$32                    ;2        
    sta     ram_90                  ;3        
    sta     ram_8E                  ;3        
    lda     ram_F1                  ;3        
    ora     ram_F3                  ;3        
    beq     Lf13b                   ;2/3      
    lda     ram_E9                  ;3        
    cmp     #$35                    ;2        
    bcc     Lf13b                   ;2/3      
    lda     ram_F5                  ;3         *
    eor     #$01                    ;2         *
    tax                             ;2         *
    lda     ram_EC,x                ;4         *
    beq     Lf13b                   ;2/3       *
    inc     ram_DE                  ;5         *
    bit     INPT4|$30               ;3         *
    bmi     Lf13e                   ;2/3       *
    dec     ram_DE                  ;5   =  58 *
Lf13b
    jsr     Lfd0b                   ;6   =   6
Lf13e
    bit     ram_DB                  ;3        
    bvc     Lf152                   ;2/3      
    lda     SWCHB                   ;4        
    lsr                             ;2        
    bcc     Lf14b                   ;2/3 =  13
Lf148
    jmp     Lf217                   ;3   =   3
    
Lf14b
    lsr                             ;2        
    bcc     Lf148                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_DB                  ;3   =   9
Lf152
    lda     #$25                    ;2        
    ldy     #$ff                    ;2        
    ldx     #$0a                    ;2   =   6
Lf158
    sta     ram_A9,x                ;4        
    sty     ram_AA,x                ;4        
    sty     ram_CC,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf158                   ;2/3      
    iny                             ;2        
    ldx     #$05                    ;2   =  22
Lf165
    sty     ram_EF,x                ;4        
    dex                             ;2        
    bpl     Lf165                   ;2/3      
    sty     ram_ED                  ;3        
    sty     ram_FB                  ;3        
    sty     ram_F6                  ;3        
    sty     ram_F7                  ;3        
    lda     ram_E9                  ;3        
    ldx     #$3f                    ;2        
    stx     ram_EC                  ;3        
    cmp     #$18                    ;2        
    bcc     Lf187                   ;2/3      
    sed                             ;2         *
    sbc     #$34                    ;2         *
    bcs     Lf183                   ;2/3       *
    adc     #$17                    ;2   =  40 *
Lf183
    cld                             ;2         *
    stx     ram_ED                  ;3         *
    iny                             ;2   =   7 *
Lf187
    sty     ram_F5                  ;3        
    tax                             ;2        
    tya                             ;2        
    ror                             ;2        
    ora     Lfe72,x                 ;4        
    sta     ram_F8                  ;3        
    cpx     #$17                    ;2        
    bne     Lf199                   ;2/3      
    ldy     #$40                    ;2         *
    sty     ram_FB                  ;3   =  25 *
Lf199
    and     #$0e                    ;2        
    tax                             ;2        
    dex                             ;2        
    stx     ram_E7                  ;3        
    lda     #$54                    ;2        
    sta     ram_93                  ;3        
    sta     ram_94                  ;3        
    lda     ram_DB                  ;3        
    ora     #$40                    ;2        
    sta     ram_DB                  ;3        
    bit     ram_DB                  ;3        
    bmi     Lf1dc                   ;2/3      
    lda     #$32                    ;2        
    sta     ram_8E                  ;3        
    sta     ram_90                  ;3        
    ldx     #$01                    ;2        
    stx     ram_DF                  ;3        
    ldx     #$04                    ;2        
    stx     ram_E0                  ;3        
    lda     #$13                    ;2        
    bit     ram_FB                  ;3        
    bvc     Lf1c5                   ;2/3      
    lda     #$fb                    ;2   =  57 *
Lf1c5
    ldy     ram_E7                  ;3        
    iny                             ;2   =   5
Lf1c8
    clc                             ;2        
    adc     #$0d                    ;2        
    dey                             ;2        
    bpl     Lf1c8                   ;2/3      
    sta     ram_9B                  ;3        
    lda     #$b0                    ;2        
    sta     ram_DE                  ;3        
    lda     ram_80                  ;3        
    ora     #$02                    ;2        
    sta     ram_D8                  ;3        
    sta     ram_D9                  ;3   =  27
Lf1dc
    jmp     Lf217                   ;3   =   3
    
Lf1df
    ldy     #$0f                    ;2        
    lda     ram_E7                  ;3        
    cmp     #$10                    ;2        
    bcs     Lf1e8                   ;2/3      
    tay                             ;2   =  11
Lf1e8
    lda     Lfe51,y                 ;4        
    bit     ram_FB                  ;3        
    bvc     Lf1f0                   ;2/3      
    lsr                             ;2   =  11 *
Lf1f0
    sta     ram_DE                  ;3        
    lda     Lfe63,y                 ;4        
    sta     ram_EB                  ;3        
    lda     #$0a                    ;2        
    sta     ram_DD                  ;3        
    lda     #$4f                    ;2        
    ldy     #$01                    ;2        
    ldx     #$02                    ;2   =  21
Lf201
    sta     ram_C4,x                ;4        
    sty     ram_C7,x                ;4        
    sty     ram_B5,x                ;4        
    dec     ram_B5,x                ;6        
    dex                             ;2        
    bpl     Lf201                   ;2/3      
    stx     ram_91                  ;3        
    stx     ram_92                  ;3        
    inx                             ;2        
    stx     ram_DF                  ;3        
    lda     #$40                    ;2        
    sta     ram_DB                  ;3   =  38
Lf217
    ldy     #$1f                    ;2        
    lda     SWCHB                   ;4        
    lsr                             ;2        
    ror                             ;2        
    bcs     Lf224                   ;2/3      
    bmi     Lf224                   ;2/3      
    ldy     #$07                    ;2   =  16
Lf224
    sty     ram_DA                  ;3        
    lda     #$ff                    ;2        
    bcc     Lf22e                   ;2/3      
    sta     ram_EA                  ;3        
    bne     Lf25b                   ;2/3 =  12
Lf22e
    sta     ram_DB                  ;3        
    lda     ram_EA                  ;3        
    bmi     Lf23a                   ;2/3      
    eor     ram_80                  ;3         *
    and     ram_DA                  ;3         *
    bne     Lf25b                   ;2/3 =  16 *
Lf23a
    lda     ram_80                  ;3        
    and     ram_DA                  ;3        
    sta     ram_EA                  ;3        
    sed                             ;2        
    clc                             ;2        
    lda     ram_E9                  ;3        
    adc     #$01                    ;2        
    cmp     #$52                    ;2        
    bne     Lf24c                   ;2/3      
    lda     #$01                    ;2   =  24 *
Lf24c
    sta     ram_E9                  ;3        
    cld                             ;2        
    lda     #$00                    ;2        
    sta     ram_DF                  ;3        
    sta     ram_E8                  ;3        
    sta     ram_E7                  ;3        
    sta     ram_8E                  ;3        
    sta     ram_90                  ;3   =  22
Lf25b
    bit     ram_FB                  ;3        
    bpl     Lf262                   ;2/3      
    jmp     Lf470                   ;3   =   8 *
    
Lf262
    jsr     Lfc89                   ;6        
    ldx     #$00                    ;2        
    lda     ram_E9                  ;3        
    cmp     #$35                    ;2        
    bcs     Lf26f                   ;2/3      
    ldx     ram_F5                  ;3   =  18
Lf26f
    lda     INPT4|$30,x             ;4        
    bpl     Lf27c                   ;2/3      
    lda     ram_A2                  ;3        
    and     #$f7                    ;2        
    sta     ram_A2                  ;3        
    jmp     Lf2f8                   ;3   =  17
    
Lf27c
    lda     ram_A2                  ;3        
    and     #$08                    ;2        
    bne     Lf2f8                   ;2/3      
    ldx     #$02                    ;2   =   9
Lf284
    lda     ram_B5,x                ;4        
    bpl     Lf29b                   ;2/3      
    dex                             ;2        
    bpl     Lf284                   ;2/3 =  10
Lf28b
    lda     ram_DF                  ;3         *
    and     #$0f                    ;2         *
    bne     Lf2f8                   ;2/3       *
    sta     ram_E0                  ;3         *
    lda     ram_DF                  ;3         *
    ora     #$02                    ;2         *
    sta     ram_DF                  ;3         *
    bne     Lf2f8                   ;2/3 =  20 *
Lf29b
    lda     #$08                    ;2        
    ora     ram_A2                  ;3        
    sta     ram_A2                  ;3        
    lda     ram_DE                  ;3        
    bmi     Lf2f8                   ;2/3      
    lda     ram_DD                  ;3        
    beq     Lf28b                   ;2/3      
    dec     ram_DD                  ;5        
    bne     Lf2b0                   ;2/3      
    jsr     Lfca5                   ;6   =  31
Lf2b0
    lda     ram_DF                  ;3        
    cmp     #$20                    ;2        
    bcs     Lf2c0                   ;2/3      
    and     #$0f                    ;2        
    ora     #$10                    ;2        
    sta     ram_DF                  ;3        
    lda     #$0a                    ;2        
    sta     ram_E1                  ;3   =  19
Lf2c0
    lda     #$4f                    ;2        
    sec                             ;2        
    sbc     ram_90                  ;3        
    bcs     Lf2cd                   ;2/3      
    inc     ram_C4,x                ;6        
    eor     #$ff                    ;2        
    adc     #$01                    ;2   =  19
Lf2cd
    sta     ram_82                  ;3        
    lda     ram_8E                  ;3        
    sec                             ;2        
    sbc     #$01                    ;2        
    cmp     ram_82                  ;3        
    bcc     Lf2e0                   ;2/3      
    sta     ram_83                  ;3        
    lda     #$c0                    ;2        
    sta     ram_B5,x                ;4        
    bne     Lf2ea                   ;2/3 =  26
Lf2e0
    ldy     ram_82                  ;3        
    sta     ram_82                  ;3        
    sty     ram_83                  ;3        
    lda     #$80                    ;2        
    sta     ram_B5,x                ;4   =  15
Lf2ea
    lda     ram_83                  ;3        
    sta     ram_B8,x                ;4        
    sta     ram_BB,x                ;4        
    lda     ram_82                  ;3        
    sta     ram_BE,x                ;4        
    lda     #$00                    ;2        
    sta     ram_C1,x                ;4   =  24
Lf2f8
    lda     SWCHB                   ;4        
    asl                             ;2        
    ldx     ram_F5                  ;3        
    bne     Lf301                   ;2/3      
    asl                             ;2   =  13
Lf301
    ldy     #$02                    ;2        
    bcc     Lf306                   ;2/3      
    dey                             ;2   =   6 *
Lf306
    sty     ram_DA                  ;3        
    ldx     #$02                    ;2   =   5
Lf30a
    lda     ram_B5,x                ;4        
    bmi     Lf311                   ;2/3 =   6
Lf30e
    jmp     Lf394                   ;3   =   3
    
Lf311
    and     #$20                    ;2        
    bne     Lf30e                   ;2/3      
    lda     ram_C1,x                ;4        
    clc                             ;2        
    adc     ram_BE,x                ;4        
    sta     ram_C1,x                ;4        
    cmp     ram_BB,x                ;4        
    bcc     Lf34b                   ;2/3      
    sbc     ram_BB,x                ;4        
    sta     ram_C1,x                ;4        
    lda     ram_B5,x                ;4        
    and     #$40                    ;2        
    bne     Lf334                   ;2/3      
    lda     ram_C7,x                ;4        
    clc                             ;2        
    adc     ram_DA                  ;3        
    sta     ram_C7,x                ;4        
    jmp     Lf34b                   ;3   =  56
    
Lf334
    lda     #$4f                    ;2        
    cmp     ram_C4,x                ;4        
    bcs     Lf344                   ;2/3      
    lda     ram_C4,x                ;4        
    clc                             ;2        
    adc     ram_DA                  ;3        
    sta     ram_C4,x                ;4        
    jmp     Lf34b                   ;3   =  24
    
Lf344
    lda     ram_C4,x                ;4        
    sec                             ;2        
    sbc     ram_DA                  ;3        
    sta     ram_C4,x                ;4   =  13
Lf34b
    lda     ram_B5,x                ;4        
    and     #$40                    ;2        
    beq     Lf35b                   ;2/3      
    lda     ram_C7,x                ;4        
    clc                             ;2        
    adc     ram_DA                  ;3        
    sta     ram_C7,x                ;4        
    jmp     Lf372                   ;3   =  24
    
Lf35b
    lda     #$4f                    ;2        
    cmp     ram_C4,x                ;4        
    bcs     Lf36b                   ;2/3      
    lda     ram_C4,x                ;4        
    clc                             ;2        
    adc     ram_DA                  ;3        
    sta     ram_C4,x                ;4        
    jmp     Lf372                   ;3   =  24
    
Lf36b
    lda     ram_C4,x                ;4         *
    sec                             ;2         *
    sbc     ram_DA                  ;3         *
    sta     ram_C4,x                ;4   =  13 *
Lf372
    lda     ram_B8,x                ;4        
    sec                             ;2        
    sbc     ram_DA                  ;3        
    sta     ram_B8,x                ;4        
    bcc     Lf37d                   ;2/3      
    bne     Lf394                   ;2/3 =  17
Lf37d
    lda     #$20                    ;2        
    ora     ram_B5,x                ;4        
    sta     ram_B5,x                ;4        
    ldy     #$00                    ;2        
    sty     ram_9E,x                ;4        
    lda     ram_C4,x                ;4        
    sec                             ;2        
    sbc     #$04                    ;2        
    sta     ram_A6,x                ;4        
    lda     ram_C7,x                ;4        
    sbc     #$04                    ;2        
    sta     ram_A3,x                ;4   =  38
Lf394
    dex                             ;2        
    bmi     Lf39a                   ;2/3      
    jmp     Lf30a                   ;3   =   7
    
Lf39a
    ldx     ram_CA                  ;3        
    ldy     #$54                    ;2        
    lda     ram_C4,x                ;4        
    sta     ram_89                  ;3        
    lda     ram_B5,x                ;4        
    and     #$20                    ;2        
    bne     Lf3aa                   ;2/3      
    ldy     ram_C7,x                ;4   =  24
Lf3aa
    sty     ram_84                  ;3        
    lda     ram_DE                  ;3        
    cmp     #$a0                    ;2        
    bcs     Lf3ce                   ;2/3      
    ldy     #$97                    ;2        
    cpy     ram_90                  ;3        
    bcc     Lf3be                   ;2/3      
    ldy     #$09                    ;2        
    cpy     ram_90                  ;3        
    bcc     Lf3c0                   ;2/3 =  24
Lf3be
    sty     ram_90                  ;3   =   3
Lf3c0
    ldy     #$0a                    ;2        
    cpy     ram_8E                  ;3        
    bcs     Lf3cc                   ;2/3      
    ldy     #$53                    ;2        
    cpy     ram_8E                  ;3        
    bcs     Lf3ce                   ;2/3 =  14
Lf3cc
    sty     ram_8E                  ;3   =   3
Lf3ce
    lda     ram_90                  ;3        
    sta     ram_8A                  ;3        
    lda     ram_8E                  ;3        
    sta     ram_85                  ;3        
    ldy     ram_92                  ;3        
    lda     ram_98                  ;3        
    and     #$02                    ;2        
    beq     Lf3e2                   ;2/3      
    tya                             ;2         *
    ora     #$10                    ;2         *
    tay                             ;2   =  28 *
Lf3e2
    lda     ram_91                  ;3        
    sty     ram_91                  ;3        
    sta     ram_92                  ;3        
    lda     ram_93                  ;3        
    ldy     ram_94                  ;3        
    sty     ram_93                  ;3        
    sta     ram_94                  ;3        
    lda     ram_97                  ;3        
    ldy     ram_98                  ;3        
    sty     ram_97                  ;3        
    sta     ram_98                  ;3        
    lda     ram_95                  ;3        
    ldy     ram_96                  ;3        
    sty     ram_95                  ;3        
    sta     ram_96                  ;3        
    lda     ram_9A                  ;3        
    sta     ram_88                  ;3        
    ldy     ram_99                  ;3        
    sty     ram_9A                  ;3        
    sta     ram_99                  ;3        
    lda     ram_91                  ;3        
    cmp     #$ff                    ;2        
    bne     Lf440                   ;2/3      
    ldx     #$0a                    ;2   =  69
Lf412
    lda     ram_A9,x                ;4        
    cmp     #$25                    ;2        
    beq     Lf422                   ;2/3      
    dex                             ;2        
    dex                             ;2        
    bpl     Lf412                   ;2/3      
    lda     ram_D8                  ;3        
    and     #$07                    ;2        
    bpl     Lf43e                   ;2/3 =  21
Lf422
    lda     ram_D8                  ;3        
    and     #$07                    ;2        
    cmp     #$06                    ;2        
    bcs     Lf43e                   ;2/3      
    asl                             ;2        
    tax                             ;2   =  13
Lf42c
    lda     ram_A9,x                ;4        
    cmp     #$25                    ;2        
    beq     Lf43c                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    cpx     #$0c                    ;2        
    bne     Lf42c                   ;2/3      
    ldx     #$00                    ;2         *
    beq     Lf42c                   ;2/3 =  20 *
Lf43c
    txa                             ;2        
    lsr                             ;2   =   4
Lf43e
    sta     ram_95                  ;3   =   3
Lf440
    lda     ram_80                  ;3        
    and     #$01                    ;2        
    tax                             ;2        
    lda     ram_9C,x                ;4        
    clc                             ;2        
    adc     ram_9B                  ;3        
    sta     ram_9C,x                ;4        
    bcc     Lf470                   ;2/3      
    lda     ram_93                  ;3        
    cmp     #$54                    ;2        
    beq     Lf470                   ;2/3      
    lda     ram_97                  ;3        
    and     #$02                    ;2        
    beq     Lf46e                   ;2/3      
    bit     ram_F8                  ;3         *
    bvc     Lf46e                   ;2/3       *
    lda     ram_80                  ;3         *
    and     #$01                    ;2         *
    tax                             ;2         *
    lda     ram_F9,x                ;4         *
    beq     Lf46e                   ;2/3       *
    inc     ram_93                  ;5         *
    inc     ram_93                  ;5         *
    dec     ram_F9,x                ;6         *
    .byte   $2c ;bit                ;4-5 =  69 *
Lf46e
    dec     ram_93                  ;5   =   5
Lf470
    ldx     ram_CA                  ;3        
    dex                             ;2        
    bpl     Lf477                   ;2/3      
    ldx     #$02                    ;2   =   9
Lf477
    stx     ram_CA                  ;3        
    lda     ram_80                  ;3        
    and     #$0f                    ;2        
    sta     ram_DA                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    ora     ram_DA                  ;3        
    sta     ram_D7                  ;3        
    ldy     #$5d                    ;2        
    lda     ram_B5,x                ;4        
    and     #$20                    ;2        
    beq     Lf495                   ;2/3      
    lda     ram_9E,x                ;4        
    tax                             ;2        
    ldy     Lff15,x                 ;4   =  45
Lf495
    sty     ram_8B                  ;3        
    lda     #$ff                    ;2        
    sta     ram_8C                  ;3        
    lda     ram_A2                  ;3        
    and     #$7f                    ;2        
    sta     ram_A2                  ;3        
    ldy     #$10                    ;2        
    lda     #$f8                    ;2        
    cpx     #$04                    ;2        
    bcc     Lf4b7                   ;2/3      
    cpx     #$0c                    ;2        
    bcs     Lf4b7                   ;2/3      
    lda     ram_A2                  ;3        
    ora     #$80                    ;2        
    sta     ram_A2                  ;3        
    lda     #$f0                    ;2        
    ldy     #$15                    ;2   =  40
Lf4b7
    sta     ram_A1                  ;3        
    sty     ram_EE                  ;3        
    ldx     ram_CA                  ;3        
    lda     ram_A3,x                ;4        
    bit     ram_A2                  ;3        
    bpl     Lf4c6                   ;2/3      
    clc                             ;2        
    adc     #$fc                    ;2   =  22
Lf4c6
    sta     ram_83                  ;3        
    lda     ram_80                  ;3        
    and     #$07                    ;2        
    bne     Lf4e7                   ;2/3      
    ldx     #$02                    ;2   =  12
Lf4d0
    inc     ram_9E,x                ;6        
    lda     ram_9E,x                ;4        
    cmp     #$10                    ;2        
    bne     Lf4e4                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_B5,x                ;4        
    lda     #$4f                    ;2        
    sta     ram_C4,x                ;4        
    lda     #$01                    ;2        
    sta     ram_C7,x                ;4   =  32
Lf4e4
    dex                             ;2        
    bpl     Lf4d0                   ;2/3 =   4
Lf4e7
    ldx     #$0a                    ;2   =   2
Lf4e9
    lda     ram_A9,x                ;4        
    cmp     #$25                    ;2        
    beq     Lf504                   ;2/3!     
    cmp     #$5b                    ;2        
    beq     Lf504                   ;2/3!     
    cmp     #$f1                    ;2         *
    beq     Lf504                   ;2/3!      *
    lda     ram_80                  ;3         *
    and     #$0f                    ;2         *
    bne     Lf504                   ;2/3!      *
    lda     ram_A9,x                ;4         *
    clc                             ;2         *
    adc     #$09                    ;2         *
    sta     ram_A9,x                ;4   =  35 *
Lf504
    dex                             ;2        
    dex                             ;2        
    bpl     Lf4e9                   ;2/3!     
    lda     #$38                    ;2        
    sta     ram_86                  ;3        
    lda     #$40                    ;2        
    sta     ram_87                  ;3        
    lda     ram_A1                  ;3        
    sta     ram_DA                  ;3        
    lda     #$00                    ;2        
    sta     ram_A1                  ;3        
    jsr     Lf000                   ;6        
    lda     ram_DA                  ;3        
    sta     ram_A1                  ;3        
    ldx     #$00                    ;2        
    lda     ram_DF                  ;3        
    and     #$0f                    ;2        
    asl                             ;2        
    tay                             ;2        
    lda     Lf531,y                 ;4        
    pha                             ;3        
    lda     Lf530,y                 ;4        
    pha                             ;3        
    rts                             ;6   =  70
    
Lf530
    .byte   $ea                             ; $f530 (A)
Lf531
    .byte   $f5,$d4,$f5                     ; $f531 (D)
    .byte   $8f,$f5,$77,$f5                 ; $f534 (*)
    .byte   $5f,$f5,$65,$f5                 ; $f538 (D)
    .byte   $3f,$f5,$ae,$f5,$a5,$e0,$29,$03 ; $f53c (*)
    .byte   $d0,$06,$a5,$d8,$29,$07,$85,$e1 ; $f544 (*)
    .byte   $a0,$0c,$a2,$08,$a5,$e1,$c6,$e0 ; $f54c (*)
    .byte   $d0,$56,$a9,$01,$85,$df,$a9,$04 ; $f554 (*)
    .byte   $85,$e0,$d0,$4c                 ; $f55c (*)
    
Lf560
    dec     ram_E0                  ;5        
    lda     ram_E0                  ;3        
    bcc     Lf570                   ;2/3      
    lda     ram_80                  ;3        
    and     #$0f                    ;2        
    cmp     #$03                    ;2        
    bcs     Lf5ac                   ;2/3      
    eor     #$07                    ;2   =  21
Lf570
    asl                             ;2        
    tax                             ;2        
    ldy     #$08                    ;2        
    lda     #$18                    ;2        
    bne     Lf5ac                   ;2/3      
    lda     ram_80                  ;3         *
    ldx     ram_E0                  ;3         *
    and     #$1f                    ;2         *
    bne     Lf586                   ;2/3       *
    cpx     #$0e                    ;2         *
    beq     Lf586                   ;2/3       *
    inc     ram_E0                  ;5   =  29 *
Lf586
    lda     ram_80                  ;3         *
    and     #$03                    ;2         *
    ora     #$08                    ;2         *
    ldy     #$05                    ;2         *
    bne     Lf5eb                   ;2/3       *
    ldy     ram_E0                  ;3         *
    lda     Lfe36,y                 ;4         *
    ldx     #$08                    ;2         *
    iny                             ;2         *
    sty     ram_E0                  ;3         *
    cpy     #$08                    ;2         *
    beq     Lf5a2                   ;2/3       *
    ldy     #$05                    ;2         *
    bne     Lf5ac                   ;2/3 =  33 *
Lf5a2
    lda     ram_DF                  ;3         *
    and     #$f0                    ;2         *
    sta     ram_DF                  ;3         *
    ldx     #$00                    ;2         *
    stx     ram_E0                  ;3   =  13 *
Lf5ac
    jmp     Lf5eb                   ;3   =   3
    
    .byte   $a0,$08,$84,$15,$a5,$80,$29,$0f ; $f5af (*)
    .byte   $d0,$10,$e6,$e0,$a5,$e0,$c9,$10 ; $f5b7 (*)
    .byte   $d0,$08,$a9,$30,$85,$df,$a2,$50 ; $f5bf (*)
    .byte   $86,$e1,$a6,$e0,$86,$19,$8a,$85 ; $f5c7 (*)
    .byte   $17,$49,$ff,$4c,$62,$f6         ; $f5cf (*)
    
Lf5d5
    lda     ram_DE                  ;3        
    cmp     #$a0                    ;2        
    bcs     Lf5eb                   ;2/3      
    ldx     ram_E0                  ;3        
    txa                             ;2        
    inx                             ;2        
    cpx     #$14                    ;2        
    bne     Lf5e5                   ;2/3      
    ldx     #$04                    ;2   =  20
Lf5e5
    stx     ram_E0                  ;3        
    ldy     #$0c                    ;2        
    ldx     #$08                    ;2   =   7
Lf5eb
    stx     AUDV0                   ;3        
    sty     AUDC0                   ;3        
    sta     AUDF0                   ;3        
    ldx     #$00                    ;2        
    lda     ram_DF                  ;3        
    and     #$f0                    ;2        
    cmp     #$10                    ;2        
    beq     Lf622                   ;2/3!     
    cmp     #$20                    ;2        
    beq     Lf64c                   ;2/3!     
    cmp     #$30                    ;2        
    bcc     Lf662                   ;2/3      
    dec     ram_E1                  ;5         *
    beq     Lf634                   ;2/3       *
    lda     ram_E1                  ;3         *
    and     #$70                    ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    ldx     Lf61d,y                 ;4         *
    ldy     #$08                    ;2         *
    lda     ram_E1                  ;3         *
    and     #$0f                    ;2         *
    ora     #$10                    ;2         *
    bne     Lf662                   ;2/3 =  65 *
Lf61d
    .byte   $02 ;.JAM               ;0         *
    NOP     COLUP0                  ;3         *
    php                             ;3         *
    .byte   $0e ;asl                ;6-2 =  10 *
Lf622
    ldy     #$08                    ;2        
    ldx     #$06                    ;2        
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    bne     Lf630                   ;2/3      
    dec     ram_E1                  ;5        
    beq     Lf634                   ;2/3 =  18
Lf630
    lda     ram_E1                  ;3        
    bne     Lf662                   ;2/3 =   5
Lf634
    lda     ram_DF                  ;3        
    and     #$0f                    ;2        
    sta     ram_DF                  ;3        
    bit     ram_FB                  ;3        
    bpl     Lf64a                   ;2/3      
    lda     #$c3                    ;2         *
    sta     ram_DB                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_FB                  ;3         *
    sta     ram_E7                  ;3         *
    sta     ram_E8                  ;3   =  29 *
Lf64a
    bpl     Lf662                   ;2/3 =   2
Lf64c
    ldy     ram_E1                  ;3        
    ldx     Lfe3e,y                 ;4        
    lda     ram_80                  ;3        
    and     #$07                    ;2        
    bne     Lf65e                   ;2/3      
    iny                             ;2        
    sty     ram_E1                  ;3        
    cpy     #$10                    ;2        
    beq     Lf634                   ;2/3 =  23
Lf65e
    lda     #$1f                    ;2        
    ldy     #$08                    ;2   =   4
Lf662
    stx     AUDV1                   ;3        
    sty     AUDC1                   ;3        
    sta     AUDF1                   ;3        
    ldy     #$00                    ;2        
    ldx     #$0f                    ;2        
    lda     SWCHB                   ;4        
    and     #$08                    ;2        
    beq     Lf675                   ;2/3      
    ldx     #$ff                    ;2   =  23
Lf675
    bit     ram_DB                  ;3        
    bpl     Lf67f                   ;2/3      
    txa                             ;2        
    and     #$f7                    ;2        
    tax                             ;2        
    ldy     #$ff                    ;2   =  13
Lf67f
    stx     ram_86                  ;3        
    tya                             ;2        
    and     ram_E8                  ;3        
    sta     ram_DA                  ;3        
    ldx     #$00                    ;2        
    ldy     #$00                    ;2        
    lda     ram_E7                  ;3        
    cmp     #$ff                    ;2        
    beq     Lf69b                   ;2/3      
    and     #$0e                    ;2        
    lsr                             ;2        
    sta     ram_82                  ;3        
    asl                             ;2        
    asl                             ;2        
    clc                             ;2        
    adc     ram_82                  ;3        
    tay                             ;2   =  40
Lf69b
    lda     Lfe8a,y                 ;4        
    eor     ram_DA                  ;3        
    and     ram_86                  ;3        
    sta     ram_E2,x                ;4        
    inx                             ;2        
    iny                             ;2        
    cpx     #$05                    ;2        
    bne     Lf69b                   ;2/3      
    bit     ram_FB                  ;3        
    bmi     Lf6c1                   ;2/3      
    ldy     ram_DF                  ;3        
    cpy     #$30                    ;2        
    bcc     Lf6c1                   ;2/3      
    lda     ram_E1                  ;3         *
    and     ram_86                  ;3         *
    cpy     #$40                    ;2         *
    bcc     Lf6bf                   ;2/3       *
    sta     ram_E2                  ;3         *
    .byte   $2c ;bit                ;4-3 =  48 *
Lf6bf
    sta     ram_E5                  ;3   =   3 *
Lf6c1
    ldx     ram_F5                  ;3        
    lda     ram_DB                  ;3        
    and     #$04                    ;2        
    beq     Lf6d9                   ;2/3      
    lda     ram_E9                  ;3        
    sta     ram_F3,x                ;4        
    ldy     #$00                    ;2        
    sty     ram_F1,x                ;4        
    iny                             ;2        
    cmp     #$18                    ;2        
    bcc     Lf6d7                   ;2/3      
    iny                             ;2   =  31 *
Lf6d7
    sty     ram_EF,x                ;4   =   4
Lf6d9
    lda     ram_EF,x                ;4        
    ldx     #$08                    ;2        
    jsr     Lfd6c                   ;6        
    ldx     ram_F5                  ;3        
    lda     ram_F1,x                ;4        
    ldx     #$04                    ;2        
    jsr     Lfd6c                   ;6        
    ldx     ram_F5                  ;3        
    lda     ram_F3,x                ;4        
    ldx     #$00                    ;2        
    jsr     Lfd6c                   ;6        
    ldy     #$5d                    ;2   =  44
Lf6f4
    lda     ram_CB,x                ;4        
    cmp     #$88                    ;2        
    bne     Lf702                   ;2/3!     
    sty     ram_CB,x                ;4        
    inx                             ;2        
    inx                             ;2        
    cpx     #$0a                    ;2        
    bne     Lf6f4                   ;2/3!=  20
Lf702
    lda     ram_DB                  ;3        
    and     #$04                    ;2        
    beq     Lf70e                   ;2/3      
    sty     ram_D1                  ;3        
    sty     ram_CF                  ;3        
    sty     ram_D3                  ;3   =  16
Lf70e
    lda     ram_D7                  ;3        
    and     ram_86                  ;3        
    sta     ram_D7                  ;3        
    bit     ram_FB                  ;3        
    bpl     Lf724                   ;2/3      
    lda     ram_DF                  ;3         *
    cmp     #$30                    ;2         *
    bne     Lf724                   ;2/3       *
    lda     ram_E1                  ;3         *
    and     ram_86                  ;3         *
    sta     ram_E4                  ;3   =  30 *
Lf724
    lda     ram_E6                  ;3        
    eor     ram_DA                  ;3        
    and     ram_86                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     #$03                    ;2        
    sta     NUSIZ1                  ;3        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    lda     ram_E4                  ;3        
    sta     COLUBK                  ;3        
    lda     ram_8E                  ;3        
    clc                             ;2        
    adc     #$28                    ;2        
    sta     ram_8E                  ;3        
    sta     ram_8D                  ;3        
    lda     ram_90                  ;3        
    adc     #$28                    ;2        
    sta     ram_90                  ;3        
    sta     ram_8F                  ;3   =  56
Lf74b
    lda     INTIM                   ;4        
    bne     Lf74b                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     VBLANK                  ;3        
    sta     ram_81                  ;3        
    sta     HMM0                    ;3        
    sta     HMM1                    ;3        
    sta     HMBL                    ;3        
    lda     #$06                    ;2        
    sta     ram_DA                  ;3   =  20
Lf760
    ldy     ram_DA                  ;3        
    lda     (ram_CB),y              ;5        
    sta     GRP0                    ;3        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    lda     (ram_CD),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_CF),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_D1),y              ;5        
    sta     ram_82                  ;3        
    lda     (ram_D3),y              ;5        
    tax                             ;2        
    lda     (ram_D5),y              ;5        
    tay                             ;2        
    lda     ram_82                  ;3        
    sta     GRP1                    ;3        
    stx     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    dec     ram_DA                  ;5        
    bpl     Lf760                   ;2/3      
    sta     WSYNC                   ;3   =  63
;---------------------------------------
    ldy     #$00                    ;2        
    sty     VDELP0                  ;3        
    sty     VDELP1                  ;3        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    ldy     #$21                    ;2        
    sty     CTRLPF                  ;3        
    lda     ram_80                  ;3        
    and     #$0f                    ;2        
    sta     COLUPF                  ;3        
    lda     SWCHA                   ;4        
    ldy     ram_E9                  ;3        
    cpy     #$35                    ;2        
    bcs     Lf7ab                   ;2/3      
    ldy     ram_F5                  ;3        
    bne     Lf7af                   ;2/3 =  43
Lf7ab
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2   =   8
Lf7af
    tay                             ;2        
    and     #$03                    ;2        
    sta     ram_CE                  ;3        
    tya                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    and     #$03                    ;2        
    sta     ram_CC                  ;3        
    sta     WSYNC                   ;3   =  21
;---------------------------------------
    lda     ram_E3                  ;3        
    sta     COLUP0                  ;3        
    lda     ram_E4                  ;3        
    sta     COLUBK                  ;3        
    lda     ram_D7                  ;3        
    sta     COLUP1                  ;3        
    ldy     ram_E6                  ;3        
    lda     ram_97                  ;3        
    and     #$02                    ;2        
    bne     Lf7da                   ;2/3      
    tay                             ;2        
    lda     ram_80                  ;3        
    and     #$08                    ;2        
    bne     Lf7da                   ;2/3      
    ldy     #$0f                    ;2   =  39
Lf7da
    tya                             ;2        
    and     ram_86                  ;3        
    sta     ram_D7                  ;3        
    ldx     ram_CA                  ;3        
    lda     ram_A6,x                ;4        
    sta     ram_87                  ;3        
    lda     ram_91                  ;3        
    sta     NUSIZ0                  ;3        
    lda     ram_EE                  ;3        
    sta     NUSIZ1                  ;3        
    ldx     #$01                    ;2        
    lda     #$36                    ;2        
    sta     TIM8T                   ;4        
    jsr     Lf002                   ;6        
    lda     ram_97                  ;3        
    asl                             ;2        
    sta     ram_D0                  ;3        
    ldy     #$34                    ;2        
    cpy     ram_E9                  ;3        
    bcc     Lf805                   ;2/3      
    lda     ram_F5                  ;3        
    lsr                             ;2   =  64
Lf805
    ror     ram_D0                  ;5        
    lda     ram_DE                  ;3        
    cmp     #$a0                    ;2        
    ror     ram_D0                  ;5   =  15
Lf80d
    lda     INTIM                   ;4        
    bne     Lf80d                   ;2/3      
    sta     HMP1                    ;3        
    ldx     #$53                    ;2        
    bne     Lf83c                   ;2/3 =  13
Lf818
    bvc     Lf81e                   ;2/3      
    lsr                             ;2         *
    lsr                             ;2         *
    bpl     Lf821                   ;2/3 =   8 *
Lf81e
    asl                             ;2        
    rol                             ;2        
    rol                             ;2   =   6
Lf821
    ldy     ram_CC                  ;3        
    and     #$03                    ;2        
    sta     ram_CC                  ;3        
    eor     Lfe0b,y                 ;4        
    beq     Lf838                   ;2/3      
    eor     #$03                    ;2        
    bne     Lf878                   ;2/3      
    dec     ram_8E                  ;5        
    bne     Lf878                   ;2/3 =  25
Lf834
    dec     ram_90                  ;5        
    bne     Lf878                   ;2/3 =   7
Lf838
    inc     ram_8E                  ;5        
    bne     Lf878                   ;2/3 =   7
Lf83c
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    cpx     ram_93                  ;3        
    bne     Lf84c                   ;2/3      
    lda     #$02                    ;2        
    ldy     ram_D7                  ;3        
    sty     COLUP0                  ;3        
    bcs     Lf854                   ;2/3 =  18
Lf84c
    lda     #$01                    ;2        
    bit     ram_D0                  ;3        
    bne     Lf854                   ;2/3      
    adc     #$00                    ;2   =   9
Lf854
    sta     ENAM0                   ;3        
    bit     ram_D0                  ;3        
    bmi     Lf878                   ;2/3      
    txa                             ;2        
    lsr                             ;2        
    lda     SWCHA                   ;4        
    bcc     Lf818                   ;2/3      
    bvs     Lf867                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2   =  28
Lf867
    ldy     ram_CE                  ;3        
    and     #$03                    ;2        
    sta     ram_CE                  ;3        
    eor     Lfe0b,y                 ;4        
    beq     Lf834                   ;2/3      
    eor     #$03                    ;2        
    bne     Lf878                   ;2/3      
    inc     ram_90                  ;5   =  23
Lf878
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    txa                             ;2        
    ldx     #$1f                    ;2        
    txs                             ;2        
    tax                             ;2        
    cpx     ram_85                  ;3        
    php                             ;3        
    sec                             ;2        
    sbc     ram_83                  ;3        
    bit     ram_A2                  ;3        
    bpl     Lf88a                   ;2/3      
    lsr                             ;2   =  26
Lf88a
    tay                             ;2        
    and     #$f8                    ;2        
    beq     Lf892                   ;2/3      
    lda     #$00                    ;2        
    .byte   $2c ;bit                ;4-5 =   7
Lf892
    lda     (ram_8B),y              ;5        
    sta     GRP1                    ;3        
    lda     ram_81                  ;3        
    clc                             ;2        
    adc     ram_95                  ;3        
    sta     ram_81                  ;3        
    ldy     #$00                    ;2        
    bcc     Lf8a3                   ;2/3      
    ldy     ram_97                  ;3   =  26
Lf8a3
    sty     HMM0                    ;3        
    cpx     ram_84                  ;3        
    php                             ;3        
    dex                             ;2        
    bne     Lf83c                   ;2/3      
    sta     WSYNC                   ;3   =  16
;---------------------------------------
    lda     #$02                    ;2        
    ldx     ram_84                  ;3        
    dex                             ;2        
    bne     Lf8b5                   ;2/3      
    .byte   $24 ;bit                ;3-2 =  10
Lf8b5
    lsr                             ;2        
    nop                             ;2        
    sta     ENAM1                   ;3        
    lsr                             ;2        
    sta     ENAM0                   ;3        
    sta     ENAM0                   ;3        
    sta     RESP0                   ;3        
    lda     ram_E5                  ;3        
    sta     COLUPF                  ;3        
    lda     ram_E2                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sta     COLUP1                  ;3        
    ldy     #$08                    ;2        
    lda     #$30                    ;2        
    ldx     #$84                    ;2        
    sta     RESP1                   ;3        
    sty     ENAM1                   ;3        
    sta     PF0                     ;3        
    lda     #$03                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3   =  59
Lf8de
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     Lff7f,y                 ;4        
    sta     PF2                     ;3        
    lda     (ram_AD),y              ;5        
    tax                             ;2        
    lda     (ram_A9),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_AB),y              ;5        
    nop                             ;2        
    sta     GRP0                    ;3        
    lda     CXM0P|$10               ;3        
    stx     GRP0                    ;3        
    lda     (ram_B3),y              ;5        
    tax                             ;2        
    lda     (ram_AF),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_B1),y              ;5        
    sta     GRP1                    ;3        
    nop                             ;2        
    stx     GRP1                    ;3        
    dey                             ;2        
    bpl     Lf8de                   ;2/3!     
    sta     WSYNC                   ;3   =  73
;---------------------------------------
    lda     #$05                    ;2        
    sta     NUSIZ1                  ;3        
    lda     ram_E5                  ;3        
    sta     COLUBK                  ;3        
    sta     COLUBK                  ;3        
    iny                             ;2        
    sty     PF2                     ;3        
    sty     PF0                     ;3        
    sty     GRP0                    ;3        
    ldy     ram_DD                  ;3        
    lda     Lff00,y                 ;4        
    sta     ram_8B                  ;3        
    lda     #$ff                    ;2        
    sta     ram_8C                  ;3        
    ldy     #$03                    ;2        
    sta     RESP1                   ;3        
    lda     ram_E4                  ;3        
    sta     COLUP1                  ;3        
    sta     COLUPF                  ;3        
    lda     ram_DB                  ;3        
    and     #$03                    ;2        
    tax                             ;2   =  61
Lf933
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     (ram_8B),y              ;5        
    sta     GRP1                    ;3        
    lda     Lfe61,x                 ;4        
    sta     PF0                     ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    dey                             ;2        
    sta     PF0                     ;3        
    bpl     Lf933                   ;2/3      
    sta     WSYNC                   ;3   =  33
;---------------------------------------
    lda     ram_E5                  ;3        
    sta     COLUBK                  ;3        
    lda     #$1a                    ;2        
    sta     TIM64T                  ;4        
    iny                             ;2        
    sty     GRP1                    ;3        
    ldx     #$ff                    ;2        
    txs                             ;2        
    ldx     #$03                    ;2   =  23
Lf95a
    dex                             ;2        
    lda     ram_F8                  ;3        
    lsr                             ;2        
    lda     ram_8E,x                ;4        
    bcs     Lf96d                   ;2/3      
    adc     ram_8D,x                ;4        
    ror                             ;2        
    bit     ram_FB                  ;3        
    bvs     Lf96c                   ;2/3      
    adc     ram_8E,x                ;4        
    ror                             ;2   =  30
Lf96c
    sec                             ;2   =   2
Lf96d
    sbc     #$28                    ;2        
    bcs     Lf972                   ;2/3      
    txa                             ;2   =   6
Lf972
    sta     ram_8E,x                ;4        
    dex                             ;2        
    bpl     Lf95a                   ;2/3      
    stx     ram_D0                  ;3        
    stx     ram_CE                  ;3        
    stx     ram_CC                  ;3        
    lda     ram_91                  ;3        
    cmp     #$ff                    ;2        
    bne     Lf998                   ;2/3      
    bit     ram_DE                  ;3        
    bmi     Lf998                   ;2/3      
    jsr     Lfa83                   ;6        
    bcc     Lf995                   ;2/3      
    lda     ram_94                  ;3        
    cmp     #$54                    ;2        
    bne     Lf995                   ;2/3      
    jsr     Lfcea                   ;6   =  50
Lf995
    jmp     Lfa73                   ;3   =   3
    
Lf998
    ldy     ram_93                  ;3        
    bne     Lf9a2                   ;2/3      
    jsr     Lfbaf                   ;6         *
    jmp     Lfa73                   ;3   =  14 *
    
Lf9a2
    and     #$07                    ;2        
    tax                             ;2        
    lda     Lfdd5,x                 ;4        
    sta     ram_88                  ;3        
    ldy     Lfddd,x                 ;4        
    lda     Lfe05,y                 ;4        
    sta     ram_8B                  ;3        
    lda     #$fe                    ;2        
    sta     ram_8C                  ;3        
    lda     #$53                    ;2        
    sec                             ;2        
    sbc     ram_93                  ;3        
    sta     ram_81                  ;3        
    lda     ram_95                  ;3        
    sta     ram_DA                  ;3        
    jsr     Lfc6f                   ;6   =  49
Lf9c4
    ldy     ram_88                  ;3        
    lda     ram_99                  ;3        
    clc                             ;2        
    adc     (ram_8B),y              ;5        
    ldy     ram_93                  ;3        
    sty     ram_83                  ;3        
    ldy     ram_97                  ;3        
    cpy     #$f0                    ;2        
    bcc     Lf9db                   ;2/3      
    clc                             ;2        
    adc     ram_87                  ;3        
    jmp     Lf9de                   ;3   =  34
    
Lf9db
    sec                             ;2        
    sbc     ram_87                  ;3   =   5
Lf9de
    sta     ram_82                  ;3        
    ldx     ram_CA                  ;3        
    lda     ram_B5,x                ;4        
    and     #$20                    ;2        
    beq     Lfa1f                   ;2/3!     
    ldy     ram_C7,x                ;4        
    lda     ram_C4,x                ;4        
    tax                             ;2        
    jsr     Lfc44                   ;6        
    sta     ram_82                  ;3        
    ldx     ram_CA                  ;3        
    ldy     ram_9E,x                ;4        
    lda     ram_97                  ;3        
    and     #$02                    ;2        
    beq     Lfa18                   ;2/3!     
    bit     ram_F8                  ;3         *
    bvc     Lfa18                   ;2/3       *
    lda     Lfdc6,y                 ;4         *
    cmp     ram_82                  ;3         *
    bcc     Lfa1f                   ;2/3       *
    lda     ram_82                  ;3         *
    cmp     #$03                    ;2         *
    bcc     Lfa22                   ;2/3       *
    lda     ram_80                  ;3         *
    and     #$01                    ;2         *
    tax                             ;2         *
    lda     #$02                    ;2         *
    sta     ram_F9,x                ;4         *
    bne     Lfa1f                   ;2/3 =  83 *
Lfa18
    lda     Lfdc6,y                 ;4        
    cmp     ram_82                  ;3        
    bcs     Lfa22                   ;2/3 =   9
Lfa1f
    jmp     Lfa6c                   ;3   =   3
    
Lfa22
    ldy     #$00                    ;2        
    lda     ram_97                  ;3        
    and     #$02                    ;2        
    beq     Lfa2b                   ;2/3      
    iny                             ;2   =  11 *
Lfa2b
    lda     #$25                    ;2        
    jsr     Lfcc1                   ;6        
    lda     ram_DF                  ;3        
    cmp     #$30                    ;2        
    bcs     Lfa40                   ;2/3      
    and     #$0f                    ;2        
    ora     #$20                    ;2        
    sta     ram_DF                  ;3        
    lda     #$00                    ;2        
    sta     ram_E1                  ;3   =  27
Lfa40
    lda     ram_8B                  ;3        
    clc                             ;2        
    adc     #$0d                    ;2        
    sta     ram_8B                  ;3        
    ldy     ram_88                  ;3        
    lda     (ram_8B),y              ;5        
    sta     ram_91                  ;3        
    cmp     #$ff                    ;2        
    bne     Lfa57                   ;2/3      
    lda     #$55                    ;2        
    sta     ram_93                  ;3        
    bne     Lfa73                   ;2/3 =  32
Lfa57
    lda     ram_8B                  ;3        
    clc                             ;2        
    adc     #$0d                    ;2        
    sta     ram_8B                  ;3        
    lda     ram_99                  ;3        
    clc                             ;2        
    adc     (ram_8B),y              ;5        
    sta     ram_99                  ;3        
    lda     ram_8B                  ;3        
    sec                             ;2        
    sbc     #$1a                    ;2        
    sta     ram_8B                  ;3   =  33
Lfa6c
    dec     ram_88                  ;5        
    bmi     Lfa73                   ;2/3      
    jmp     Lf9c4                   ;3   =  10
    
Lfa73
    lda     INTIM                   ;4        
    bne     Lfa73                   ;2/3      
    ldx     #$02                    ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    stx     VBLANK                  ;3        
    sta     COLUBK                  ;3        
    jmp     Lf04e                   ;3   =   9
    
Lfa83
    ldy     #$00                    ;2        
    lda     ram_97                  ;3        
    and     #$02                    ;2        
    beq     Lfa97                   ;2/3      
    lda     ram_98                  ;3         *
    and     #$02                    ;2         *
    bne     Lfa97                   ;2/3       *
    lda     ram_DF                  ;3         *
    and     #$f0                    ;2         *
    sta     ram_DF                  ;3   =  24 *
Lfa97
    sty     ram_97                  ;3        
    bit     ram_DE                  ;3        
    bmi     Lfaa5                   ;2/3      
    lda     ram_DE                  ;3        
    bne     Lfac7                   ;2/3      
    lda     ram_EB                  ;3        
    bne     Lfaab                   ;2/3 =  18
Lfaa5
    lda     #$54                    ;2        
    sta     ram_93                  ;3        
    sec                             ;2        
    rts                             ;6   =  13
    
Lfaab
    ldy     #$02                    ;2         *
    sty     ram_E0                  ;3         *
    ldy     #$ff                    ;2         *
    sty     ram_97                  ;3         *
    lda     ram_80                  ;3         *
    and     #$01                    ;2         *
    tax                             ;2         *
    iny                             ;2         *
    sty     ram_F9,x                ;4         *
    dec     ram_EB                  ;5         *
    lda     ram_DF                  ;3         *
    and     #$f0                    ;2         *
    ora     #$03                    ;2         *
    sta     ram_DF                  ;3         *
    bne     Lfad1                   ;2/3 =  40 *
Lfac7
    lda     ram_D8                  ;3        
    and     #$18                    ;2        
    bne     Lfad1                   ;2/3      
    lda     ram_EB                  ;3        
    bne     Lfaab                   ;2/3 =  12
Lfad1
    ldy     ram_95                  ;3        
    lda     Lfdec,y                 ;4        
    sta     ram_82                  ;3        
    bit     ram_97                  ;3        
    bmi     Lfaec                   ;2/3      
    lda     ram_DE                  ;3        
    cmp     #$04                    ;2        
    bcs     Lfae8                   ;2/3      
    tay                             ;2        
    lda     Lfdfc,y                 ;4        
    bpl     Lfafb                   ;2/3 =  30
Lfae8
    cpy     #$06                    ;2        
    bcc     Lfaf0                   ;2/3 =   4
Lfaec
    lda     #$00                    ;2        
    beq     Lfafb                   ;2/3 =   4
Lfaf0
    lda     ram_D8                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    and     #$07                    ;2        
    tay                             ;2        
    lda     Lfdf4,y                 ;4   =  17
Lfafb
    tay                             ;2        
    sta     ram_91                  ;3        
    lda     Lfde4,y                 ;4        
    sta     ram_83                  ;3        
    lda     ram_D8                  ;3        
    cmp     #$a0                    ;2        
    bcc     Lfb0a                   ;2/3      
    lsr                             ;2   =  21
Lfb0a
    clc                             ;2        
    adc     ram_83                  ;3        
    cmp     #$a0                    ;2        
    bcc     Lfb12                   ;2/3      
    lsr                             ;2   =  11
Lfb12
    sec                             ;2        
    sbc     ram_83                  ;3        
    sta     ram_99                  ;3        
    cmp     ram_82                  ;3        
    bcs     Lfb4b                   ;2/3      
    adc     ram_83                  ;3        
    cmp     ram_82                  ;3        
    bcc     Lfb47                   ;2/3      
    lda     ram_82                  ;3        
    clc                             ;2        
    adc     ram_83                  ;3        
    cmp     #$a0                    ;2        
    bcs     Lfb34                   ;2/3      
    lda     ram_83                  ;3        
    lsr                             ;2        
    clc                             ;2        
    adc     ram_99                  ;3        
    cmp     ram_82                  ;3        
    bcs     Lfb43                   ;2/3 =  48
Lfb34
    lda     ram_82                  ;3        
    cmp     ram_83                  ;3        
    bcc     Lfb43                   ;2/3      
    ldx     #$10                    ;2   =  10
Lfb3c
    lda     ram_83                  ;3        
    clc                             ;2        
    adc     ram_99                  ;3        
    bne     Lfb4f                   ;2/3 =  10
Lfb43
    ldx     #$f0                    ;2        
    bne     Lfb4d                   ;2/3 =   4
Lfb47
    ldx     #$f0                    ;2        
    bne     Lfb3c                   ;2/3 =   4
Lfb4b
    ldx     #$10                    ;2   =   2
Lfb4d
    lda     ram_99                  ;3   =   3
Lfb4f
    stx     ram_DA                  ;3        
    tax                             ;2        
    lda     ram_97                  ;3        
    and     #$02                    ;2        
    ora     ram_DA                  ;3        
    sta     ram_97                  ;3        
    txa                             ;2        
    ldy     #$53                    ;2        
    sty     ram_93                  ;3        
    sec                             ;2        
    sbc     ram_82                  ;3        
    bcs     Lfb68                   ;2/3      
    eor     #$ff                    ;2        
    adc     #$01                    ;2   =  34
Lfb68
    sty     ram_81                  ;3        
    jsr     Lfb7b                   ;6        
    stx     ram_87                  ;3        
    ldx     #$00                    ;2        
    sta     ram_DA                  ;3        
    jsr     Lfb7f                   ;6        
    stx     ram_86                  ;3        
    jmp     Lfb99                   ;3   =  29
    
Lfb7b
    sta     ram_DA                  ;3        
    lda     #$00                    ;2   =   5
Lfb7f
    ldy     #$07                    ;2   =   2
Lfb81
    rol     ram_DA                  ;5        
    rol                             ;2        
    bcs     Lfb94                   ;2/3      
    cmp     ram_81                  ;3        
    bcc     Lfb8c                   ;2/3      
    sbc     ram_81                  ;3   =  17
Lfb8c
    dey                             ;2        
    bpl     Lfb81                   ;2/3      
    rol     ram_DA                  ;5        
    ldx     ram_DA                  ;3        
    rts                             ;6   =  18
    
Lfb94
    sbc     ram_81                  ;3         *
    sec                             ;2         *
    bcs     Lfb8c                   ;2/3 =   7 *
Lfb99
    lda     ram_86                  ;3        
    sta     ram_95                  ;3        
    lda     ram_97                  ;3        
    and     #$02                    ;2        
    bne     Lfbad                   ;2/3      
    ldy     ram_91                  ;3        
    lda     ram_DE                  ;3        
    clc                             ;2        
    sbc     Lfdd5,y                 ;4        
    sta     ram_DE                  ;3   =  28
Lfbad
    clc                             ;2        
    rts                             ;6   =   8
    
Lfbaf
    lda     ram_91                  ;3         *
    and     #$07                    ;2         *
    tay                             ;2         *
    lda     Lfdd5,y                 ;4         *
    sta     ram_82                  ;3         *
    lda     Lfddd,y                 ;4         *
    tay                             ;2         *
    lda     Lfe05,y                 ;4         *
    sta     ram_8B                  ;3         *
    lda     #$fe                    ;2         *
    sta     ram_8C                  ;3         *
    lda     #$53                    ;2         *
    sta     ram_81                  ;3         *
    lda     ram_95                  ;3         *
    sta     ram_DA                  ;3         *
    jsr     Lfc6f                   ;6   =  49 *
Lfbd1
    ldy     ram_82                  ;3         *
    lda     ram_99                  ;3         *
    clc                             ;2         *
    adc     (ram_8B),y              ;5         *
    ldx     ram_97                  ;3         *
    cpx     #$f0                    ;2         *
    bcs     Lfbe4                   ;2/3       *
    sec                             ;2         *
    sbc     ram_87                  ;3         *
    jmp     Lfbe7                   ;3   =  28 *
    
Lfbe4
    clc                             ;2         *
    adc     ram_87                  ;3   =   5 *
Lfbe7
    sta     ram_81                  ;3         *
    ldy     #$00                    ;2   =   5 *
Lfbeb
    lda     Lfdec,y                 ;4         *
    sec                             ;2         *
    sbc     #$04                    ;2         *
    cmp     ram_81                  ;3         *
    bcs     Lfbfb                   ;2/3       *
    adc     #$08                    ;2         *
    cmp     ram_81                  ;3         *
    bcs     Lfc02                   ;2/3!=  20 *
Lfbfb
    iny                             ;2         *
    cpy     #$07                    ;2         *
    bne     Lfbeb                   ;2/3!      *
    beq     Lfc37                   ;2/3 =   8 *
Lfc02
    cpy     #$06                    ;2         *
    bne     Lfc1c                   ;2/3       *
    lda     #$20                    ;2         *
    sta     ram_DC                  ;3         *
    lda     ram_DF                  ;3         *
    and     #$0f                    ;2         *
    ora     #$30                    ;2         *
    sta     ram_DF                  ;3         *
    lda     #$50                    ;2         *
    sta     ram_E1                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_DD                  ;3         *
    beq     Lfc37                   ;2/3 =  31 *
Lfc1c
    tya                             ;2         *
    asl                             ;2         *
    tax                             ;2         *
    ldy     #$40                    ;2         *
    lda     ram_A9,x                ;4         *
    cmp     #$5b                    ;2         *
    beq     Lfc35                   ;2/3       *
    ldy     #$2e                    ;2         *
    lda     ram_DF                  ;3         *
    and     #$0f                    ;2         *
    ora     #$40                    ;2         *
    sta     ram_DF                  ;3         *
    lda     #$50                    ;2         *
    sta     ram_E1                  ;3   =  33 *
Lfc35
    sty     ram_A9,x                ;4   =   4 *
Lfc37
    dec     ram_82                  ;5         *
    bpl     Lfbd1                   ;2/3!      *
    lda     #$ff                    ;2         *
    sta     ram_91                  ;3         *
    lda     #$55                    ;2         *
    sta     ram_93                  ;3         *
    rts                             ;6   =  23 *
    
Lfc44
    txa                             ;2        
    sec                             ;2        
    sbc     ram_82                  ;3        
    bcs     Lfc4e                   ;2/3      
    eor     #$ff                    ;2        
    adc     #$01                    ;2   =  13
Lfc4e
    sta     ram_82                  ;3        
    tya                             ;2        
    sec                             ;2        
    sbc     ram_83                  ;3        
    bcs     Lfc5a                   ;2/3      
    eor     #$ff                    ;2        
    adc     #$01                    ;2   =  16
Lfc5a
    cmp     ram_82                  ;3        
    bcc     Lfc63                   ;2/3      
    ldx     ram_82                  ;3        
    sta     ram_82                  ;3        
    txa                             ;2   =  13
Lfc63
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_83                  ;3        
    asl                             ;2        
    adc     ram_83                  ;3        
    lsr                             ;2        
    clc                             ;2        
    adc     ram_82                  ;3        
    rts                             ;6   =  25
    
Lfc6f
    lda     #$00                    ;2        
    sta     ram_87                  ;3        
    ldx     #$08                    ;2   =   7
Lfc75
    asl                             ;2        
    rol     ram_87                  ;5        
    asl     ram_81                  ;5        
    bcc     Lfc83                   ;2/3      
    clc                             ;2        
    adc     ram_DA                  ;3        
    bcc     Lfc83                   ;2/3      
    inc     ram_87                  ;5   =  26
Lfc83
    dex                             ;2        
    bne     Lfc75                   ;2/3      
    sta     ram_86                  ;3        
    rts                             ;6   =  13
    
Lfc89
    asl     ram_D8                  ;5        
    rol     ram_D9                  ;5        
    bpl     Lfc91                   ;2/3      
    inc     ram_D8                  ;5   =  17
Lfc91
    lda     ram_D8                  ;3        
    bit     Lfdc7                   ;4        
    beq     Lfc9c                   ;2/3      
    eor     #$01                    ;2        
    sta     ram_D8                  ;3   =  14
Lfc9c
    ora     ram_D9                  ;3        
    bne     Lfca2                   ;2/3      
    inc     ram_D8                  ;5   =  10
Lfca2
    lda     ram_D8                  ;3        
    rts                             ;6   =   9
    
Lfca5
    ldy     #$01                    ;2        
    lda     ram_DB                  ;3        
    and     #$03                    ;2        
    beq     Lfcb7                   ;2/3      
    cmp     #$01                    ;2        
    beq     Lfcb6                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_DD                  ;3        
    rts                             ;6   =  24
    
Lfcb6
    iny                             ;2   =   2
Lfcb7
    tya                             ;2        
    ora     ram_DB                  ;3        
    sta     ram_DB                  ;3        
    lda     #$0a                    ;2        
    sta     ram_DD                  ;3        
    rts                             ;6   =  19
    
Lfcc1
    sta     ram_DA                  ;3        
    ldx     #$05                    ;2        
    lda     ram_E7                  ;3        
    cmp     #$0c                    ;2        
    bcs     Lfccd                   ;2/3      
    lsr                             ;2        
    tax                             ;2   =  16
Lfccd
    stx     ram_82                  ;3   =   3
Lfccf
    ldx     ram_F5                  ;3        
    lda     ram_DA                  ;3        
    sed                             ;2        
    clc                             ;2        
    adc     ram_EF,x                ;4        
    sta     ram_EF,x                ;4        
    tya                             ;2        
    adc     ram_F1,x                ;4        
    sta     ram_F1,x                ;4        
    lda     #$00                    ;2        
    adc     ram_F3,x                ;4        
    sta     ram_F3,x                ;4        
    cld                             ;2        
    dec     ram_82                  ;5        
    bpl     Lfccf                   ;2/3      
    rts                             ;6   =  53
    
Lfcea
    ldx     #$00                    ;2        
    txa                             ;2   =   4
Lfced
    ldy     ram_A9,x                ;4        
    cpy     #$25                    ;2        
    beq     Lfcf4                   ;2/3      
    clc                             ;2   =  10 *
Lfcf4
    rol                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$0c                    ;2        
    bne     Lfced                   ;2/3      
    ldx     ram_F5                  ;3        
    sta     ram_EC,x                ;4   =  17
Lfcff
    ldx     #$ff                    ;2        
    stx     ram_DE                  ;3        
    inx                             ;2        
    stx     ram_90                  ;3        
    stx     ram_8E                  ;3        
    stx     ram_E0                  ;3        
    rts                             ;6   =  22
    
Lfd0b
    bit     ram_F8                  ;3        
    bmi     Lfd2d                   ;2/3      
    lda     ram_EC                  ;3        
    bne     Lfd42                   ;2/3 =  10
Lfd13
    lda     #$07                    ;2         *
    sta     ram_DF                  ;3         *
    ldx     #$ff                    ;2         *
    stx     ram_FB                  ;3         *
    lda     ram_E9                  ;3         *
    cmp     #$13                    ;2         *
    bne     Lfcff                   ;2/3!      *
    lda     ram_F1                  ;3         *
    ora     ram_F3                  ;3         *
    bne     Lfcff                   ;2/3!      *
    lda     #$f1                    ;2         *
    sta     ram_B3                  ;3         *
    bne     Lfcff                   ;2/3!=  32 *
Lfd2d
    lda     ram_EC                  ;3         *
    ora     ram_ED                  ;3         *
    beq     Lfd13                   ;2/3       *
    lda     ram_F5                  ;3         *
    eor     #$01                    ;2         *
    tax                             ;2         *
    lda     ram_EC,x                ;4         *
    bne     Lfd40                   ;2/3       *
    ldx     ram_F5                  ;3         *
    bpl     Lfd46                   ;2/3 =  26 *
Lfd40
    stx     ram_F5                  ;3   =   3 *
Lfd42
    ldx     ram_F5                  ;3        
    bne     Lfd5a                   ;2/3 =   5
Lfd46
    inc     ram_E7                  ;5        
    lda     ram_E7                  ;3        
    cmp     #$10                    ;2        
    bcs     Lfd5a                   ;2/3      
    lda     ram_9B                  ;3        
    adc     #$08                    ;2        
    bit     ram_FB                  ;3        
    bvs     Lfd58                   ;2/3      
    adc     #$05                    ;2   =  24
Lfd58
    sta     ram_9B                  ;3   =   3
Lfd5a
    lda     ram_EC,x                ;4        
    ldx     #$0a                    ;2   =   6
Lfd5e
    lsr                             ;2        
    ldy     #$25                    ;2        
    bcs     Lfd65                   ;2/3      
    ldy     #$5b                    ;2   =   8 *
Lfd65
    sty     ram_A9,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bpl     Lfd5e                   ;2/3      
    rts                             ;6   =  16
    
Lfd6c
    pha                             ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lff0b,y                 ;4        
    sta     ram_CB,x                ;4        
    pla                             ;4        
    and     #$0f                    ;2        
    tay                             ;2        
    lda     Lff0b,y                 ;4        
    sta     ram_CD,x                ;4   =  37
Lfd80
    rts                             ;6   =   6
    
Lfd81
    ldx     ram_F5                  ;3        
    lda     ram_F3,x                ;4        
    cmp     ram_F6,x                ;4        
    beq     Lfd80                   ;2/3      
    lda     ram_EC,x                ;4         *
    cmp     #$3f                    ;2         *
    beq     Lfd80                   ;2/3       *
    lda     #$06                    ;2         *
    sta     ram_DF                  ;3         *
    lda     #$a0                    ;2         *
    sta     ram_E0                  ;3         *
    lda     ram_D8                  ;3         *
    and     #$07                    ;2         *
    cmp     #$06                    ;2         *
    bcc     Lfda1                   ;2/3       *
    sbc     #$04                    ;2   =  42 *
Lfda1
    tay                             ;2         *
    lda     Lfe7c,y                 ;4   =   6 *
Lfda5
    sta     ram_DA                  ;3   =   3 *
Lfda7
    lda     ram_DA                  ;3         *
    and     ram_EC,x                ;4         *
    beq     Lfdb5                   ;2/3       *
    lsr     ram_DA                  ;5         *
    bcc     Lfda7                   ;2/3       *
    lda     #$20                    ;2         *
    bne     Lfda5                   ;2/3 =  20 *
Lfdb5
    lda     ram_F6,x                ;4         *
    sed                             ;2         *
    clc                             ;2         *
    adc     #$01                    ;2         *
    cld                             ;2         *
    sta     ram_F6,x                ;4         *
    lda     ram_EC,x                ;4         *
    ora     ram_DA                  ;3         *
    sta     ram_EC,x                ;4         *
    bne     Lfd81                   ;2/3 =  29 *
    
Lfdc6
    .byte   $01                             ; $fdc6 (D)
Lfdc7
    .byte   $02,$03,$04,$02,$04,$06,$08,$06 ; $fdc7 (D)
    .byte   $04,$02,$04,$03,$02,$01         ; $fdcf (D)
Lfdd5
    .byte   $00,$01,$01,$02,$01             ; $fdd5 (D)
    .byte   $00                             ; $fdda (*)
    .byte   $02,$01                         ; $fddb (D)
Lfddd
    .byte   $00,$01,$03,$02,$05             ; $fddd (D)
    .byte   $00                             ; $fde2 (*)
    .byte   $04                             ; $fde3 (D)
Lfde4
    .byte   $00                             ; $fde4 (D)
    .byte   $10,$20                         ; $fde5 (*)
    .byte   $20,$40                         ; $fde7 (D)
    .byte   $00                             ; $fde9 (*)
    .byte   $40                             ; $fdea (D)
    .byte   $00                             ; $fdeb (*)
Lfdec
    .byte   $18                             ; $fdec (D)
    .byte   $28                             ; $fded (*)
    .byte   $38                             ; $fdee (D)
    .byte   $6a                             ; $fdef (*)
    .byte   $7a,$8a,$50,$50                 ; $fdf0 (D)
Lfdf4
    .byte   $00,$01,$02                     ; $fdf4 (*)
    .byte   $03,$04                         ; $fdf7 (D)
    .byte   $00                             ; $fdf9 (*)
    .byte   $06                             ; $fdfa (D)
    .byte   $04                             ; $fdfb (*)
Lfdfc
    .byte   $00,$00,$02                     ; $fdfc (*)
Lfdff
    .byte   $06,$08,$04,$00,$02,$0a         ; $fdff (D)
Lfe05
    .byte   $0f,$10,$12,$15,$17,$1a         ; $fe05 (D)
Lfe0b
    .byte   $02,$00,$00,$02,$00,$00,$10,$00 ; $fe0b (D)
    .byte   $10,$20,$00,$20,$00,$20,$40,$00 ; $fe13 (D)
    .byte   $40,$ff,$00                     ; $fe1b (D)
    .byte   $00                             ; $fe1e (*)
    .byte   $01                             ; $fe1f (D)
    .byte   $02,$01,$00                     ; $fe20 (*)
    .byte   $00                             ; $fe23 (D)
    .byte   $02,$04                         ; $fe24 (*)
    .byte   $02,$00                         ; $fe26 (D)
    .byte   $00,$00                         ; $fe28 (*)
    .byte   $10                             ; $fe2a (D)
    .byte   $00                             ; $fe2b (*)
    .byte   $10                             ; $fe2c (D)
    .byte   $00,$00,$20                     ; $fe2d (*)
    .byte   $00                             ; $fe30 (D)
    .byte   $20,$00                         ; $fe31 (*)
    .byte   $00,$40                         ; $fe33 (D)
    .byte   $00                             ; $fe35 (*)
Lfe36
    .byte   $0c,$0a,$08,$06,$04,$02,$00,$00 ; $fe36 (*)
Lfe3e
    .byte   $0f                             ; $fe3e (A)
    .byte   $0c                             ; $fe3f (A)
    .byte   $0a                             ; $fe40 (A)
    .byte   $08                             ; $fe41 (A)
    .byte   $0a                             ; $fe42 (A)
    .byte   $08                             ; $fe43 (A)
    .byte   $08                             ; $fe44 (A)
    .byte   $06                             ; $fe45 (A)
    .byte   $04                             ; $fe46 (A)
    .byte   $04                             ; $fe47 (A)
    .byte   $08                             ; $fe48 (A)
    .byte   $08                             ; $fe49 (A)
    .byte   $06                             ; $fe4a (A)
    .byte   $04                             ; $fe4b (A)
    .byte   $02                             ; $fe4c (A)
    .byte   $13                             ; $fe4d (A)
    .byte   $14,$15,$16                     ; $fe4e (*)
Lfe51
    .byte   $0c                             ; $fe51 (D)
    .byte   $0f,$12,$0c,$10,$0e,$11,$0a,$0d ; $fe52 (*)
    .byte   $10,$13,$0c,$0e,$10,$12,$14     ; $fe5a (*)
    
Lfe61
    .byte   %10100000 ; |* *     |            $fe61 (P)
    .byte   %10000000 ; |*       |            $fe62 (P)
    
Lfe63
    .byte   $00                             ; $fe63 (D)
    
    .byte   %00000000 ; |        |            $fe64 (P)
    
    .byte   $00,$00,$00,$01,$01,$02,$03,$04 ; $fe65 (*)
    .byte   $04,$05,$05,$06,$06             ; $fe6d (*)
Lfe72
    .byte   $07                             ; $fe72 (*)
    .byte   $00                             ; $fe73 (D)
    .byte   $01,$40,$41,$06,$07,$46,$47,$0a ; $fe74 (*)
Lfe7c
    .byte   $20,$10,$08,$04,$02,$01,$0b,$4a ; $fe7c (*)
    .byte   $4b,$0e,$0f,$4e,$4f,$00         ; $fe84 (*)
Lfe8a
    .byte   $84,$48,$00,$24,$47             ; $fe8a (D)
    .byte   $84,$ce,$00,$d8,$0e,$da,$48,$00 ; $fe8f (*)
    .byte   $84,$88,$8a,$1a,$00,$44,$24,$28 ; $fe97 (*)
    .byte   $4c,$84,$da,$da,$88,$0e,$c4,$00 ; $fe9f (*)
    .byte   $0e,$1a,$0e,$74,$c8,$ca,$44,$00 ; $fea7 (*)
    .byte   $1a,$c8,$44,$20,$4d,$69,$73,$73 ; $feaf (*)
    .byte   $69,$6c,$65,$20,$43,$6f,$6d,$6d ; $feb7 (*)
    .byte   $61,$6e,$64,$20,$54,$52,$41,$4b ; $febf (*)
    .byte   $2d,$42,$41,$4c,$4c,$20,$43,$58 ; $fec7 (*)
    .byte   $2d,$32,$32,$20,$48,$61,$63,$6b ; $fecf (*)
    .byte   $20,$76,$31,$2e,$31,$20,$2d,$20 ; $fed7 (*)
    .byte   $28,$43,$29,$20,$32,$30,$30,$32 ; $fedf (*)
    .byte   $20,$41,$74,$61,$72,$69,$2c,$20 ; $fee7 (*)
    .byte   $54,$68,$6f,$6d,$61,$73,$20,$4a ; $feef (*)
    .byte   $65,$6e,$74,$7a,$73,$63,$68,$20 ; $fef7 (*)
    .byte   $00                             ; $feff (*)
Lff00
    .byte   $5d,$ed,$e9,$e5,$e1,$dd,$d9,$d5 ; $ff00 (D)
    .byte   $d1,$cd,$c9                     ; $ff08 (D)
Lff0b
    .byte   $88,$a3,$af,$b5,$96,$9d,$c2,$a9 ; $ff0b (D)
    .byte   $8f,$bc                         ; $ff13 (D)
Lff15
    .byte   $62,$68,$6f,$77,$62,$68,$6f,$77 ; $ff15 (D)
    .byte   $77,$6f,$68,$62,$77,$6f,$68,$62 ; $ff1d (D)
    
    .byte   %11111111 ; |########|            $ff25 (G)
    .byte   %11111111 ; |########|            $ff26 (G)
    .byte   %11111111 ; |########|            $ff27 (G)
    .byte   %01111110 ; | ###### |            $ff28 (G)
    .byte   %01111110 ; | ###### |            $ff29 (G)
    .byte   %01110110 ; | ### ## |            $ff2a (G)
    .byte   %00110110 ; |  ## ## |            $ff2b (G)
    .byte   %00100110 ; |  #  ## |            $ff2c (G)
    .byte   %00100010 ; |  #   # |            $ff2d (G)
    
    .byte   $ff,$ff,$ff,$7e,$7e,$76,$36,$26 ; $ff2e (*)
    .byte   $22,$ff,$ff,$ff,$7e,$ff,$ff,$7e ; $ff36 (*)
    .byte   $7e,$00,$ff,$ff,$3c,$3c,$ff,$ff ; $ff3e (*)
    .byte   $7e,$00,$00,$ff,$18,$18,$18,$db ; $ff46 (*)
    .byte   $ff,$ff,$7e,$00,$ff,$18,$18,$18 ; $ff4e (*)
    .byte   $18,$00,$81,$c3,$7e             ; $ff56 (*)
    
    .byte   %11111111 ; |########|            $ff5b (G)
    .byte   %01101101 ; | ## ## #|            $ff5c (G)
    .byte   %00000000 ; |        |            $ff5d (G)
    .byte   %00000000 ; |        |            $ff5e (G)
    .byte   %00000000 ; |        |            $ff5f (G)
    .byte   %00000000 ; |        |            $ff60 (G)
    .byte   %00000000 ; |        |            $ff61 (G)
    .byte   %00000000 ; |        |            $ff62 (G)
    .byte   %00000000 ; |        |            $ff63 (G)
    .byte   %00000000 ; |        |            $ff64 (G)
    .byte   %00011000 ; |   ##   |            $ff65 (G)
    .byte   %00011000 ; |   ##   |            $ff66 (G)
    .byte   %00000000 ; |        |            $ff67 (G)
    .byte   %00000000 ; |        |            $ff68 (G)
    .byte   %00000000 ; |        |            $ff69 (G)
    .byte   %00011000 ; |   ##   |            $ff6a (G)
    .byte   %00111100 ; |  ####  |            $ff6b (G)
    .byte   %00111100 ; |  ####  |            $ff6c (G)
    .byte   %00011000 ; |   ##   |            $ff6d (G)
    .byte   %00000000 ; |        |            $ff6e (G)
    .byte   %00000000 ; |        |            $ff6f (G)
    .byte   %00011000 ; |   ##   |            $ff70 (G)
    .byte   %00111100 ; |  ####  |            $ff71 (G)
    .byte   %01111110 ; | ###### |            $ff72 (G)
    .byte   %01111110 ; | ###### |            $ff73 (G)
    .byte   %00111100 ; |  ####  |            $ff74 (G)
    .byte   %00011000 ; |   ##   |            $ff75 (G)
    .byte   %00000000 ; |        |            $ff76 (G)
    .byte   %00011000 ; |   ##   |            $ff77 (G)
    .byte   %00111100 ; |  ####  |            $ff78 (G)
    .byte   %01111110 ; | ###### |            $ff79 (G)
    .byte   %11111111 ; |########|            $ff7a (G)
    .byte   %11111111 ; |########|            $ff7b (G)
    .byte   %01111110 ; | ###### |            $ff7c (G)
    .byte   %00111100 ; |  ####  |            $ff7d (G)
    .byte   %00011000 ; |   ##   |            $ff7e (G)
Lff7f
    .byte   %11100000 ; |***     |            $ff7f (P)
    .byte   %11100000 ; |***     |            $ff80 (P)
    .byte   %11100000 ; |***     |            $ff81 (P)
    .byte   %11000000 ; |**      |            $ff82 (P)
    .byte   %11000000 ; |**      |            $ff83 (P)
    .byte   %11000000 ; |**      |            $ff84 (P)
    .byte   %10000000 ; |*       |            $ff85 (P)
    .byte   %10000000 ; |*       |            $ff86 (P)
    .byte   %10000000 ; |*       |            $ff87 (P)
    .byte   %00111000 ; |  ###   |            $ff88 (G)
    .byte   %01000100 ; | #   #  |            $ff89 (G)
    .byte   %11000110 ; |##   ## |            $ff8a (G)
    .byte   %11000110 ; |##   ## |            $ff8b (G)
    .byte   %11000110 ; |##   ## |            $ff8c (G)
    .byte   %01000100 ; | #   #  |            $ff8d (G)
    .byte   %00111000 ; |  ###   |            $ff8e (G)
    .byte   %01111100 ; | #####  |            $ff8f (G)
    .byte   %10000110 ; |#    ## |            $ff90 (G)
    .byte   %10011110 ; |#  #### |            $ff91 (G)
    .byte   %01111000 ; | ####   |            $ff92 (G)
    .byte   %11100100 ; |###  #  |            $ff93 (G)
    .byte   %11000100 ; |##   #  |            $ff94 (G)
    .byte   %01111000 ; | ####   |            $ff95 (G)
    .byte   %00001100 ; |    ##  |            $ff96 (G)
    .byte   %00001100 ; |    ##  |            $ff97 (G)
    .byte   %11111110 ; |####### |            $ff98 (G)
    .byte   %11001100 ; |##  ##  |            $ff99 (G)
    .byte   %01101100 ; | ## ##  |            $ff9a (G)
    .byte   %00111100 ; |  ####  |            $ff9b (G)
    .byte   %00011100 ; |   ###  |            $ff9c (G)
    .byte   %01111100 ; | #####  |            $ff9d (G)
    .byte   %11000110 ; |##   ## |            $ff9e (G)
    .byte   %00000110 ; |     ## |            $ff9f (G)
    .byte   %00000110 ; |     ## |            $ffa0 (G)
    .byte   %11111100 ; |######  |            $ffa1 (G)
    .byte   %11000000 ; |##      |            $ffa2 (G)
    .byte   %11111100 ; |######  |            $ffa3 (G)
    .byte   %00110000 ; |  ##    |            $ffa4 (G)
    .byte   %00110000 ; |  ##    |            $ffa5 (G)
    .byte   %00110000 ; |  ##    |            $ffa6 (G)
    .byte   %00110000 ; |  ##    |            $ffa7 (G)
    .byte   %01110000 ; | ###    |            $ffa8 (G)
    .byte   %00110000 ; |  ##    |            $ffa9 (G)
    .byte   %00110000 ; |  ##    |            $ffaa (G)
    .byte   %00110000 ; |  ##    |            $ffab (G)
    .byte   %00011000 ; |   ##   |            $ffac (G)
    .byte   %00001100 ; |    ##  |            $ffad (G)
    .byte   %11000110 ; |##   ## |            $ffae (G)
    .byte   %11111110 ; |####### |            $ffaf (G)
    .byte   %11100000 ; |###     |            $ffb0 (G)
    .byte   %01111000 ; | ####   |            $ffb1 (G)
    .byte   %00111100 ; |  ####  |            $ffb2 (G)
    .byte   %00001110 ; |    ### |            $ffb3 (G)
    .byte   %11000110 ; |##   ## |            $ffb4 (G)
    .byte   %01111100 ; | #####  |            $ffb5 (G)
    .byte   %11000110 ; |##   ## |            $ffb6 (G)
    .byte   %00000110 ; |     ## |            $ffb7 (G)
    .byte   %00111100 ; |  ####  |            $ffb8 (G)
    .byte   %00011000 ; |   ##   |            $ffb9 (G)
    .byte   %00001100 ; |    ##  |            $ffba (G)
    .byte   %01111110 ; | ###### |            $ffbb (G)
    .byte   %01111000 ; | ####   |            $ffbc (G)
    .byte   %00001100 ; |    ##  |            $ffbd (G)
    .byte   %00000110 ; |     ## |            $ffbe (G)
    .byte   %01111110 ; | ###### |            $ffbf (G)
    .byte   %11000110 ; |##   ## |            $ffc0 (G)
    .byte   %11000110 ; |##   ## |            $ffc1 (G)
    .byte   %01111100 ; | #####  |            $ffc2 (G)
    .byte   %11000110 ; |##   ## |            $ffc3 (G)
    .byte   %11000110 ; |##   ## |            $ffc4 (G)
    .byte   %11111100 ; |######  |            $ffc5 (G)
    .byte   %11000000 ; |##      |            $ffc6 (G)
    .byte   %01100000 ; | ##     |            $ffc7 (G)
    .byte   %00111100 ; |  ####  |            $ffc8 (G)
    .byte   %10101010 ; |# # # # |            $ffc9 (G)
    .byte   %01010100 ; | # # #  |            $ffca (G)
    .byte   %00101000 ; |  # #   |            $ffcb (G)
    .byte   %00010000 ; |   #    |            $ffcc (G)
    .byte   %10101000 ; |# # #   |            $ffcd (G)
    .byte   %01010100 ; | # # #  |            $ffce (G)
    .byte   %00101000 ; |  # #   |            $ffcf (G)
    .byte   %00010000 ; |   #    |            $ffd0 (G)
    .byte   %10100000 ; |# #     |            $ffd1 (G)
    .byte   %01010100 ; | # # #  |            $ffd2 (G)
    .byte   %00101000 ; |  # #   |            $ffd3 (G)
    .byte   %00010000 ; |   #    |            $ffd4 (G)
    .byte   %10000000 ; |#       |            $ffd5 (G)
    .byte   %01010100 ; | # # #  |            $ffd6 (G)
    .byte   %00101000 ; |  # #   |            $ffd7 (G)
    .byte   %00010000 ; |   #    |            $ffd8 (G)
    .byte   %00000000 ; |        |            $ffd9 (G)
    .byte   %01010100 ; | # # #  |            $ffda (G)
    .byte   %00101000 ; |  # #   |            $ffdb (G)
    .byte   %00010000 ; |   #    |            $ffdc (G)
    .byte   %00000000 ; |        |            $ffdd (G)
    .byte   %01010000 ; | # #    |            $ffde (G)
    .byte   %00101000 ; |  # #   |            $ffdf (G)
    .byte   %00010000 ; |   #    |            $ffe0 (G)
    .byte   %00000000 ; |        |            $ffe1 (G)
    .byte   %01000000 ; | #      |            $ffe2 (G)
    .byte   %00101000 ; |  # #   |            $ffe3 (G)
    .byte   %00010000 ; |   #    |            $ffe4 (G)
    .byte   %00000000 ; |        |            $ffe5 (G)
    .byte   %00000000 ; |        |            $ffe6 (G)
    .byte   %00101000 ; |  # #   |            $ffe7 (G)
    .byte   %00010000 ; |   #    |            $ffe8 (G)
    .byte   %00000000 ; |        |            $ffe9 (G)
    .byte   %00000000 ; |        |            $ffea (G)
    .byte   %00100000 ; |  #     |            $ffeb (G)
    .byte   %00010000 ; |   #    |            $ffec (G)
    .byte   %00000000 ; |        |            $ffed (G)
    .byte   %00000000 ; |        |            $ffee (G)
    .byte   %00000000 ; |        |            $ffef (G)
    .byte   %00010000 ; |   #    |            $fff0 (G)
    
    .byte   $26,$29,$21,$21,$21,$21,$f9,$00 ; $fff1 (*)
    .byte   $00,$00,$00                     ; $fff9 (*)
    .byte   $39,$f0,$39,$f0                 ; $fffc (D)
