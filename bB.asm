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

.L00 ;;line 6;;  dim _P1_L_R = player1x.a

.L01 ;;line 7;;  dim _P1_U_D = player1y.b

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

.L06 ;;line 17;;  dim _Bit0_Bird_Dead = d

.L07 ;;line 18;;  dim _Bit1_Bird_Falling = d

.L08 ;;line 19;;  dim _Bit2_Dog_Show = d

.
 ;;line 20;; 

.
 ;;line 21;; 

.L09 ;;line 22;;  dim _bird_counter = e

.L010 ;;line 23;;  dim _wait_counter = f

.L011 ;;line 24;;  dim _bulletcounter = g

.L012 ;;line 25;;  dim _Master_Counter = h

.L013 ;;line 26;;  dim _Frame_Counter = i

.L014 ;;line 27;;  dim _Frame_Counter_dead = j

.L015 ;;line 28;;  dim _flight_pattern = k

.L016 ;;line 29;;  dim _dog_timer = l

.L017 ;;line 30;;  dim _dog_frame = m

.
 ;;line 31;; 

.
 ;;line 32;; 

.
 ;;line 33;; 

.
 ;;line 34;; 

.L018 ;;line 35;;  dim rand16 = z

.
 ;;line 36;; 

.
 ;;line 37;; 

.
 ;;line 38;; 

.
 ;;line 39;; 

.
 ;;line 40;; 

.L019 ;;line 41;;  set kernel_options pfcolors

.
 ;;line 42;; 

.
 ;;line 43;; 

.
 ;;line 44;; 

.
 ;;line 45;; 

.
 ;;line 46;; 

.
 ;;line 47;; 

.
 ;;line 48;; 

.
 ;;line 49;; 

.
 ;;line 50;; 

.L020 ;;line 51;;  const _P_Edge_Top = 9

.L021 ;;line 52;;  const _P_Edge_Bottom = 88

.L022 ;;line 53;;  const _P_Edge_Left = 1

.L023 ;;line 54;;  const _P_Edge_Right = 153

.
 ;;line 55;; 

.
 ;;line 56;; 

.
 ;;line 57;; 

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

.
 ;;line 63;; 

.L024 ;;line 64;;  _Bit1_Bird_Falling{1} = 0

	LDA _Bit1_Bird_Falling
	AND #253
	STA _Bit1_Bird_Falling
.
 ;;line 65;; 

.L025 ;;line 66;;  _Bit0_Bird_Dead{0} = 0

	LDA _Bit0_Bird_Dead
	AND #254
	STA _Bit0_Bird_Dead
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

.__Start_Restart
 ;;line 74;; __Start_Restart

.
 ;;line 75;; 

.
 ;;line 76;; 

.
 ;;line 77;; 

.
 ;;line 78;; 

.
 ;;line 79;; 

.
 ;;line 80;; 

.L026 ;;line 81;;  AUDV0 = 0  :  AUDV1 = 0

	LDA #0
	STA AUDV0
	STA AUDV1
.
 ;;line 82;; 

.
 ;;line 83;; 

.
 ;;line 84;; 

.
 ;;line 85;; 

.
 ;;line 86;; 

.
 ;;line 87;; 

.
 ;;line 88;; 

.
 ;;line 89;; 

.L027 ;;line 90;;  a = 0  :  b = 0  :  c = 0  :  d = 0  :  e = 0  :  f = 0  :  g = 0  :  h = 0  :  i = 0

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
.L028 ;;line 91;;  j = 0  :  k = 0  :  l = 0  :  m = 0  :  n = 0  :  o = 0  :  p = 0  :  q = 0  :  r = 0

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
.L029 ;;line 92;;  s = 0  :  t = 0  :  u = 0  :  v = 0  :  w = 0  :  x = 0  :  y = 0

	LDA #0
	STA s
	STA t
	STA u
	STA v
	STA w
	STA x
	STA y
.
 ;;line 93;; 

.
 ;;line 94;; 

.
 ;;line 95;; 

.
 ;;line 96;; 

.
 ;;line 97;; 

.
 ;;line 98;; 

.L030 ;;line 99;;  player1x = 0  :  player1y = 90

	LDA #0
	STA player1x
	LDA #90
	STA player1y
.
 ;;line 100;; 

.
 ;;line 101;; 

.
 ;;line 102;; 

.
 ;;line 103;; 

.
 ;;line 104;; 

.L031 ;;line 105;;  player0x = 45  :  player0y = 53

	LDA #45
	STA player0x
	LDA #53
	STA player0y
.
 ;;line 106;; 

.
 ;;line 107;; 

.
 ;;line 108;; 

.
 ;;line 109;; 

.L032 ;;line 110;;  missile0height = 2

	LDA #2
	STA missile0height
.
 ;;line 111;; 

.
 ;;line 112;; 

.
 ;;line 113;; 

.
 ;;line 114;; 

.
 ;;line 115;; 

.L033 ;;line 116;;  COLUPF = $3A

	LDA #$3A
	STA COLUPF
.
 ;;line 117;; 

.
 ;;line 118;; 

.
 ;;line 119;; 

.
 ;;line 120;; 

.
 ;;line 121;; 

.
 ;;line 122;; 

.L034 ;;line 123;;  COLUBK = $84

	LDA #$84
	STA COLUBK
.
 ;;line 124;; 

.
 ;;line 125;; 

.
 ;;line 126;; 

.
 ;;line 127;; 

.
 ;;line 128;; 

.
 ;;line 129;; 

.
 ;;line 130;; 

.
 ;;line 131;; 

.
 ;;line 132;; 

.L035 ;;line 133;;  _Bit0_Reset_Restrainer{0} = 1

	LDA _Bit0_Reset_Restrainer
	ORA #1
	STA _Bit0_Reset_Restrainer
.
 ;;line 134;; 

.
 ;;line 135;; 

.
 ;;line 136;; 

.
 ;;line 137;; 

.
 ;;line 138;; 

.
 ;;line 139;; 

.L036 ;;line 140;;  player0:

	LDX #<playerL036_0
	STX player0pointerlo
	LDA #>playerL036_0
	STA player0pointerhi
	LDA #9
	STA player0height
.
 ;;line 152;; 

.
 ;;line 153;; 

.
 ;;line 154;; 

.
 ;;line 155;; 

.
 ;;line 156;; 

.
 ;;line 157;; 

.L037 ;;line 158;;  player1:

	LDX #<playerL037_1
	STX player1pointerlo
	LDA #>playerL037_1
	STA player1pointerhi
	LDA #10
	STA player1height
.
 ;;line 171;; 

.
 ;;line 172;; 

.
 ;;line 173;; 

.
 ;;line 174;; 

.
 ;;line 175;; 

.
 ;;line 176;; 

.L038 ;;line 177;;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel0
PF_data0
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00110000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00110110, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %10111000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %01110000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00110000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00110000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.
 ;;line 190;; 

.L039 ;;line 191;;  pfcolors:

 lda # $3A
 sta COLUPF
 ifconst pfres
 lda #>(pfcolorlabel42-132+pfres*pfwidth)
 else
 lda #>(pfcolorlabel42-84)
 endif
 sta pfcolortable+1
 ifconst pfres
 lda #<(pfcolorlabel42-132+pfres*pfwidth)
 else
 lda #<(pfcolorlabel42-84)
 endif
 sta pfcolortable
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

.L040 ;;line 209;;  scorecolor = $F8

	LDA #$F8
	STA scorecolor
.
 ;;line 210;; 

.
 ;;line 211;; 

.
 ;;line 212;; 

.
 ;;line 213;; 

.
 ;;line 214;; 

.
 ;;line 215;; 

.
 ;;line 216;; 

.__Main_Loop
 ;;line 217;; __Main_Loop

.
 ;;line 218;; 

.
 ;;line 219;; 

.
 ;;line 220;; 

.
 ;;line 221;; 

.
 ;;line 222;; 

.L041 ;;line 223;;  COLUP0 = 0

	LDA #0
	STA COLUP0
.L042 ;;line 224;;  COLUP1 = $B4

	LDA #$B4
	STA COLUP1
.
 ;;line 225;; 

.
 ;;line 226;; 

.
 ;;line 227;; 

.L043 ;;line 228;;  NUSIZ0 = $20

	LDA #$20
	STA NUSIZ0
.
 ;;line 229;; 

.
 ;;line 230;; 

.
 ;;line 231;; 

.
 ;;line 232;; 

.
 ;;line 233;; 

.
 ;;line 234;; 

.
 ;;line 235;; 

.
 ;;line 236;; 

.
 ;;line 237;; 

.L044 ;;line 238;;  if !joy0fire then _Bit1_FireB_Restrainer{1} = 0  :  goto __Skip_Joy0_Fire

 bit INPT4
	BPL .skipL044
.condpart0
	LDA _Bit1_FireB_Restrainer
	AND #253
	STA _Bit1_FireB_Restrainer
 jmp .__Skip_Joy0_Fire

.skipL044
.
 ;;line 239;; 

.
 ;;line 240;; 

.
 ;;line 241;; 

.
 ;;line 242;; 

.
 ;;line 243;; 

.L045 ;;line 244;;  if _Bit1_FireB_Restrainer{1} then goto __Skip_Joy0_Fire

	LDA _Bit1_FireB_Restrainer
	AND #2
	BEQ .skipL045
.condpart1
 jmp .__Skip_Joy0_Fire

.skipL045
.
 ;;line 245;; 

.
 ;;line 246;; 

.
 ;;line 247;; 

.
 ;;line 248;; 

.
 ;;line 249;; 

.L046 ;;line 250;;  _Bit1_FireB_Restrainer{1} = 1

	LDA _Bit1_FireB_Restrainer
	ORA #2
	STA _Bit1_FireB_Restrainer
.
 ;;line 251;; 

.
 ;;line 252;; 

.
 ;;line 253;; 

.L047 ;;line 254;;  missile0x =  ( player0x  +  3 ) 

; complex statement detected
	LDA player0x
	CLC
	ADC #3
	STA missile0x
.L048 ;;line 255;;  missile0y =  ( player0y  -  4 ) 

; complex statement detected
	LDA player0y
	SEC
	SBC #4
	STA missile0y
.
 ;;line 256;; 

.
 ;;line 257;; 

.L049 ;;line 258;;  if collision(player1,missile0) then _Bit0_Bird_Dead{0} = 1  :  goto __dead_bird

	bit 	CXM0P
	BPL .skipL049
.condpart2
	LDA _Bit0_Bird_Dead
	ORA #1
	STA _Bit0_Bird_Dead
 jmp .__dead_bird

.skipL049
.
 ;;line 259;; 

.L050 ;;line 260;;  _Bit1_FireB_Restrainer{1} = 0

	LDA _Bit1_FireB_Restrainer
	AND #253
	STA _Bit1_FireB_Restrainer
.
 ;;line 261;; 

.
 ;;line 262;; 

.
 ;;line 263;; 

.__Skip_Joy0_Fire
 ;;line 264;; __Skip_Joy0_Fire

.
 ;;line 265;; 

.
 ;;line 266;; 

.
 ;;line 267;; 

.
 ;;line 268;; 

.
 ;;line 269;; 

.
 ;;line 270;; 

.
 ;;line 271;; 

.
 ;;line 272;; 

.L051 ;;line 273;;  if !joy0up then goto __Skip_Joy0_Up

 lda #$10
 bit SWCHA
	BEQ .skipL051
.condpart3
 jmp .__Skip_Joy0_Up

.skipL051
.
 ;;line 274;; 

.
 ;;line 275;; 

.
 ;;line 276;; 

.
 ;;line 277;; 

.L052 ;;line 278;;  if _P0_U_D  <=  _P_Edge_Top then goto __Skip_Joy0_Up

	LDA #_P_Edge_Top
	CMP _P0_U_D
     BCC .skipL052
.condpart4
 jmp .__Skip_Joy0_Up

.skipL052
.
 ;;line 279;; 

.
 ;;line 280;; 

.
 ;;line 281;; 

.
 ;;line 282;; 

.L053 ;;line 283;;  _P0_U_D = _P0_U_D  -  1.00

	LDA b
	SEC 
	SBC #0
	STA b
	LDA _P0_U_D
	SBC #1
	STA _P0_U_D
.
 ;;line 284;; 

.
 ;;line 285;; 

.
 ;;line 286;; 

.
 ;;line 287;; 

.L054 ;;line 288;;  if _P0_U_D  <=  _P_Edge_Top then goto __Skip_Joy0_Up

	LDA #_P_Edge_Top
	CMP _P0_U_D
     BCC .skipL054
.condpart5
 jmp .__Skip_Joy0_Up

.skipL054
.
 ;;line 289;; 

.
 ;;line 290;; 

.__Skip_Joy0_Up
 ;;line 291;; __Skip_Joy0_Up

.
 ;;line 292;; 

.
 ;;line 293;; 

.
 ;;line 294;; 

.
 ;;line 295;; 

.
 ;;line 296;; 

.
 ;;line 297;; 

.
 ;;line 298;; 

.
 ;;line 299;; 

.L055 ;;line 300;;  if !joy0down then goto __Skip_Joy0_Down

 lda #$20
 bit SWCHA
	BEQ .skipL055
.condpart6
 jmp .__Skip_Joy0_Down

.skipL055
.
 ;;line 301;; 

.
 ;;line 302;; 

.
 ;;line 303;; 

.
 ;;line 304;; 

.L056 ;;line 305;;  if _P0_U_D  >=  _P_Edge_Bottom then goto __Skip_Joy0_Down

	LDA _P0_U_D
	CMP #_P_Edge_Bottom
     BCC .skipL056
.condpart7
 jmp .__Skip_Joy0_Down

.skipL056
.
 ;;line 306;; 

.
 ;;line 307;; 

.
 ;;line 308;; 

.
 ;;line 309;; 

.L057 ;;line 310;;  _P0_U_D = _P0_U_D  +  1.00

	LDA b
	CLC 
	ADC #0
	STA b
	LDA _P0_U_D
	ADC #1
	STA _P0_U_D
.
 ;;line 311;; 

.
 ;;line 312;; 

.
 ;;line 313;; 

.
 ;;line 314;; 

.L058 ;;line 315;;  if _P0_U_D  >=  _P_Edge_Bottom then goto __Skip_Joy0_Down

	LDA _P0_U_D
	CMP #_P_Edge_Bottom
     BCC .skipL058
.condpart8
 jmp .__Skip_Joy0_Down

.skipL058
.
 ;;line 316;; 

.
 ;;line 317;; 

.__Skip_Joy0_Down
 ;;line 318;; __Skip_Joy0_Down

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

.
 ;;line 325;; 

.
 ;;line 326;; 

.L059 ;;line 327;;  if !joy0left then goto __Skip_Joy0_Left

 bit SWCHA
	BVC .skipL059
.condpart9
 jmp .__Skip_Joy0_Left

.skipL059
.
 ;;line 328;; 

.
 ;;line 329;; 

.
 ;;line 330;; 

.
 ;;line 331;; 

.L060 ;;line 332;;  if _P0_L_R  <=  _P_Edge_Left then goto __Skip_Joy0_Left

	LDA #_P_Edge_Left
	CMP _P0_L_R
     BCC .skipL060
.condpart10
 jmp .__Skip_Joy0_Left

.skipL060
.
 ;;line 333;; 

.
 ;;line 334;; 

.
 ;;line 335;; 

.
 ;;line 336;; 

.
 ;;line 337;; 

.L061 ;;line 338;;  _P0_L_R = _P0_L_R  -  1.00

	LDA a
	SEC 
	SBC #0
	STA a
	LDA _P0_L_R
	SBC #1
	STA _P0_L_R
.
 ;;line 339;; 

.
 ;;line 340;; 

.
 ;;line 341;; 

.
 ;;line 342;; 

.L062 ;;line 343;;  if _P0_L_R  <=  _P_Edge_Left then goto __Skip_Joy0_Left

	LDA #_P_Edge_Left
	CMP _P0_L_R
     BCC .skipL062
.condpart11
 jmp .__Skip_Joy0_Left

.skipL062
.
 ;;line 344;; 

.
 ;;line 345;; 

.
 ;;line 346;; 

.
 ;;line 347;; 

.__Skip_Joy0_Left
 ;;line 348;; __Skip_Joy0_Left

.
 ;;line 349;; 

.
 ;;line 350;; 

.
 ;;line 351;; 

.
 ;;line 352;; 

.
 ;;line 353;; 

.
 ;;line 354;; 

.
 ;;line 355;; 

.
 ;;line 356;; 

.L063 ;;line 357;;  if !joy0right then goto __Skip_Joy0_Right

 bit SWCHA
	BPL .skipL063
.condpart12
 jmp .__Skip_Joy0_Right

.skipL063
.
 ;;line 358;; 

.
 ;;line 359;; 

.
 ;;line 360;; 

.
 ;;line 361;; 

.L064 ;;line 362;;  if _P0_L_R  >=  _P_Edge_Right then goto __Skip_Joy0_Right

	LDA _P0_L_R
	CMP #_P_Edge_Right
     BCC .skipL064
.condpart13
 jmp .__Skip_Joy0_Right

.skipL064
.
 ;;line 363;; 

.
 ;;line 364;; 

.
 ;;line 365;; 

.
 ;;line 366;; 

.
 ;;line 367;; 

.
 ;;line 368;; 

.
 ;;line 369;; 

.L065 ;;line 370;;  _P0_L_R = _P0_L_R  +  1.00

	LDA a
	CLC 
	ADC #0
	STA a
	LDA _P0_L_R
	ADC #1
	STA _P0_L_R
.
 ;;line 371;; 

.
 ;;line 372;; 

.
 ;;line 373;; 

.
 ;;line 374;; 

.L066 ;;line 375;;  if _P0_L_R  >=  _P_Edge_Right then goto __Skip_Joy0_Right

	LDA _P0_L_R
	CMP #_P_Edge_Right
     BCC .skipL066
.condpart14
 jmp .__Skip_Joy0_Right

.skipL066
.
 ;;line 376;; 

.__Skip_Joy0_Right
 ;;line 377;; __Skip_Joy0_Right

.
 ;;line 378;; 

.
 ;;line 379;; 

.
 ;;line 380;; 

.
 ;;line 381;; 

.L067 ;;line 382;;  if _Bit2_Dog_Show{2} then goto __dog_show

	LDA _Bit2_Dog_Show
	AND #4
	BEQ .skipL067
.condpart15
 jmp .__dog_show

.skipL067
.
 ;;line 383;; 

.
 ;;line 384;; 

.
 ;;line 385;; 

.
 ;;line 386;; 

.
 ;;line 387;; 

.L068 ;;line 388;;  if _P1_L_R  >=  _P_Edge_Right then goto __Skip_Flight

	LDA _P1_L_R
	CMP #_P_Edge_Right
     BCC .skipL068
.condpart16
 jmp .__Skip_Flight

.skipL068
.
 ;;line 389;; 

.
 ;;line 390;; 

.
 ;;line 391;; 

.
 ;;line 392;; 

.
 ;;line 393;; 

.
 ;;line 394;; 

.
 ;;line 395;; 

.L069 ;;line 396;;  _Master_Counter = _Master_Counter  +  1

	INC _Master_Counter
.
 ;;line 397;; 

.L070 ;;line 398;;  if _Master_Counter  <  4 then goto __Skip_Frame_Counter

	LDA _Master_Counter
	CMP #4
     BCS .skipL070
.condpart17
 jmp .__Skip_Frame_Counter

.skipL070
.
 ;;line 399;; 

.L071 ;;line 400;;  _Frame_Counter = _Frame_Counter  +  1  :  _Master_Counter = 0

	INC _Frame_Counter
	LDA #0
	STA _Master_Counter
.
 ;;line 401;; 

.L072 ;;line 402;;  if _Frame_Counter = 4 then _Frame_Counter = 0

	LDA _Frame_Counter
	CMP #4
     BNE .skipL072
.condpart18
	LDA #0
	STA _Frame_Counter
.skipL072
.
 ;;line 403;; 

.__Skip_Frame_Counter
 ;;line 404;; __Skip_Frame_Counter

.
 ;;line 405;; 

.
 ;;line 406;; 

.
 ;;line 407;; 

.
 ;;line 408;; 

.L073 ;;line 409;;  if !_Bit0_Bird_Dead{0} then on _Frame_Counter goto __Frame0 __Frame1 __Frame1 __Frame2

	LDA _Bit0_Bird_Dead
	LSR
	BCS .skipL073
.condpart19
	LDX _Frame_Counter
	LDA .19thenjumptablehi,x
	PHA
	LDA .19thenjumptablelo,x
	PHA
	RTS
.19thenjumptablehi
	.byte >(.__Frame0-1)
	.byte >(.__Frame1-1)
	.byte >(.__Frame1-1)
	.byte >(.__Frame2-1)
.19thenjumptablelo
	.byte <(.__Frame0-1)
	.byte <(.__Frame1-1)
	.byte <(.__Frame1-1)
	.byte <(.__Frame2-1)
.skipL073
.L074 ;;line 410;;  if _Bit0_Bird_Dead{0} then on _Frame_Counter goto __Frame_Dead_1 __Frame_Dead_1 __Frame_Dead_2 __Frame_Dead_2

	LDA _Bit0_Bird_Dead
	LSR
	BCC .skipL074
.condpart20
	LDX _Frame_Counter
	LDA .20thenjumptablehi,x
	PHA
	LDA .20thenjumptablelo,x
	PHA
	RTS
.20thenjumptablehi
	.byte >(.__Frame_Dead_1-1)
	.byte >(.__Frame_Dead_1-1)
	.byte >(.__Frame_Dead_2-1)
	.byte >(.__Frame_Dead_2-1)
.20thenjumptablelo
	.byte <(.__Frame_Dead_1-1)
	.byte <(.__Frame_Dead_1-1)
	.byte <(.__Frame_Dead_2-1)
	.byte <(.__Frame_Dead_2-1)
.skipL074
.
 ;;line 411;; 

.__end_flying
 ;;line 412;; __end_flying

.
 ;;line 413;; 

.
 ;;line 414;; 

.
 ;;line 415;; 

.
 ;;line 416;; 

.L075 ;;line 417;;  if !_Bit0_Bird_Dead{0} then goto __flying_bird

	LDA _Bit0_Bird_Dead
	LSR
	BCS .skipL075
.condpart21
 jmp .__flying_bird

.skipL075
.
 ;;line 418;; 

.L076 ;;line 419;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 420;; 

.__dead_bird
 ;;line 421;; __dead_bird

.
 ;;line 422;; 

.L077 ;;line 423;;  _Bit1_Bird_Falling{0} = 0

	LDA _Bit1_Bird_Falling
	AND #254
	STA _Bit1_Bird_Falling
.L078 ;;line 424;;  _Bit0_Bird_Dead{0} = 1

	LDA _Bit0_Bird_Dead
	ORA #1
	STA _Bit0_Bird_Dead
.L079 ;;line 425;;  score = score  +  1

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
.L080 ;;line 426;;  _Bit2_Dog_Show{2} = 1

	LDA _Bit2_Dog_Show
	ORA #4
	STA _Bit2_Dog_Show
.L081 ;;line 427;;  _dog_timer = 0

	LDA #0
	STA _dog_timer
.L082 ;;line 428;;  _dog_frame = 0

	LDA #0
	STA _dog_frame
.L083 ;;line 429;;  _P1_L_R = 76

	LDA #0
	STA a
	LDA #76
	STA _P1_L_R
.L084 ;;line 430;;  _P1_U_D = 84

	LDA #0
	STA b
	LDA #84
	STA _P1_U_D
.L085 ;;line 431;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 432;; 

.__dog_show
 ;;line 433;; __dog_show

.
 ;;line 434;; 

.L086 ;;line 435;;  _dog_timer = _dog_timer  +  1

	INC _dog_timer
.
 ;;line 436;; 

.L087 ;;line 437;;  if _dog_timer  <  16 then _P1_U_D = 84  -  _dog_timer

	LDA _dog_timer
	CMP #16
     BCS .skipL087
.condpart22
	SEC
	SBC _dog_timer
	STA _P1_U_D
.skipL087
.L088 ;;line 438;;  if _dog_timer  >=  16 then _P1_U_D = 68

	LDA _dog_timer
	CMP #16
     BCC .skipL088
.condpart23
	LDA #0
	STA b
	LDA #68
	STA _P1_U_D
.skipL088
.
 ;;line 439;; 

.L089 ;;line 440;;  if _dog_timer = 8 then _dog_frame = 1

	LDA _dog_timer
	CMP #8
     BNE .skipL089
.condpart24
	LDA #1
	STA _dog_frame
.skipL089
.L090 ;;line 441;;  if _dog_timer = 16 then _dog_frame = 0

	LDA _dog_timer
	CMP #16
     BNE .skipL090
.condpart25
	LDA #0
	STA _dog_frame
.skipL090
.L091 ;;line 442;;  if _dog_timer = 24 then _dog_frame = 1

	LDA _dog_timer
	CMP #24
     BNE .skipL091
.condpart26
	LDA #1
	STA _dog_frame
.skipL091
.L092 ;;line 443;;  if _dog_timer = 32 then _dog_frame = 0

	LDA _dog_timer
	CMP #32
     BNE .skipL092
.condpart27
	LDA #0
	STA _dog_frame
.skipL092
.L093 ;;line 444;;  if _dog_timer = 40 then _dog_frame = 1

	LDA _dog_timer
	CMP #40
     BNE .skipL093
.condpart28
	LDA #1
	STA _dog_frame
.skipL093
.L094 ;;line 445;;  if _dog_timer = 48 then _dog_frame = 0

	LDA _dog_timer
	CMP #48
     BNE .skipL094
.condpart29
	LDA #0
	STA _dog_frame
.skipL094
.
 ;;line 446;; 

.L095 ;;line 447;;  if _dog_timer  >=  180 then _Bit2_Dog_Show{2} = 0  :  goto __bird_spawn

	LDA _dog_timer
	CMP #180
     BCC .skipL095
.condpart30
	LDA _Bit2_Dog_Show
	AND #251
	STA _Bit2_Dog_Show
 jmp .__bird_spawn

.skipL095
.
 ;;line 448;; 

.L096 ;;line 449;;  if _dog_frame = 0 then goto __Dog_Frame0

	LDA _dog_frame
	CMP #0
     BNE .skipL096
.condpart31
 jmp .__Dog_Frame0

.skipL096
.L097 ;;line 450;;  if _dog_frame = 1 then goto __Dog_Frame1

	LDA _dog_frame
	CMP #1
     BNE .skipL097
.condpart32
 jmp .__Dog_Frame1

.skipL097
.
 ;;line 451;; 

.L098 ;;line 452;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 453;; 

.
 ;;line 454;; 

.
 ;;line 455;; 

.__Skip_Flight
 ;;line 456;; __Skip_Flight

.
 ;;line 457;; 

.L099 ;;line 458;;  player1x = 200  :  player1y = 200

	LDA #200
	STA player1x
	STA player1y
.L0100 ;;line 459;;  _wait_counter = _wait_counter  +  1

	INC _wait_counter
.
 ;;line 460;; 

.L0101 ;;line 461;;  if _wait_counter = 60 then goto __bird_spawn

	LDA _wait_counter
	CMP #60
     BNE .skipL0101
.condpart33
 jmp .__bird_spawn

.skipL0101
.
 ;;line 462;; 

.L0102 ;;line 463;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 464;; 

.__bird_spawn
 ;;line 465;; __bird_spawn

.
 ;;line 466;; 

.L0103 ;;line 467;;  player1:

	LDX #<playerL0103_1
	STX player1pointerlo
	LDA #>playerL0103_1
	STA player1pointerhi
	LDA #10
	STA player1height
.
 ;;line 480;; 

.L0104 ;;line 481;;  _Bit0_Bird_Dead{0} = 0

	LDA _Bit0_Bird_Dead
	AND #254
	STA _Bit0_Bird_Dead
.L0105 ;;line 482;;  _Bit1_Bird_Falling{0} = 0

	LDA _Bit1_Bird_Falling
	AND #254
	STA _Bit1_Bird_Falling
.L0106 ;;line 483;;  _wait_counter = 0

	LDA #0
	STA _wait_counter
.L0107 ;;line 484;;  _flight_pattern = rand  &  3

 jsr randomize
	AND #3
	STA _flight_pattern
.
 ;;line 485;; 

.L0108 ;;line 486;;  player1x = 0

	LDA #0
	STA player1x
.
 ;;line 487;; 

.L0109 ;;line 488;;  player1y = 90

	LDA #90
	STA player1y
.
 ;;line 489;; 

.
 ;;line 490;; 

.
 ;;line 491;; 

.
 ;;line 492;; 

.
 ;;line 493;; 

.__clear_missile
 ;;line 494;; __clear_missile

.
 ;;line 495;; 

.
 ;;line 496;; 

.
 ;;line 497;; 

.L0110 ;;line 498;;  _bulletcounter = 0

	LDA #0
	STA _bulletcounter
.L0111 ;;line 499;;  missile0x = 160  :  missile0y = 200

	LDA #160
	STA missile0x
	LDA #200
	STA missile0y
.L0112 ;;line 500;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 501;; 

.__flying_bird
 ;;line 502;; __flying_bird

.L0113 ;;line 503;;  _bird_counter = _bird_counter  +  1

	INC _bird_counter
.L0114 ;;line 504;;  if _bird_counter = 60 then _bird_counter = 0

	LDA _bird_counter
	CMP #60
     BNE .skipL0114
.condpart34
	LDA #0
	STA _bird_counter
.skipL0114
.L0115 ;;line 505;;  on _flight_pattern goto __pattern0 __pattern1 __pattern2 __pattern3

	LDX _flight_pattern
	LDA .L0115jumptablehi,x
	PHA
	LDA .L0115jumptablelo,x
	PHA
	RTS
.L0115jumptablehi
	.byte >(.__pattern0-1)
	.byte >(.__pattern1-1)
	.byte >(.__pattern2-1)
	.byte >(.__pattern3-1)
.L0115jumptablelo
	.byte <(.__pattern0-1)
	.byte <(.__pattern1-1)
	.byte <(.__pattern2-1)
	.byte <(.__pattern3-1)
.
 ;;line 506;; 

.__pattern0
 ;;line 507;; __pattern0

.L0116 ;;line 508;;  _P1_L_R = _P1_L_R  +  .7

	LDA a
	CLC 
	ADC #179
	STA a
	LDA _P1_L_R
	ADC #
	STA _P1_L_R
.L0117 ;;line 509;;  _P1_U_D = _P1_U_D  - .2

	LDA b
	SEC 
	SBC #51
	STA b
	LDA _P1_U_D
	SBC #
	STA _P1_U_D
.L0118 ;;line 510;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 511;; 

.__pattern1
 ;;line 512;; __pattern1

.L0119 ;;line 513;;  if _bird_counter  <  30 then _P1_U_D = _P1_U_D  -  .5

	LDA _bird_counter
	CMP #30
     BCS .skipL0119
.condpart35
	LDA b
	SEC 
	SBC #128
	STA b
	LDA _P1_U_D
	SBC #
	STA _P1_U_D
.skipL0119
.L0120 ;;line 514;;  if _bird_counter  >=  30 then _P1_U_D = _P1_U_D  +  .5

	LDA _bird_counter
	CMP #30
     BCC .skipL0120
.condpart36
	LDA b
	CLC 
	ADC #128
	STA b
	LDA _P1_U_D
	ADC #
	STA _P1_U_D
.skipL0120
.L0121 ;;line 515;;  _P1_L_R = _P1_L_R  +  .6

	LDA a
	CLC 
	ADC #153
	STA a
	LDA _P1_L_R
	ADC #
	STA _P1_L_R
.L0122 ;;line 516;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 517;; 

.__pattern2
 ;;line 518;; __pattern2

.L0123 ;;line 519;;  if _P1_U_D  <  player0y then _P1_U_D = _P1_U_D  +  .3

	LDA _P1_U_D
	CMP player0y
     BCS .skipL0123
.condpart37
	LDA b
	CLC 
	ADC #76
	STA b
	LDA _P1_U_D
	ADC #
	STA _P1_U_D
.skipL0123
.L0124 ;;line 520;;  if _P1_U_D  >  player0y then _P1_U_D = _P1_U_D  -  .3

	LDA player0y
	CMP _P1_U_D
     BCS .skipL0124
.condpart38
	LDA b
	SEC 
	SBC #76
	STA b
	LDA _P1_U_D
	SBC #
	STA _P1_U_D
.skipL0124
.L0125 ;;line 521;;  _P1_L_R = _P1_L_R  +  .8

	LDA a
	CLC 
	ADC #204
	STA a
	LDA _P1_L_R
	ADC #
	STA _P1_L_R
.L0126 ;;line 522;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 523;; 

.__pattern3
 ;;line 524;; __pattern3

.L0127 ;;line 525;;  _P1_L_R = _P1_L_R  +  .7

	LDA a
	CLC 
	ADC #179
	STA a
	LDA _P1_L_R
	ADC #
	STA _P1_L_R
.L0128 ;;line 526;;  if rand  &  1 then _P1_U_D = _P1_U_D  +  .4

	LDA rand
	AND #1
     BEQ .skipL0128
.condpart39
	LDA b
	CLC 
	ADC #102
	STA b
	LDA _P1_U_D
	ADC #
	STA _P1_U_D
.skipL0128
.L0129 ;;line 527;;  if ! ( rand  &  1 )  then _P1_U_D = _P1_U_D  -  .4

; complex statement detected
 jsr randomize
	AND #1
	BNE .skipL0129
.condpart40
	LDA b
	SEC 
	SBC #102
	STA b
	LDA _P1_U_D
	SBC #
	STA _P1_U_D
.skipL0129
.L0130 ;;line 528;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 529;; 

.
 ;;line 530;; 

.__falling_bird
 ;;line 531;; __falling_bird

.
 ;;line 532;; 

.L0131 ;;line 533;;  _P1_U_D = _P1_U_D  +  .6

	LDA b
	CLC 
	ADC #153
	STA b
	LDA _P1_U_D
	ADC #
	STA _P1_U_D
.L0132 ;;line 534;;  if _P1_U_D  >=  70 then goto __Skip_Flight

	LDA _P1_U_D
	CMP #70
     BCC .skipL0132
.condpart41
 jmp .__Skip_Flight

.skipL0132
.
 ;;line 535;; 

.L0133 ;;line 536;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 537;; 

.__exit_flight_sub
 ;;line 538;; __exit_flight_sub

.
 ;;line 539;; 

.
 ;;line 540;; 

.
 ;;line 541;; 

.
 ;;line 542;; 

.L0134 ;;line 543;;  drawscreen

 jsr drawscreen
.
 ;;line 544;; 

.
 ;;line 545;; 

.
 ;;line 546;; 

.
 ;;line 547;; 

.
 ;;line 548;; 

.
 ;;line 549;; 

.
 ;;line 550;; 

.
 ;;line 551;; 

.
 ;;line 552;; 

.
 ;;line 553;; 

.
 ;;line 554;; 

.
 ;;line 555;; 

.
 ;;line 556;; 

.
 ;;line 557;; 

.L0135 ;;line 558;;  if !switchreset then _Bit0_Reset_Restrainer{0} = 0  :  goto __Main_Loop

 lda #1
 bit SWCHB
	BEQ .skipL0135
.condpart42
	LDA _Bit0_Reset_Restrainer
	AND #254
	STA _Bit0_Reset_Restrainer
 jmp .__Main_Loop

.skipL0135
.
 ;;line 559;; 

.
 ;;line 560;; 

.
 ;;line 561;; 

.
 ;;line 562;; 

.
 ;;line 563;; 

.L0136 ;;line 564;;  if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

	LDA _Bit0_Reset_Restrainer
	LSR
	BCC .skipL0136
.condpart43
 jmp .__Main_Loop

.skipL0136
.
 ;;line 565;; 

.
 ;;line 566;; 

.
 ;;line 567;; 

.
 ;;line 568;; 

.L0137 ;;line 569;;  goto __Start_Restart

 jmp .__Start_Restart

.
 ;;line 570;; 

.
 ;;line 571;; 

.__Frame0
 ;;line 572;; __Frame0

.
 ;;line 573;; 

.L0138 ;;line 574;;  player1:

	LDX #<playerL0138_1
	STX player1pointerlo
	LDA #>playerL0138_1
	STA player1pointerhi
	LDA #16
	STA player1height
.L0139 ;;line 593;;  goto __end_flying

 jmp .__end_flying

.
 ;;line 594;; 

.__Dog_Frame0
 ;;line 595;; __Dog_Frame0

.L0140 ;;line 596;;  player1:

	LDX #<playerL0140_1
	STX player1pointerlo
	LDA #>playerL0140_1
	STA player1pointerhi
	LDA #15
	STA player1height
.L0141 ;;line 614;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 615;; 

.__Dog_Frame1
 ;;line 616;; __Dog_Frame1

.L0142 ;;line 617;;  player1:

	LDX #<playerL0142_1
	STX player1pointerlo
	LDA #>playerL0142_1
	STA player1pointerhi
	LDA #15
	STA player1height
.L0143 ;;line 635;;  goto __exit_flight_sub

 jmp .__exit_flight_sub

.
 ;;line 636;; 

.__Frame2
 ;;line 637;; __Frame2

.
 ;;line 638;; 

.L0144 ;;line 639;;  player1:

	LDX #<playerL0144_1
	STX player1pointerlo
	LDA #>playerL0144_1
	STA player1pointerhi
	LDA #15
	STA player1height
.
 ;;line 657;; 

.
 ;;line 658;; 

.L0145 ;;line 659;;  goto __end_flying

 jmp .__end_flying

.
 ;;line 660;; 

.__Frame1
 ;;line 661;; __Frame1

.
 ;;line 662;; 

.L0146 ;;line 663;;  player1:

	LDX #<playerL0146_1
	STX player1pointerlo
	LDA #>playerL0146_1
	STA player1pointerhi
	LDA #15
	STA player1height
.L0147 ;;line 681;;  goto __end_flying

 jmp .__end_flying

.
 ;;line 682;; 

.
 ;;line 683;; 

.__Frame_Dead_1
 ;;line 684;; __Frame_Dead_1

.
 ;;line 685;; 

.L0148 ;;line 686;;  player1:

	LDX #<playerL0148_1
	STX player1pointerlo
	LDA #>playerL0148_1
	STA player1pointerhi
	LDA #15
	STA player1height
.L0149 ;;line 704;;  goto __falling_bird

 jmp .__falling_bird

.
 ;;line 705;; 

.__Frame_Dead_2
 ;;line 706;; __Frame_Dead_2

.L0150 ;;line 707;;  player1:

	LDX #<playerL0150_1
	STX player1pointerlo
	LDA #>playerL0150_1
	STA player1pointerhi
	LDA #15
	STA player1height
.L0151 ;;line 725;;  goto __falling_bird

 jmp .__falling_bird

 if (<*) > (<(*+9))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL036_0
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
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL037_1
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
 ifconst pfres
 if (<*) > (254-pfres*pfwidth)
	align 256
	endif
 if (<*) < (136-pfres*pfwidth)
	repeat ((136-pfres*pfwidth)-(<*))
	.byte 0
	repend
	endif
 else
 if (<*) > 206
	align 256
	endif
 if (<*) < 88
	repeat (88-(<*))
	.byte 0
	repend
	endif
 endif
pfcolorlabel42
 .byte  $3A,0,0,0
 .byte  $3A,0,0,0
 .byte  $3A,0,0,0
 .byte  $3A,0,0,0
 .byte  $3A,0,0,0
 .byte  $3A,0,0,0
 .byte  $3A,0,0,0
 .byte  $36,0,0,0
 .byte  $36,0,0,0
 .byte  $36,0,0,0
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0103_1
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
 if (<*) > (<(*+16))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0138_1
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
 if (<*) > (<(*+15))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0140_1
	.byte  %00000000
	.byte  %00011000
	.byte  %00111100
	.byte  %01111110
	.byte  %00111100
	.byte  %00011000
	.byte  %00011000
	.byte  %00111100
	.byte  %01111110
	.byte  %01111110
	.byte  %00111100
	.byte  %00011000
	.byte  %00011000
	.byte  %00011000
	.byte  %00000000
	.byte  %00000000
 if (<*) > (<(*+15))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0142_1
	.byte  %00000000
	.byte  %00011000
	.byte  %00111100
	.byte  %01111110
	.byte  %00111100
	.byte  %00011000
	.byte  %00011000
	.byte  %00111100
	.byte  %01111110
	.byte  %01101110
	.byte  %00111100
	.byte  %00011000
	.byte  %00011000
	.byte  %00011000
	.byte  %00000000
	.byte  %00000000
 if (<*) > (<(*+15))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0144_1
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
 if (<*) > (<(*+15))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0146_1
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
 if (<*) > (<(*+15))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0148_1
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
 if (<*) > (<(*+15))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0150_1
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
 if ECHOFIRST
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 endif 
ECHOFIRST = 1
 
 
 
