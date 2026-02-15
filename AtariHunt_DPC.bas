 
   ;```````````````````````````````````````````````````````````````
   ;  Player0 fixed point variables for more flexibility in
   ;  gameplay mechanics.
   ;
   dim _P1_L_R = n
   dim _P1_U_D = o
   dim _P0_L_R = player0x.a
   dim _P0_U_D = player0y.b

   ;```````````````````````````````````````````````````````````````
   ;  Bits for various jobs.
   ;
  
   dim _Bit0_Reset_Restrainer = c
   dim _Bit1_FireB_Restrainer = c
   dim _Splash_Active = q
   dim _Bit0_Bird_Dead = d
   dim _Bit1_Bird_Falling = d 
   dim _Bit2_Dog_Show = d

    ; full variables
   dim _bird_counter = e
   dim _wait_counter = f
   dim _bulletcounter = g
   dim _Master_Counter = h
   dim _Frame_Counter = i
   dim _Frame_Counter_dead = j
   dim _flight_pattern = k
   dim _dog_timer = l
   dim _dog_frame = m
   dim _bird_dir = p
   dim _Splash_Blink = var0
   dim _Round = var1
   dim _Shots_Remaining = var2
   dim _Shots_Fired = r
   dim _Round_Hits = var4
   dim _Game_Over = var5
   dim _Just_Started = var6
   dim _Accuracy = var7
   dim _Splash_P1_L_R = s
   dim _Splash_P1_U_D = t
   dim _Splash_P1_Pattern = u
   dim _Splash_P1_Dir = v
   dim _Grey_X_Prev = w
   dim _Grey_Y_Prev = x

   ;```````````````````````````````````````````````````````````````
   ;  Makes better random numbers.
   ;
   dim rand16 = z
   dim _Bit7_Splash_Seen = rand16

   ;***************************************************************
   ;
   ;  Kernel options.
   ;
   set kernel DPC+
   set kernel_options pfcolors

   ;***************************************************************
   ;
   ;  Standard used in North America and most of South America.
   ;
   set tv ntsc



   ;***************************************************************
   ;
   ;  NTSC colors.
   ;
   ;  Use these constants so you can quickly and easily swap them
   ;  out for PAL-60 colors. Or use this if you created a PAL-60
   ;  game and want to instantly convert the colors to NTSC (if you
   ;  were already using the PAL-60 constants).
   ;
   
   const _00 = $00
   const _02 = $02
   const _04 = $04
   const _06 = $06
   const _08 = $08
   const _0A = $0A
   const _0C = $0C
   const _0E = $0E
   const _10 = $10
   const _12 = $12
   const _14 = $14
   const _16 = $16
   const _18 = $18
   const _1A = $1A
   const _1C = $1C
   const _1E = $1E
   const _20 = $20
   const _22 = $22
   const _24 = $24
   const _26 = $26
   const _28 = $28
   const _2A = $2A
   const _2C = $2C
   const _2E = $2E
   const _30 = $30
   const _32 = $32
   const _34 = $34
   const _36 = $36
   const _38 = $38
   const _3A = $3A
   const _3C = $3C
   const _3E = $3E
   const _40 = $40
   const _42 = $42
   const _44 = $44
   const _46 = $46
   const _48 = $48
   const _4A = $4A
   const _4C = $4C
   const _4E = $4E
   const _50 = $50
   const _52 = $52
   const _54 = $54
   const _56 = $56
   const _58 = $58
   const _5A = $5A
   const _5C = $5C
   const _5E = $5E
   const _60 = $60
   const _62 = $62
   const _64 = $64
   const _66 = $66
   const _68 = $68
   const _6A = $6A
   const _6C = $6C
   const _6E = $6E
   const _70 = $70
   const _72 = $72
   const _74 = $74
   const _76 = $76
   const _78 = $78
   const _7A = $7A
   const _7C = $7C
   const _7E = $7E
   const _80 = $80
   const _82 = $82
   const _84 = $84
   const _86 = $86
   const _88 = $88
   const _8A = $8A
   const _8C = $8C
   const _8E = $8E
   const _90 = $90
   const _92 = $92
   const _94 = $94
   const _96 = $96
   const _98 = $98
   const _9A = $9A
   const _9C = $9C
   const _9E = $9E
   const _A0 = $A0
   const _A2 = $A2
   const _A4 = $A4
   const _A6 = $A6
   const _A8 = $A8
   const _AA = $AA
   const _AC = $AC
   const _AE = $AE
   const _B0 = $B0
   const _B2 = $B2
   const _B4 = $B4
   const _B6 = $B6
   const _B8 = $B8
   const _BA = $BA
   const _BC = $BC
   const _BE = $BE
   const _C0 = $C0
   const _C2 = $C2
   const _C4 = $C4
   const _C6 = $C6
   const _C8 = $C8
   const _CA = $CA
   const _CC = $CC
   const _CE = $CE
   const _D0 = $D0
   const _D2 = $D2
   const _D4 = $D4
   const _D6 = $D6
   const _D8 = $D8
   const _DA = $DA
   const _DC = $DC
   const _DE = $DE
   const _E0 = $E0
   const _E2 = $E2
   const _E4 = $E4
   const _E6 = $E6
   const _E8 = $E8
   const _EA = $EA
   const _EC = $EC
   const _EE = $EE
   const _F0 = $F0
   const _F2 = $F2
   const _F4 = $F4
   const _F6 = $F6
   const _F8 = $F8
   const _FA = $FA
   const _FC = $FC
   const _FE = $FE



   ;***************************************************************
   ;
   ;  Defines the edges of the playfield for an 8 x 8 sprite.
   ;  If your sprite is a different size, you`ll need to adjust
   ;  the numbers.
   ;
   const _P_Edge_Top = 0
   const _P_Edge_Bottom = 170
   const _P_Edge_Left = 1
   const _P_Edge_Right = 153
   const _Shots_Per_Round = 10
   const _Min_Hits_To_Pass = 7
   const _Ammo_Row = 170



   ;***************************************************************
   ;
   ;  Disables the =. (We don`t need it in this program.)
   ;
  ;   const noscore = 1
    ;bird's default is NOT falling
    _Bit1_Bird_Falling{1} = 0 ;bird's default is NOT falling
    
    _Bit0_Bird_Dead{0} = 0 ;bird's default is alive
  
   ;***************************************************************
   ;***************************************************************
   ;
   ;  PROGRAM START/RESTART
   ;
   ;
   _Bit7_Splash_Seen{7} = 0
   goto __Bank_2 bank2



   bank 2
   temp1=temp1



__Bank_2


__Start_Restart


   ;***************************************************************
   ;
   ;  Mutes volume of both sound channels.
   ;
   AUDV0 = 0 : AUDV1 = 0


     ;***************************************************************
   ;
   ;  Clears 25 of the normal 26 variables (fastest way).
   ;  The variable z is used for random numbers in this program
   ;  and clearing it would mess up those random numbers.
   ;
   a = 0 : b = 0 : c = 0 : d = 0 : e = 0 : f = 0 : g = 0 : h = 0 : i = 0
   j = 0 : k = 0 : l = 0 : m = 0 : n = 0 : o = 0 : p = 0 : q = 0 : r = 0
   s = 0 : t = 0 : u = 0 : v = 0
   var0 = 0 : var1 = 0 : var2 = 0 : var3 = 0 : var4 = 0
   var5 = 0 : var6 = 0 : var7 = 0 : var8 = 0

   _Round = 1
   _Shots_Remaining = _Shots_Per_Round
   _Shots_Fired = 0
   _Round_Hits = 0
   _Game_Over = 0
   _Just_Started = 1
   score = 0

   _Splash_Active = 1
   if _Bit7_Splash_Seen{7} then _Splash_Active = 0


   ;***************************************************************
   ;
   ;  Starting position of Player1.
   ;
   player1x = 0 : player1y = 90
   _P1_L_R = 0 : _P1_U_D = 90

   ;***************************************************************
   ;
   ;  Starting position of Player0 and initialize grey code.
   ;
   player0x = 45 : player0y = 53
   _Grey_X_Prev = 0 : _Grey_Y_Prev = 0

   ;***************************************************************
   ;
   ;  Missle  Size
   missile0height = 2
   
   ;***************************************************************
   ;
   ;  Sets playfield color.
   ;
   COLUPF = $3A


   ;***************************************************************
   ;
   ;  Sets background color.
   ;
   COLUBK = $84


   ;***************************************************************
   ;
   ;  Restrains the reset switch for the main loop.
   ;
   ;  This bit fixes it so the reset switch becomes inactive if
   ;  it hasn't been released after being pressed once.
   ;
   _Bit0_Reset_Restrainer{0} = 1


   ;***************************************************************
   ;
   ;  Defines shape of player0 sprite.
   ;
   player0:
   
   %00011000
   %00011000
   %00100100
   %11000011
   %11000011
   %00100100
   %00011000
   %00011000
   
end


   ;***************************************************************
   ;
   ;  Defines shape of player1 sprite. (bird) used in splash screen only it seems.
   ;
 player1:
 %00000000
 %00000000
 %00111000
 %01111100
 %11111111
 %00111010
 %00011000
 %00011000
 %00001000
 %00000000
 %00000000
end

   player1color:
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
end


   ;***************************************************************
   ;
   ;  Sets up the playfield.
   ;
   playfield:
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   .XX.............................
   .XX.............................
   .XX.............................
   .XX..X..........................
   XXX..X..........................
   XXX..X..........................
   XXX..X..........................
   XXX.XX..........................
   XXX.XXX.........................
   XXX.XXX.........................
   XXX.XXX.........................
   XXX.XXX.........................
   XXX.XX..........................
   .X..XX..........................
   .X...X..........................
   .X...X..........................
   .X...X..........................
   .X...X..........................
   .XX.XX..........................
   ..X.X...........................
   ..X.X...........................
   ..X.X...........................
   ..X.X.X.........................
   ..X.X.X.........................
   ..X.X.XXX.......................
   ..X.X.XXX.......................
   ..X.X.XXX.......................
   ..X.X.XXX.......................
   ..X.X.XXX.......................
   ..X.X.XXX.......................
   ..XXX.XX........................
   ...X..XX........................
   ...X..XX........................
   ...X..XX........................
   ...X..XXX.......................
   ...X..XXX.......................
   ...X..XXX.......................
   ...X..XXX.......................
   .X.X..XXX.......................
   .X.X..XXX.......................
   .X.X..XXX.......................
   .X.X..X.........................
   .X.X..X.........................
   .X.X..X.........................
   .X.X..X.........................
   .X.X..X.........................
   .X.X.X..........................
   .X.X.X..........................
   ..XX.X..........................
   ..XX.X..........................
   ..XXXX..........................
   ..XXX...........................
   ..XXX...........................
   ..XXX...........................
   ..XXX...........................
   ..XXX...........................
   ..XXX...........................
   ...XX...........................
   ...XX...........................
   ...XX...........................
   ....X...........................
   ....X...........................
   ...XX...........................
   ...XX...........................
   ...XX...........................
   ...XX......................X....
   ...XX......................X....
   ...XX......................XX...
   ...XX......................XX...
   ...XX......................XX...
   ...XX......................XX...
   ...XX......................XX...
   ...XX......................XX...
   ...XX.....................XXX...
   ...XX.....................XXX...
   ...XX.....................XXX...
   ...XX....X................XXXX..
   ...XX....X................XXXX..
   ...XX....X...............XXXXX..
   ...XX....X.......X.......XXXXX..
   ...X.....X.......X.......XXXXX..
   ...XX............X.......XXXX...
   X..XX............X.......XXXXX..
   ...XX............X..X....XXXXX..
   ...X.............X..XX...XXXXX..
   ......X.....X.X..X...X...XX..X..
   ......X.X...X..X..X.......XXXX..
   .X....X.X.X.......X....X..XXX...
   .X....X.X.X.......X....X........
   ........X.X.......X.X..XX.....X.
   .X....XXX.X.X...XXX.XX..X.......
   .X.X..X..XX.XXXX..X.X...X......X
   ...X..X..X.XXX....X.X...X...XXXX
   .XXXX.X..XXX.X..........X..XX...
   ....XXXX.X...X........XX........
   X......X..X.....................
   X....XXX..X.....................
   .....XX...X....X................
   .....XX...X...XX..XXX.X......XX.
   ......X..XX.....XXX..XX......X..
   ..XXX....X...X..X....XX......X..
   .......X.X..XX..XX....X...X.XX..
   X..X.X.XXX.XX....X....X...X..XX.
   ........XX............XX........
   X.X....XXX.............XXXX.....
   X.X....X.X..............XX......
   ..X..X.XX.........XXXX..XXX.....
   ..X..X.X.........XXXX.....X.X..X
   ..X....X............X.......X..X
   ..XX...X.....X.......X..........
   ..XX.XXX.....X..................
   ..X......XXX....................
   ................................
   .......................X........
   .................XX....XX...XX..
   .....XX................XX.......
   X...............................
   ...........XX...................
   ...X............................
   ...X...XXX......................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ..................XXXXXX........
   .......................X........
   ...XX..XX....................X..
   ..XX..........XX..........X.XX..
   ..............X...........XXX...
   .X..............................
   ................................
   ................................
   ................................
   ................................
   ...............................X
   ...............................X
   ................................
   ................................
   ................................
   X........X.XXX..XXX.............
   ..XX............................
   ...X.XX......................XX.
   .....XX................XX..XXX..
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
   ................................
end

   pfcolors:
   _F8
   _F8
   _D6
   _D6
   _D6
   _D6
   _D6
   _D6
   _CE
   _CA
   _CA
   _CA
   _C8
   _C8
   _C8
   _CA
   _C4
   _C8
   _CA
   _CA
   _CA
   _C4
   _C4
   _CA
   _CA
   _CA
   _CA
   _CA
   _C8
   _C8
   _CA
   _CA
   _CA
   _CA
   _CA
   _CA
   _C8
   _C8
   _C8
   _CA
   _C8
   _C8
   _CA
   _CA
   _C4
   _D6
   _D6
   _C8
   _D4
   _D4
   _D2
   _D2
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _D4
   _F6
   _F0
   _F0
   _F6
   _F6
   _F6
   _F6
   _F0
   _F6
   _F6
   _F0
   _F6
   _FE
   _F6
   _F0
   _F0
   _FE
   _FE
   _F0
   _F0
   _F0
   _F0
end

   bkcolors:
   _90
   _90
   _90
   _94
   _96
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _98
   _A8
   _AC
   _9E
   _9E
   _FE
   _FE
   _FC
   _FA
   _FA
   _FC
   _F8
   _FA
   _FA
   _F8
   _F8
   _D4
   _F6
   _F8
   _F4
   _F4
   _F6
   _FA
   _F4
   _D4
   _F4
   _F6
   _F8
   _D4
   _F4
   _F8
   _F4
   _D4
   _F8
   _F8
   _FE
   _FE
   _FE
   _FE
   _FE
   _FE
   _FE
   _FE
end
 

   scorecolor = _F8

     ;***************************************************************
   ;***************************************************************
   ;
   ;  MAIN LOOP (MAKES THE PROGRAM GO)
   ;
   ;
__Main_Loop



   ;***************************************************************
   ;
   ;  Sets color of player0 sprite. (the gun sight)
   ;
   COLUP0 = 0
   COLUP1 = $C8

   ; Sets the width of missle

   NUSIZ0 = $20
;I think this is where the trackball input and the main gameplay loop start. 
   ;The game over check is here so that the player can see the final score and;
   ; accuracy after the game ends, instead of it immediately jumping to the ;
   ;game over screen where the score and accuracy are not visible.

   ;***************************************************************
   ;  Grey code trackball reading (quadrature encoding).
   ;  Reads INPT0-3 for horizontal and vertical movement.
   ;
   ;

   ; Read horizontal grey code bits (INPT0 bit 7, INPT1 bit 7)
   temp1 = 0

   ;Debug if trackball is working by changing background color based on horizontal movement.
   
   ;if temp1 > 0 then COLUBK = $46 else COLUBK = $8C
   if INPT0 >= 128 then temp1 = 2
   if INPT1 >= 128 then temp1 = temp1 + 1

   ; Decode horizontal movement using state table
   temp3 = _Grey_X_Prev * 4
   temp3 = temp3 + temp1
   if temp3 = 1 then _P0_L_R = _P0_L_R + 2
   if temp3 = 7 then _P0_L_R = _P0_L_R + 2
   if temp3 = 8 then _P0_L_R = _P0_L_R + 2
   if temp3 = 14 then _P0_L_R = _P0_L_R + 2
   if temp3 = 2 then _P0_L_R = _P0_L_R - 2
   if temp3 = 4 then _P0_L_R = _P0_L_R - 2
   if temp3 = 11 then _P0_L_R = _P0_L_R - 2
   if temp3 = 13 then _P0_L_R = _P0_L_R - 2
   _Grey_X_Prev = temp1

   ; Read vertical grey code bits (INPT2 bit 7, INPT3 bit 7)
   temp2 = 0
   if INPT2 >= 128 then temp2 = 2
   if INPT3 >= 128 then temp2 = temp2 + 1

   ; Decode vertical movement using state table
   temp4 = _Grey_Y_Prev * 4
   temp4 = temp4 + temp2
   if temp4 = 1 then _P0_U_D = _P0_U_D + 2
   if temp4 = 7 then _P0_U_D = _P0_U_D + 2
   if temp4 = 8 then _P0_U_D = _P0_U_D + 2
   if temp4 = 14 then _P0_U_D = _P0_U_D + 2
   if temp4 = 2 then _P0_U_D = _P0_U_D - 2
   if temp4 = 4 then _P0_U_D = _P0_U_D - 2
   if temp4 = 11 then _P0_U_D = _P0_U_D - 2
   if temp4 = 13 then _P0_U_D = _P0_U_D - 2
   _Grey_Y_Prev = temp2

   ; Boundary checks
   if _P0_L_R < _P_Edge_Left then _P0_L_R = _P_Edge_Left
   if _P0_L_R > _P_Edge_Right then _P0_L_R = _P_Edge_Right
   if _P0_U_D < _P_Edge_Top then _P0_U_D = _P_Edge_Top
   if _P0_U_D > _P_Edge_Bottom then _P0_U_D = _P_Edge_Bottom

   ;***************************************************************
   ;
   ;  Splash screen.
   ;
   if _Splash_Active then goto __Splash_Screen

   if _Game_Over then goto __Game_Over
   
   
   
   if _Shots_Remaining = 0 && !_Bit0_Bird_Dead{0} && !_Bit1_Bird_Falling{0} && !_Bit2_Dog_Show{2} then goto __Round_End_Check

   if _Just_Started then goto __Just_Started_Check

   

__Fire_Button_Check

   ;***************************************************************
   ;
   ;  Fire button check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Turns off restrainer bit and skips this section if button is
   ;  not pressed.
   ;
   if !joy0fire then AUDV0 = 0 : _Bit1_FireB_Restrainer{1} = 0 : goto __Skip_Joy0_Fire

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if button hasn't been released after
   ;  being pressed.
   ;
   if _Bit1_FireB_Restrainer{1} then goto __Skip_Joy0_Fire

   ;```````````````````````````````````````````````````````````````
   ;  Turns on restrainer bit for fire button and turns on
   ;  bird restrainer.
   ;
   _Bit1_FireB_Restrainer{1} = 1




   if _Shots_Remaining = 0 then goto __Skip_Joy0_Fire
   _Shots_Remaining = _Shots_Remaining - 1
   _Shots_Fired = _Shots_Fired + 1

   AUDC0 = 8 : AUDF0 = 10 : AUDV0 = 8


         ; center the bullet on the gunsight
      missile0x = (player0x + 3)
      missile0y = (player0y + 3)
    

    if collision(player1,missile0) then _Bit0_Bird_Dead{0} = 1 : goto __dead_bird

      if _Bit0_Bird_Dead{0} then goto __Skip_Hitbox
      if player1x < missile0x - 4 then goto __Skip_Hitbox
      if player1x > missile0x + 4 then goto __Skip_Hitbox
      if player1y < missile0y - 4 then goto __Skip_Hitbox
      if player1y > missile0y + 4 then goto __Skip_Hitbox

      _Bit0_Bird_Dead{0} = 1 : goto __dead_bird

__Skip_Hitbox

   _bulletcounter = 2
 


__Skip_Joy0_Fire


   if _bulletcounter > 0 then _bulletcounter = _bulletcounter - 1

   if _bulletcounter = 0 then missile0x = 160 : missile0y = 200

   goto __After_Fire_Check

__Just_Started_Check

   if joy0fire then _Bit1_FireB_Restrainer{1} = 1 : goto __Skip_Joy0_Fire
   _Just_Started = 0
   _Grey_X_Prev = 0 : _Grey_Y_Prev = 0
   goto __Fire_Button_Check

__After_Fire_Check

   ;***************************************************************
   ;  Dog popup display after a hit.
   ;

   ;***************************************************************
   ;  Dog popup display after a hit.
   ;
   if _Bit2_Dog_Show{2} then goto __dog_show

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;  

   if _P1_L_R > _P_Edge_Right then goto __Skip_Flight
   if _P1_L_R < _P_Edge_Left then goto __Skip_Flight

   ;  Moves player0 right.

   ;***************************************************************
   ;
   ;  Controls animation speed.
   ;
   _Master_Counter = _Master_Counter + 1

   if _Master_Counter < 4 then goto __Skip_Frame_Counter

   _Frame_Counter = _Frame_Counter + 1 : _Master_Counter = 0

   if _Frame_Counter = 4 then _Frame_Counter = 0
  
__Skip_Frame_Counter

  ;```````````````````````````````````````````````````````````````
   ;  Animates player1 sprite.
   ;
   if !_Bit0_Bird_Dead{0} then on _Frame_Counter goto __Frame0 __Frame1 __Frame1 __Frame2
   if _Bit0_Bird_Dead{0} then on _Frame_Counter goto __Frame_Dead_1 __Frame_Dead_1 __Frame_Dead_2 __Frame_Dead_2

__end_flying 

   player1height = 16

   ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   ;  Controls Motion of Bird

   if !_Bit0_Bird_Dead{0} then goto __flying_bird

   goto __exit_flight_sub

__dead_bird

   _Bit1_Bird_Falling{0} = 1
   _Bit0_Bird_Dead{0} = 1
   _Bit2_Dog_Show{2} = 0
   _dog_timer = 0
   _Frame_Counter = 0
   _P0_U_D = _P_Edge_Bottom
   player0y = _P0_U_D
   _Round_Hits = _Round_Hits + 1
   score = score + 10
   goto __exit_flight_sub

__dog_show

   _dog_timer = _dog_timer + 1

   if _dog_timer < 16 then _P1_U_D = 84 - _dog_timer
   if _dog_timer >= 16 then _P1_U_D = 68

   player1x = _P1_L_R : player1y = _P1_U_D

   if _dog_timer >= 180 then _Bit2_Dog_Show{2} = 0 : _bulletcounter = 0 : missile0x = 160 : missile0y = 200 : AUDV0 = 0 : player0height = 8 : player0:
   %00011000
   %00011000
   %00100100
   %11000011
   %11000011
   %00100100
   %00011000
   %00011000
end : goto __bird_spawn

   goto __Dog_Frame0


   ;```````````````````````````````````````````````````````````````
__Skip_Flight

   player1x = 200 : player1y = 200
   _P1_L_R = 200 : _P1_U_D = 200
   _wait_counter = _wait_counter + 1
   
      if _wait_counter = 60 then goto __bird_spawn

   goto __exit_flight_sub

__bird_spawn

 player1:
 %00000000
 %00000000
 %00111000
 %01111100
 %11111111
 %00111010
 %00011000
 %00011000
 %00001000
 %00000000
 %00000000
end

   player1color:
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
end

    _Bit0_Bird_Dead{0} = 0
    _Bit1_Bird_Falling{0} = 0
   _Bit2_Dog_Show{2} = 0
    _wait_counter = 0
    _flight_pattern = rand & 3
    _bird_dir = rand & 1
 ;   score = 0
    if _bird_dir then player1x = _P_Edge_Left + 2 : _P1_L_R = _P_Edge_Left + 2
    if !_bird_dir then player1x = _P_Edge_Right - 2 : _P1_L_R = _P_Edge_Right - 2
    player1y = 90
    _P1_U_D = 90
   ;***************************************************************
   ;
   ;  Displays the screen.
   ;

__clear_missile

   ;```````````````````````````````````````````````````````````````
   ;  Clears missile0 moving bit and moves missile0 back to gun starting position.
   _bulletcounter = 0
    missile0x = 160 : missile0y = 200
    goto __exit_flight_sub

__Round_End_Check

   AUDV0 = 0 : AUDV1 = 0

   drawscreen

   if !joy0fire then _Bit1_FireB_Restrainer{1} = 0 : goto __Round_End_Check
   if _Bit1_FireB_Restrainer{1} then goto __Round_End_Check
   _Bit1_FireB_Restrainer{1} = 1

   if _Round_Hits < _Min_Hits_To_Pass then _Game_Over = 1 : goto __Game_Over

   goto __Next_Round

__Next_Round

   _Round = _Round + 1
   _Shots_Remaining = _Shots_Per_Round
   _Shots_Fired = 0
   _Round_Hits = 0
   goto __bird_spawn

__Game_Over

   AUDV0 = 0 : AUDV1 = 0
   COLUBK = $46
   COLUPF = $0E
   player0x = 200 : player0y = 200
   player1x = 200 : player1y = 200
   missile0x = 200 : missile0y = 200

   pfpixel 4 _Ammo_Row off
   pfpixel 6 _Ammo_Row off
   pfpixel 8 _Ammo_Row off
   pfpixel 10 _Ammo_Row off
   pfpixel 12 _Ammo_Row off
   pfpixel 14 _Ammo_Row off
   pfpixel 16 _Ammo_Row off
   pfpixel 18 _Ammo_Row off
   pfpixel 20 _Ammo_Row off
   pfpixel 22 _Ammo_Row off

   drawscreen

   if !switchreset then goto __Game_Over
   goto __Start_Restart

__Splash_Screen

   scorecolor = _8E ; this doesn't actually set the score color for the splash screen, but it does prevent a bug where the score flashes white when you start the game from the splash screen. So I consider it worth leaving in even though it doesn't do anything for the splash screen itself.
   score = 8008 ; this does change the splash screen text to "BOOB" ;
   ;which is amusing and also prevents a bug where the score flashes "0000";
   ; on the splash screen. So I consider it worth leaving in even though ;
   ;it doesn't do anything for the splash screen itself.

   ; Blue background, white logo and indicator.
   COLUBK = $8C
   COLUP0 = $0E
   COLUP1 = $0E
   COLUPF = $0E

   missile0height = 4
   player1height = 16
   player0height = 16
   missile0x = 200 : missile0y = 200

   _Splash_Blink = _Splash_Blink + 1
   if _Splash_Blink = 1 then _flight_pattern = rand & 3 : _bird_dir = rand & 1 : _P1_L_R = _P_Edge_Left + 10 : _P1_U_D = 50 : _Splash_P1_Pattern = rand & 1 : _Splash_P1_Dir = rand & 1 : _Splash_P1_L_R = _P_Edge_Right - 10 : _Splash_P1_U_D = 70

   _Master_Counter = _Master_Counter + 1
   if _Master_Counter < 4 then goto __Splash_Skip_Move
   _Frame_Counter = _Frame_Counter + 1 : _Master_Counter = 0
   if _Frame_Counter = 4 then _Frame_Counter = 0

   _bird_counter = _bird_counter + 1
   if _bird_counter = 60 then _bird_counter = 0

   if _bird_dir then _P1_L_R = _P1_L_R + 1 else _P1_L_R = _P1_L_R - 1
   if _flight_pattern = 0 && _Frame_Counter = 0 then _P1_U_D = _P1_U_D - 1
   if _flight_pattern = 1 && _bird_counter < 30 && (_Frame_Counter & 1) then _P1_U_D = _P1_U_D - 1
   if _flight_pattern = 1 && _bird_counter >= 30 && (_Frame_Counter & 1) then _P1_U_D = _P1_U_D + 1
   if _flight_pattern = 2 && _P1_U_D < 70 && (_Frame_Counter & 1) then _P1_U_D = _P1_U_D + 1
   if _flight_pattern = 2 && _P1_U_D > 40 && (_Frame_Counter & 1) then _P1_U_D = _P1_U_D - 1
   if _flight_pattern = 3 && (rand & 1) then _P1_U_D = _P1_U_D + 1 else if _flight_pattern = 3 then _P1_U_D = _P1_U_D - 1

   if _Splash_P1_Dir then _Splash_P1_L_R = _Splash_P1_L_R + 1 else _Splash_P1_L_R = _Splash_P1_L_R - 1
   if _Splash_P1_Pattern && _Frame_Counter = 0 then _Splash_P1_U_D = _Splash_P1_U_D + 1


__Splash_Skip_Move

   if _P1_L_R > _P_Edge_Right then _bird_dir = 0 : _flight_pattern = rand & 3 : _bird_counter = 0
   if _P1_L_R < _P_Edge_Left then _bird_dir = 1 : _flight_pattern = rand & 3 : _bird_counter = 0
   if _Splash_P1_L_R > _P_Edge_Right then _Splash_P1_Dir = 0
   if _Splash_P1_L_R < _P_Edge_Left then _Splash_P1_Dir = 1

   player1x = _P1_L_R
   player1y = _P1_U_D

   player0x = _Splash_P1_L_R
   player0y = _Splash_P1_U_D

__Splash_Gfx
   if _Splash_Blink & 8 then goto __Splash_Bird_Frame2

__Splash_Bird_Frame1
   player1:
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00111000
 %01111100
 %11111111
 %00000110
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
end
   player0:
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00111000
 %01111100
 %11111111
 %00000110
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
end
   goto __Splash_Draw

__Splash_Bird_Frame2
   player1:
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00111000
 %01111100
 %11111111
 %00111010
 %00011000
 %00011000
 %00001000
 %00000000
 %00000000
end
   player0:
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00111000
 %01111100
 %11111111
 %00111010
 %00011000
 %00011000
 %00001000
 %00000000
 %00000000
end

__Splash_Draw

   drawscreen

   if joy0fire then _Bit7_Splash_Seen{7} = 1 : _Bit1_FireB_Restrainer{1} = 1 : _Splash_Active = 0 : goto __Restore_Sprites

   goto __Splash_Screen

__Restore_Sprites

   player0:
   %00011000
   %00011000
   %00100100
   %11000011
   %11000011
   %00100100
   %00011000
   %00011000
end

   player1:
   %00000000
   %00000000
   %00111000
   %01111100
   %11111111
   %00111010
   %00011000
   %00011000
   %00001000
   %00000000
   %00000000
end

   goto __Start_Restart

__flying_bird
    _bird_counter = _bird_counter + 1
    if _bird_counter = 60 then _bird_counter = 0
    on _flight_pattern goto __pattern0 __pattern1 __pattern2 __pattern3

__pattern0 ; Straight flight
   if _bird_dir then _P1_L_R = _P1_L_R + 1 else _P1_L_R = _P1_L_R - 1
   if _bird_counter & 3 = 0 then _P1_U_D = _P1_U_D - 1
   player1x = _P1_L_R : player1y = _P1_U_D
    goto __exit_flight_sub

__pattern1 ; Wavy flight
   if _bird_dir then _P1_L_R = _P1_L_R + 1 else _P1_L_R = _P1_L_R - 1
   if _bird_counter < 30 && (_bird_counter & 1) then _P1_U_D = _P1_U_D - 1
   if _bird_counter >= 30 && (_bird_counter & 1) then _P1_U_D = _P1_U_D + 1
   player1x = _P1_L_R : player1y = _P1_U_D
    goto __exit_flight_sub

__pattern2 ; Aggressive flight
   if _bird_dir then _P1_L_R = _P1_L_R + 1 else _P1_L_R = _P1_L_R - 1
   if _P1_U_D < player0y && (_bird_counter & 1) then _P1_U_D = _P1_U_D + 1
   if _P1_U_D > player0y && (_bird_counter & 1) then _P1_U_D = _P1_U_D - 1
   player1x = _P1_L_R : player1y = _P1_U_D
    goto __exit_flight_sub

__pattern3 ; Erratic flight
   if _bird_dir then _P1_L_R = _P1_L_R + 1 else _P1_L_R = _P1_L_R - 1
   if rand & 1 then _P1_U_D = _P1_U_D + 1
   if !(rand & 1) then _P1_U_D = _P1_U_D - 1
   player1x = _P1_L_R : player1y = _P1_U_D
    goto __exit_flight_sub


__falling_bird

   _bird_counter = _bird_counter + 1
   if _bird_counter & 1 then _P1_U_D = _P1_U_D + 1
   player1x = _P1_L_R : player1y = _P1_U_D
   if _P1_U_D >= 160 then _Bit1_Bird_Falling{0} = 0 : _Bit2_Dog_Show{2} = 1 : _dog_timer = 0 : _dog_frame = 0 : _P1_L_R = 76 : _P1_U_D = 84 : player1x = _P1_L_R : player1y = _P1_U_D : goto __exit_flight_sub
   
   goto __exit_flight_sub

__exit_flight_sub

 
;   _Bit0_Bird_Restrainer{0} = 1

   pfpixel 4 _Ammo_Row off
   pfpixel 6 _Ammo_Row off
   pfpixel 8 _Ammo_Row off
   pfpixel 10 _Ammo_Row off
   pfpixel 12 _Ammo_Row off
   pfpixel 14 _Ammo_Row off
   pfpixel 16 _Ammo_Row off
   pfpixel 18 _Ammo_Row off
   pfpixel 20 _Ammo_Row off
   pfpixel 22 _Ammo_Row off

   if _Shots_Remaining > 0 then pfpixel 4 _Ammo_Row on
   if _Shots_Remaining > 1 then pfpixel 6 _Ammo_Row on
   if _Shots_Remaining > 2 then pfpixel 8 _Ammo_Row on
   if _Shots_Remaining > 3 then pfpixel 10 _Ammo_Row on
   if _Shots_Remaining > 4 then pfpixel 12 _Ammo_Row on
   if _Shots_Remaining > 5 then pfpixel 14 _Ammo_Row on
   if _Shots_Remaining > 6 then pfpixel 16 _Ammo_Row on
   if _Shots_Remaining > 7 then pfpixel 18 _Ammo_Row on
   if _Shots_Remaining > 8 then pfpixel 20 _Ammo_Row on
   if _Shots_Remaining > 9 then pfpixel 22 _Ammo_Row on

   ;***************************************************************
   ;
   ;  176 rows that are 1 scanline high. (88 rows of color).
   ;
   DF6FRACINC = 255 ; Background colors.
   DF4FRACINC = 255 ; Playfield colors.

   DF0FRACINC = 255 ; Column 0.
   DF1FRACINC = 255 ; Column 1.
   DF2FRACINC = 255 ; Column 2.
   DF3FRACINC = 255 ; Column 3.

   drawscreen

   
   ;***************************************************************
   ;
   ;  Reset switch check and end of main loop.
   ;
   ;  Any Atari 2600 program should restart when the reset  
   ;  switch is pressed. It is part of the usual standards
   ;  and procedures.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Turns off reset restrainer bit and jumps to beginning of
   ;  main loop if the reset switch is not pressed.
   ;
   if !switchreset then _Bit0_Reset_Restrainer{0} = 0 : goto __Main_Loop

   ;```````````````````````````````````````````````````````````````
   ;  Jumps to beginning of main loop if the reset switch hasn't
   ;  been released after being pressed.
   ;
   if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

   ;```````````````````````````````````````````````````````````````
   ;  Restarts the program.
   ;
   goto __Start_Restart

     
__Frame0   
;down wing
  player1:
 %00000000
 %00000000
 %00000000
 %00000000
 %00001000
 %00001000
 %00011000
 %00111000
 %00111000
 %11111111
 %00000110
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000

end
   player1color:
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
end
    goto __end_flying

__Dog_Frame0
   player0height = 8
   player1height = 8
   player1:
   %00111100 ; Top of head
    %01111110
    %11011011 ; Eyes
    %11111111
    %10000001 ; Snout top
    %01111110 ; Nose area
    %01000010 ; Open mouth top
    %01011010 ; Tongue detail
    %01011010
    %00111100 ; Jaw
    %00011000 ; Neck
    %00111100 ; Collar
    %01111110 ; Shoulders
    %11111111 ; Chest
    %11111111
    %11111111
    %11111111
    %01111110
    %01111110
    %01100110 ; Legs start
    %01100110
    %01100110
    %01100110
    %01100110
    %11100111 ; Paws
    %11100111
end
   player1color:
   $34 ; Brown
   $34
   $0E ; White (Eyes)
   $34
   $32 ; Darker brown
   $0E ; White (Snout)
   $00 ; Black (Mouth)
   $44 ; Red (Tongue)
   $44
   $0E ; White
   $34 ; Brown
   $84 ; Blue (Collar)
   $34 ; Brown... (repeat for body)
   $34
   $34
   $34
   $34
   $34
   $34
   $34
   $34
   $34
   $34
   $34
   $0E ; White (Paws)
   $0E
end
   goto __exit_flight_sub

__Frame2
 ;up wing
    player1:
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00111000
 %01111100
 %11111111
 %00111010
 %00011000
 %00011000
 %00001000
 %00000000
 %00000000
end

   player1color:
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
end


    goto __end_flying

__Frame1
;center frame
    player1:
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00111000
 %01111100
 %11111111
 %00000110
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
end
   player1color:
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
end
    goto __end_flying


__Frame_Dead_1

    player1:
 %00000000
 %00000000
 %00000000
 %00011000
 %00011100
 %00001000
 %00001000
 %00011100
 %00111100
 %00101100
 %01101110
 %00001000
 %00010100
 %00000000
 %00000000
 %00000000
end
   player1color:
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
end
   goto __falling_bird

__Frame_Dead_2
 player1:
 %00000000
 %00000000
 %00000000
 %00011000
 %00111000
 %00010000
 %00010000
 %00111000
 %00111100
 %00110100
 %01110110
 %00010000
 %00101000
 %00000000
 %00000000
 %00000000
end
   player1color:
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
   _00
end
   goto __falling_bird


   bank 3
   temp1=temp1



   bank 4
   temp1=temp1



   bank 5
   temp1=temp1



   bank 6
   temp1=temp1
