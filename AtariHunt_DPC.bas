 
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

   ;```````````````````````````````````````````````````````````````
   ;  Makes better random numbers.
   ;
   dim rand16 = z

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
   ;  Defines the edges of the playfield for an 8 x 8 sprite.
   ;  If your sprite is a different size, you`ll need to adjust
   ;  the numbers.
   ;
   const _P_Edge_Top = 0
   const _P_Edge_Bottom = 170
   const _P_Edge_Left = 1
   const _P_Edge_Right = 153



   ;***************************************************************
   ;
   ;  Disables the score. (We don`t need it in this program.)
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
   s = 0 : t = 0 : u = 0 : v = 0 : w = 0 : x = 0 : y = 0
   var0 = 0 : var1 = 0 : var2 = 0 : var3 = 0 : var4 = 0
   var5 = 0 : var6 = 0 : var7 = 0 : var8 = 0


   ;***************************************************************
   ;
   ;  Starting position of Player1.
   ;
   player1x = 0 : player1y = 90
   _P1_L_R = 0 : _P1_U_D = 90

   ;***************************************************************
   ;
   ;  Starting position of Player0.
   ;
   player0x = 45 : player0y = 53

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
   ;  Defines shape of player1 sprite. (bird)
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
   ....XXXXXX......................
   ...XXXXXXXX.....................
   ...XXXXXXXX.....................
   ....XXXXXX......................
   .....XXXX.......................
   ......XX........................
   ......XX........................
   .....XXXX.......................
   ....XXXXXX......................
   .....XXXX.......................
   ......XX........................
   ......XX........................
   .....XXXX.......................
   ....XXXXX.......................
   .....XXXX.......................
   ......XX........................
   ......XX........................
   .....XXXX.......................
   ....XXXXXX......................
   .....XXXX.......................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   ......XX........................
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

   pfcolors:
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $3A
   $2A
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
   $36
end

   bkcolors:
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
   $84
end
 


; const scorefade = 1

   scorecolor = $F8

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
   COLUP1 = $B4

   ; Sets the width of missle

   NUSIZ0 = $20

   ;***************************************************************
   ;
   ;  Fire button check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Turns off restrainer bit and skips this section if button is
   ;  not pressed.
   ;
   if !joy0fire then _Bit1_FireB_Restrainer{1} = 0 : goto __Skip_Joy0_Fire

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


         ; center the bullet on the gunsight
      missile0x = (player0x + 3)
      missile0y = (player0y + 3)
    

    if collision(player1,missile0) then _Bit0_Bird_Dead{0} = 1 : goto __dead_bird

   _Bit1_FireB_Restrainer{1} = 0
 


__Skip_Joy0_Fire
  
   ;***************************************************************
   ;
   ;  Joy0 up check.   THIS DOES THE GUN MOVEMENT
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved up.
   ;
   if !joy0up then goto __Skip_Joy0_Up

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if _P0_U_D <= _P_Edge_Top then goto __Skip_Joy0_Up

   ;```````````````````````````````````````````````````````````````
   ;  Moves player0 up.
   ;
   _P0_U_D = _P0_U_D - 1.00

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if _P0_U_D <= _P_Edge_Top then goto __Skip_Joy0_Up

 
__Skip_Joy0_Up

   ;***************************************************************
   ;
   ;  Joy0 down check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved down.
   ;
   if !joy0down then goto __Skip_Joy0_Down

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if _P0_U_D >= _P_Edge_Bottom then goto __Skip_Joy0_Down

   ;```````````````````````````````````````````````````````````````
   ;  Moves player0 down.
   ;
   _P0_U_D = _P0_U_D + 1.00

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if _P0_U_D >= _P_Edge_Bottom then goto __Skip_Joy0_Down


__Skip_Joy0_Down

   ;***************************************************************
   ;
   ;  Joy0 left check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved to the left.
   ;
   if !joy0left then goto __Skip_Joy0_Left

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if _P0_L_R <= _P_Edge_Left then goto __Skip_Joy0_Left

   ;```````````````````````````````````````````````````````````````
   ;```````````````````````````````````````````````````````````````
   ;  Moves player0 left.
   ;
   _P0_L_R = _P0_L_R - 1.00

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if _P0_L_R <= _P_Edge_Left then goto __Skip_Joy0_Left

   ;```````````````````````````````````````````````````````````````
   ;  Speeds up if fire button is pressed.
 
__Skip_Joy0_Left

   ;***************************************************************
   ;
   ;  Joy0 right check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved to the right.
   ;
   if !joy0right then goto __Skip_Joy0_Right

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if _P0_L_R >= _P_Edge_Right then goto __Skip_Joy0_Right

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Moves player0 right.
   ;
   _P0_L_R = _P0_L_R + 1.00

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if _P0_L_R >= _P_Edge_Right then goto __Skip_Joy0_Right

__Skip_Joy0_Right

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

   ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   ;  Controls Motion of Bird

   if !_Bit0_Bird_Dead{0} then goto __flying_bird

   goto __exit_flight_sub

__dead_bird

   _Bit1_Bird_Falling{0} = 1
   _Bit0_Bird_Dead{0} = 1
   score = score + 1
   goto __exit_flight_sub

__dog_show

   _dog_timer = _dog_timer + 1

   if _dog_timer < 16 then _P1_U_D = 84 - _dog_timer
   if _dog_timer >= 16 then _P1_U_D = 68

   player1x = _P1_L_R : player1y = _P1_U_D

   if _dog_timer = 8 then _dog_frame = 1
   if _dog_timer = 16 then _dog_frame = 0
   if _dog_timer = 24 then _dog_frame = 1
   if _dog_timer = 32 then _dog_frame = 0
   if _dog_timer = 40 then _dog_frame = 1
   if _dog_timer = 48 then _dog_frame = 0

   if _dog_timer >= 180 then _Bit2_Dog_Show{2} = 0 : goto __bird_spawn

   if _dog_frame = 0 then goto __Dog_Frame0
   if _dog_frame = 1 then goto __Dog_Frame1

   goto __exit_flight_sub


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

    _Bit0_Bird_Dead{0} = 0
    _Bit1_Bird_Falling{0} = 0
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
   if _P1_U_D >= 140 then _Bit1_Bird_Falling{0} = 0 : _Bit2_Dog_Show{2} = 1 : _dog_timer = 0 : _dog_frame = 0 : _P1_L_R = 76 : _P1_U_D = 84 : player1x = _P1_L_R : player1y = _P1_U_D : goto __exit_flight_sub
   
   goto __exit_flight_sub

__exit_flight_sub

 
;   _Bit0_Bird_Restrainer{0} = 1

   ;***************************************************************
   ;
   ;  88 rows that are 2 scanlines high.
   ;
   DF6FRACINC = 255 ; Background colors.
   DF4FRACINC = 255 ; Playfield colors.

   DF0FRACINC = 128 ; Column 0.
   DF1FRACINC = 128 ; Column 1.
   DF2FRACINC = 128 ; Column 2.
   DF3FRACINC = 128 ; Column 3.

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
    goto __end_flying

__Dog_Frame0
   player1:
 %00000000
 %00011000
 %00111100
 %01111110
 %00111100
 %00011000
 %00011000
 %00111100
 %01111110
 %01111110
 %00111100
 %00011000
 %00011000
 %00011000
 %00000000
 %00000000
end
   goto __exit_flight_sub

__Dog_Frame1
   player1:
 %00000000
 %00011000
 %00111100
 %01111110
 %00111100
 %00011000
 %00011000
 %00111100
 %01111110
 %01101110
 %00111100
 %00011000
 %00011000
 %00011000
 %00000000
 %00000000
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
   goto __falling_bird


   bank 3
   temp1=temp1



   bank 4
   temp1=temp1



   bank 5
   temp1=temp1



   bank 6
   temp1=temp1
