 
   ;```````````````````````````````````````````````````````````````
   ;  Player0 fixed point variables for more flexibility in
   ;  gameplay mechanics.
   ;
   dim _P1_L_R = player1x.a
   dim _P1_U_D = player1y.b
   dim _P0_L_R = player0x.a
   dim _P0_U_D = player0y.b

   ;```````````````````````````````````````````````````````````````
   ;  Bits for various jobs.
   ;
  
   dim _Bit0_Reset_Restrainer = c
   dim _Bit1_FireB_Restrainer = c
   dim _Bit0_Bird_Dead = d
   dim _Bit1_Bird_Falling = d 

    ; full variables
   dim _bird_counter = e
   dim _wait_counter = f
   dim _bulletcounter = g
   dim _Master_Counter = h
   dim _Frame_Counter = i
   dim _Frame_Counter_dead = j

   ;```````````````````````````````````````````````````````````````
   ;  Makes better random numbers.
   ;
   dim rand16 = z



   ;***************************************************************
   ;
   ;  Defines the edges of the playfield for an 8 x 8 sprite.
   ;  If your sprite is a different size, you`ll need to adjust
   ;  the numbers.
   ;
   const _P_Edge_Top = 9
   const _P_Edge_Bottom = 88
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


   ;***************************************************************
   ;
   ;  Starting position of Player1.
   ;
   player1x = 0 : player1y = 90

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
   COLUPF = $CE


   ;***************************************************************
   ;
   ;  Sets background color.
   ;
   COLUBK = $9C


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
   .............XX.................
   ............XXXX................
   ....XX.....XXXXXXX.........XX...
   ...XXXX...XXXXXXXXXXXX....XXXX..
   ..XXXXXX.XXXXXXXXXXXXXXX.XXXXXX.
   .XXXXXXXX...............XXXXXXXX
   XXXXXXXXXX.............XXXXXXXXX
   XXXXXXXXXXX...........XXXXXXXXXX
   .....................XXXXXXXXXXX
   ....................XXXXXXXXXXXX
   ...................XXXXXXXXXXXXX
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


      ; the reason for the offset is to put the bullet in the middle of the bulls eye
    missile0x = (player0x + 3)
    missile0y = (player0y - 4)
    

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

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;  

   if _P1_L_R >= _P_Edge_Right then goto __Skip_Flight

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

   _Bit1_Bird_Falling{0} = 1 ; turn on falling bird

     score = score + 1

   goto __exit_flight_sub


   ;```````````````````````````````````````````````````````````````
__Skip_Flight

   player1x = 200 : player1y = 200
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
 ;   score = 0
    player1x = 0  
    ;player1y = (rand&31) + (rand&15) + (rand&1) + 20
    player1y = 90
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
    
  if _bird_counter > 30 then goto __flying_bird_2

  _P1_L_R = _P1_L_R + .7
  _P1_U_D = _P1_U_D -.2


   goto __exit_flight_sub

__flying_bird_2

   _P1_L_R = _P1_L_R + .5 
   _P1_U_D = _P1_U_D - .3

   goto __exit_flight_sub

__falling_bird

   _P1_U_D = _P1_U_D + .6
   if _P1_U_D >= 70 then goto __Skip_Flight
   
   goto __exit_flight_sub

__exit_flight_sub

 
;   _Bit0_Bird_Restrainer{0} = 1

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
