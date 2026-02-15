# AtariHunt_DPC.bas Logic Outline

This outline summarizes the main logic and flow of AtariHunt_DPC.bas using simple IF/THEN statements and plain English. It is organized by major sections and game states.

---

## Program Start/Restart
- IF program starts or is reset THEN
    - Set all variables to initial values
    - Set player and bird positions
    - Set playfield and background colors
    - Show splash screen IF not already seen
    - GOTO main loop

## Main Loop
- IF splash screen is active THEN GOTO splash screen logic
- IF game over THEN GOTO game over logic
- IF out of shots AND bird is not dead/falling AND dog is not showing THEN GOTO round end check
- IF just started THEN GOTO just started check

### Trackball/Movement
- Read trackball input for player0 (horizontal and vertical)
- Update player0 position based on input
- Enforce playfield boundaries for player0

### Fire Button Logic
- IF fire button is NOT pressed THEN
    - Turn off fire restrainer
    - GOTO skip fire logic
- IF fire restrainer is active THEN GOTO skip fire logic
- Set fire restrainer ON
- IF no shots remaining THEN GOTO skip fire logic
- Decrement shots remaining, increment shots fired
- Play fire sound
- Center missile on gunsight
- IF missile collides with bird THEN set bird dead and GOTO dead bird logic
- IF bird is dead or not in hitbox THEN GOTO skip hitbox
- Set bird dead and GOTO dead bird logic

### After Fire
- IF bullet is active THEN decrement bullet counter
- IF bullet counter is zero THEN hide missile
- GOTO after fire check

### Dog Popup
- IF dog is showing THEN GOTO dog show logic

### Bird Animation
- IF player1 (bird) is out of bounds THEN GOTO skip flight
- Increment master and frame counters
- Animate bird sprite based on frame counter and state
- IF bird is not dead THEN GOTO flying bird logic
- ELSE GOTO exit flight sub

### Dead Bird
- Set bird falling, dead, and hide dog
- Reset dog timer and frame counter
- Move player0 to bottom
- Increment round hits and score
- GOTO exit flight sub

### Dog Show
- Increment dog timer
- Animate dog position
- IF dog timer exceeds threshold THEN hide dog, reset bullet, hide missile, play sound, reset player0 sprite, GOTO bird spawn
- ELSE GOTO dog frame logic

### Bird Spawn
- Reset bird and dog states
- Randomize bird direction and pattern
- Set bird and player1 positions

### Round End Check
- IF not enough hits THEN set game over and GOTO game over logic
- ELSE GOTO next round

### Next Round
- Increment round, reset shots and hits, GOTO bird spawn

### Game Over
- Hide all sprites
- Turn off ammo indicators
- Wait for reset switch
- GOTO start/restart

### Splash Screen
- Set splash screen colors and score
- Animate bird and player positions
- IF fire button pressed THEN mark splash seen, set fire restrainer, hide splash, GOTO restore sprites
- ELSE GOTO splash screen

### Bird Flight Patterns
- IF pattern 0 THEN straight flight
- IF pattern 1 THEN wavy flight
- IF pattern 2 THEN aggressive flight
- IF pattern 3 THEN erratic flight

### Falling Bird
- Animate bird falling
- IF bird reaches bottom THEN show dog, reset dog state, GOTO exit flight sub

### Exit Flight Sub
- Update ammo indicators
- Draw screen
- IF reset switch pressed THEN restart main loop

---

This outline covers the main game logic, state transitions, and key IF/THEN decisions in the code. Details such as sprite data, color tables, and sound effects are omitted for clarity.
