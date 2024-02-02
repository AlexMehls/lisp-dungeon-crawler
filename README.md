# lisp-dungeon-crawler

## Dependencies
To install the required packages via quicklisp, load "quicklisp-packages.lisp".
On windows, the cl-cffi-gtk package may be missing some DLLs. See "gtk dependencies windows.txt" for details.

## Controls
- Mouse 1 to shoot
- WASD to move
- F11 for fullscreen
- Mouse Wheel to zoom
- F3 to enable debug features:
  - Unlimited zoom
  - F1 to immediately load next level

## Gameplay
- Move through randomly generated rooms to find the ladder to the next level
- Red enemies chase down the player and deal damage on contact
- Purple enemies stay at a distance from the player and shoot projectiles
- All enemies have 3 HP
- The player has 10 HP (Displayed at the top left corner of the screen)
- Player projectiles can pierce through 1 enemy

## Additional Notes
- The file "waaf-cffi.lisp" is taken from the repository https://github.com/jetmonk/waaf-cffi
- There is no "Game Over" screen (instead the player is destroyed as any enemy would be)
