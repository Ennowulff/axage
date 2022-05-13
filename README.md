# AXAGE ~ ABAP teXt Adventure Game Engine
![axage-logo](https://github.com/Ennowulff/axage/blob/8c7adcaf8e7b0af5f697b1f021c5cf16fd4e9608/img/axage_logo.png)

# Game engine
A simple game engine as base for text adventures

# maps
define rooms with exits to north, east, south and west

# actors
define actors 

# things
define things that can be found, taken or dropped.

## openable things
create things that can be opened using special things

# parser
use simple two-word commands to navigate in the world

## commands
```
N or NORTH        Go to the room on the north side
E or EAST         Go to the room on the east side
S or SOUTH        Go to the room on the south side
W or WEST         Go to the room on the west side
MAP               show floor plan/ world

INV or INVENTARY  Show everything you carry
LOOK              Look what''s in the room
LOOK <object>     Have a closer look at the object in the room or in your inventory
TAKE <object>     Take object in the room
DROP <object>     Drop an object that you carry
OPEN <object>     Open something that is in the room
```
