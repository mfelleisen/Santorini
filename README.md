# The Santorini Game Explored 

## Files 

- plan.md ~~ how to develop the game for the students 
- rules.pdf ~~ the official rules of the game 

## Directories 

- Code ~~ game-specific code 
  - Lib: things Racket should have probably
    - set-from ~~ create a set from a list and an equivalence relation
    - require ~~ require- and require+ 
    - struct-with ~~ a with construct 
  - Common: the common ontology of players and administrators 
    - actions ~~ players' way to express how they act on their turn 
    - buildings ~~ buildings, of course, just their height 
    - board ~~ the game board, incl. buildings 
    - directions ~~ where workers can move, where they can build 
    - player-interface ~~ for referees to interact with players 
    - rule-checking ~~ referees and players need to agree on the rules 
    - workers ~~ the tokens (mostly hidden) 
  - Player: the player mechanism and the player strategy 
    - player ~~ the mechanics 
    - move-generating ~~ build a game tree 
    - strategy ~~ evaluate the tree (just one so far)
  - Admin: the admin mechanism 
    - referee ~~ the mechanics 
    - referee-interface ~~ how a tournament administrator interacts with referees
    
