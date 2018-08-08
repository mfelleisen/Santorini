# The Santorini Game Explored 

## Files 

- plan.md ~~ how to develop the game for the students 
- rules.pdf ~~ the official rules of the game 

## Directories 

- Code ~~ game-specific code 
  - Lib: things Racket should have 
  - Common: the common ontology of players and administrators 
    - actions ~~ players' way to express how they act on their turn 
    - board ~~ the game board, incl. buildings 
    - tokens ~~ the tokens (mostly hidden) 
  - Player: the player mechanism and the player strategy 
    - player ~~ the mechanics 
    - move-generating ~~ build a game tree 
    - strategy ~~ evaluate the tree (just one so far)
  - Admin: the admin mechanism 
    - admin ~~ the mechanics 
    - rule-checking ~~ a rule-checking mechanism 

