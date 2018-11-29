## The Common Ontology .. Understanding between the Player and the Admin 

This directory defines the interface between a (local) Santorini player and
the gaming framework (_Admin_). This includes 

- the class contract for a player 
- the data exchanged between the _Admin_ framework and a player. 


### The Game Pieces 

The game framework relies on the following data representations of the
"physical" game pieces: 

- [`board`](board.rkt) ~~ the board consists of `worker`s and `building`s
- [`workers`](workers.rkt) ~~ a worker is labeled as belonging to a player (via name) and index
- [`buildings`](buildings.rkt) ~~ a building is simply a height: 0 (no building), ..., 4 (capped building)
- [`directions`](directions.rkt) ~~ the framework uses cardinal directions to determine how 
  - a player moves 
  - a player builds 

### The Interactions between the Player and the Admin 

- [`player-interface`](player-interface.rkt) ~~ the referee is expected to call the specified
  methods of the player at the correct point in time; each call is
  considered an observable event. 

- [`rule-checking`](rule-checking.rkt) ~~ the `referee` and the `player` may use this rule
  checker to confirm the validity of a turn action. 
  
  The referee checks the validity of a placement interaction
  separately. A newly chosen place must be distinct from all
  already-occupied places. 

### Representing the Information that Flows from Referees to Players and Vice Versa

The player interface demands agreement on a number of additional forms of
data: 

- [`placements`](placements.rkt) ~~ where workers have already been placed and where a new
  worker is to be placed next 

- [`actions`](actions.rkt) ~~ the action a player wishes to take next given the current
  game state (`board`): 
  - move and build 
  - just move to win 
  - giving up 

- [`results`](results.rkt) ~~ how the tournament manager informs the players of the
  results of a tournament 

### Observers 

The framework should eventually implement observers for 

- games ~~ see [`observer-interface`](observer-interface.rkt), which observes all game events: 
  - the placement of a worker 
  - the execution of a turn request by a player 
  - the board resulting from a turn 

### Ideas, Not Implemented 

- observers for tournaments
- observers for server/client interactions on TCP
