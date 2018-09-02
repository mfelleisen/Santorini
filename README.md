# The Santorini Game Explored 

## Material 

| file			 | description					|
| ---------------------- | -------------------------------------------- |
|  game-description 	 | a description suitable for Sw Dev 		|
|  plan 		 | Santori specific thoughts  	 		|
|  plan-v2 		 | how to split the game according to the sw dev|
|  rules.pdf 		 | the official rules, distinct from 4500's	|

## Code: game-specific code 
  
### Lib: things Racket should probably have

| file			 | description					|
| ---------------------- | -------------------------------------------- |
|  xsend 		 | protect method calls from exns, time-outs 	|
|  io.rkt 		 | dealing with tcp io (in/out messages) 	|
|  json-pretty 		 | pretty printing json        			|
|  with-output-to-dev-null | hiding some output (for tests)		|
|  set-from 		   | create a set from a list & an equivalence	|
|  require 		   | require- and require+    	   		|
|  struct-with 		   | a with construct				|

### Common: the common ontology of players and administrators 

| file			 | description					|
| ---------------------- | -------------------------------------------- |
|  player-interface 	 | for referees to interact with players 	|
|  observer-interface 	 | for connecting an observer to a referee's 'play' |
|  rule-checking 	 | referees and players need to agree on the rules |
|  			 |						|
|  actions 		 | players' way to express how they act on their turn |
|  placements 		 | player's way to deal with placing their workers |
|  results 		 | communicating the results of a tournament 	   |
|  			 |						|
|  board 		 | the game board, incl. buildings 		|
|  buildings 		 | buildings, of course, just their height 	|
|  directions 		 | where workers can move, where they can build |
|  worker 		 | the tokens (mostly hidden) 	      	  	|

### Player: the player mechanism and the player strategy 

| file			 | description					|
| ---------------------- | -------------------------------------------- |
|  super  		 | the base class that implements the mechanics |
|  player 		 | the mechanics with a strategy      		|
|  textual 		 | a primitive interactive player		|
|  failiang 		 | players that can fail during placement and playing |
|  strategy 		 | evaluate the game tree to some depth	        |
|  move-generating 	 | build a game tree 	     	  	      	|

### Admin: the admin mechanism 

| file			 | description					|
| ---------------------- | -------------------------------------------- |
|  referee 		 | safely play a game or a round of games between two players |
|  tournament-manager 	 | set up and run a round-robin tournament 	  |
|  			 |     	      	    				  |
|  primitive game and tournament setups | |
|  run-a-game 	        | run a single game for two players		|
|  run-a-tournament 	| run a tournament for N players		|
|  run-me-vs-someone 	| run an interactive game 			|
    
### Remote: everything needed to play remotely 

| file			 | description					|
| ---------------------- | -------------------------------------------- |
|  server 		 | a server for distributed tournaments 	|
|  client 		 | set up a remote proxy player, connect via proxy tournament-manager|
|  player 		 | a remote proxy player 	 	     	   |
|  tournament-manager 	 | a remote proxy tournament-manager 		   |
