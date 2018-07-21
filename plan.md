# Stages of Santorini Building 

## Stage 1 

Your friends love the game. You want to create a set-up where you can
explore game-playing strategies. You imagine that you, and possibly 
one of your friends, create players that play specific strategies. Then
you run thousands of rounds to find out which strategy works best. 

Since your friend's player implementation may accidentally violate the
rules of the game, you plan on implementing the game in a somewhat
realistic fashion. 

Of course, deep down you're hoping that Santorini.com will become the next
Facebook and make you infinitely rich.

### A Realistic Game 

means 

- there is an administrator 
- there are players 
- there is an API for players to interact with the administrator 

Questions: 

- Who needs to know what? 
- What is the common "ontology" "shared data representation and interpretation"? 

Both the admin and the players need a representation of the basic game
entities: a board, the tokens and buildings on the board, etc.

### The Board 

- initialize with four unique tokens, two per player 
- allow players to move to a neighboring place 
- allow players to build up a house on a neighboring tile 

## Stage 2 

All of your remaining friends are software developers and they all favor
the same language as you. So you now have the idea to run a competition. 
Each of your friends will develop a player and plug into your game
framework. The winner will be allowed to buy (legal) drinks for everyone. 

### The Administrator 

knows

- where each player is 
- where each building is 
- how tall each building is 
- whose turn it is 
- whether a player's move is legal 

### A Player 

wants to pick the next move. It needs to know what the current state of the
game is. Based on that, it decides 

- to move one of the players according to the rules 
- to build (optionally) on a neighboring field 

(in that order). 

### The Rules 

The game is governed by the following basic rules, including the decision
to end the game: 

- if it is player's P turn, P must (1) move one token and (2) build up one
  building after the move 

- a player can move to a neighboring place if 
  - there is no other player on that field, 
  - he is "jumping" down from a building (of arbitrary height), or
  - the building on this place is only one step taller than the one he is on
    but not capped (fourth floor). 

- a player can add a level to a neighboring field if the building isn't
  already 3 storied tall 

The game ends

- if player A's token reaches the third level of a building.
- if player A can't move or, after the move, can't build up a building

How do Players and the Administrator use these rules? 

## Stage 3

Develop visualization software for the game state. 

## Stage 4

Your friends hate your language. It has an ugly syntax. It uses white space
to specify scope. It lacks a type system. Its libraries suck. And their
only goal is to bring down your software framework so that they can show
off how little you know. 

So you decide to replace the player API with a (Transfer) Protocol. That
way you can run your friends' player implementation in separate processes
or, even better, on separate machines so that there is no way they can
wreck your own machine. 

### The Remote Proxy Protocol 

- develop a server 
- create a remote proxy for the players 

- develop a client wrapper (for player implementations in your chosen language)
- create a remote proxy for the administrator 

## Stage 5 

Expand the server-client framework to run entire knock-out tournaments. 
Each round is a best-of-five series, with the winner moving on and the
loser dropping out. 

The End. 
