# Stages of Santorini Building 

## Stage 1 

Your friends love the game. You want to create a set-up where you can
explore game-playing strategies. You imagine that you, and possibly 
one of your friends, create players that stick to a specific strategy. Then
you run thousands of rounds to find out which strategy works well. 

Since your friend's player implementation may accidentally violate the
rules of the game, you plan on implementing the game in a somewhat
realistic fashion. 

Of course, deep down you're hoping that Santorini.com will become the next
Facebook and make you infinitely rich. 

### The Board 

- initialize with four unique tokens, two per player 
- allow players to move to a neighboring place 
- allow players to build up a house on a neighboring tile 

### The Rules 

- a player can move to a neighboring place if 
  - there is no other player on that field, 
  - he is "jumping" down from a building (of arbitrary height), or
  - the building on this place is only one step taller than the one he is on
    but not capped (fourth floor). 

- a player can add a level to a neighboring field if the building isn't
  already 3 storied tall 

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

### A Player 

knows what to do next when given the current game configuration: 

- to move one of the players according to the rules 
- to build (optionally) on a neighboring field 

(in that order). 

### The End 

The game ends if a player reaches the third level of a building. 

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
