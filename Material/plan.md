# Stages of Santorini Building 

_Plan top down, build bottom up, and fix as you go._

Your friends love the game. You want to create a set-up where you can
explore game-playing strategies. You imagine that you, and possibly 
one of your friends, create players that play specific strategies. Then
you run thousands of rounds to find out which strategy works best. 

Since your friend's player implementation may accidentally violate the
rules of the game, you plan on implementing the game in a somewhat
realistic fashion. 

Of course, deep down you're hoping that Santorini.com will become the next
Facebook and make you infinitely rich.

## A Realistic Game 

means 

- there is a referee 
- there are players 
- there is an API for players to interact with the referee 

Questions: 

- Who needs to know what? 
- What is the common "ontology" (*) between referee and players? 
  (*) "shared data representation and interpretation"

Both the referee and the players need a representation of the basic game
entities: a board, the tokens and buildings on the board. 

## Stage 1 ~~ common ontology 

#### The Board

- initialize with four unique tokens, two per player 
- allow players to move to a neighboring place 
- allow players to build up a house on a neighboring tile 

#### The Buildings 

Nothing matters about them besides their height and (relative) location on
the board, so we'll keep them there. 

#### The Tokens

These can exist on their own, with relative movements and checks for
"neighborliness". 

#### The Player-Referee Interface

- how can the referee call a player 
- when a player takes a its turn, how does it communicate what it wants done

#### Actions 

- giving up 
- moving to win
- moving and building 

#### The Rules 

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

How do Players and the Referee use these rules? 

## Stage 2 

All of your remaining friends are software developers and they all favor
the same language as you. So you now have the idea to run a competition. 
Each of your friends will develop a player and plug into your game
framework. The winner will be allowed to buy (legal) drinks for everyone. 

### The Referee 

knows

- where each player is 
- where each building is 
- how tall each building is 
- whose turn it is 
- whether a player's move is legal 

checks each player's behavior (adherence to rules)

### A Player 

knows 

-- where its tokens are 
-- where other tokens are 
-- where buildings are 

It can generate all possible moves and then decide 
- to move one of the tokens according to the rules 
- to build on a neighboring field 

(in that order). 

### Testing 

Since y'all chosen different languages, I need a testing DSL. 
We will use JSON for that. Each test consists of two files: 
- an file that specifies the input for a function/method
- another file that specifies the expected output. 
You may also want a testing harness that runs such tests 
automatically. Or you do it by hand and perhaps remember 
the design recipe for abstraction anyway. 

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
- create a remote proxy for the referee  

## Stage 5 

Expand the server-client framework to run entire knock-out tournaments. 
Each round is a best-of series, with the winner moving on and the loser 
dropping out. 

A tournament pitches n players against each other in a round-robin
("everyone against everyone"]. A player is specified with two pieces of
information per line 
-- an identifying name; and
-- a path to a file that implements the player mechanics.

Each confrontation between two players is run as a "best of (3 for now)"
game.  A player that fails or cheats gets eliminated and all of its past
results are counter in favor of its opponents.

The End.
