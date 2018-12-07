## A Full-fledged Santorini Gaming Framework 

This directory implements a distributed Santorini gaming framework, both the
client and the server side. The _Remote_ directory provides the

- `server` and 
- `client` 

programs plus some remote-proxy components for turning the monolithic
version into a distributed one.

The server waits for a certain number of seconds to sign up a (minimum)
number of remote players. Once there are enough players, the server
hands the (proxy) players to the _tournament manager_, which implements the
logic of running a complete "everyone plays everyone" tournament. 

By connecting remote clients to the _tournament manager_, we isolate their
control-flow disturbing, memory, and file-access capabilities. But we still
wish to make the server robust enough so that exception-raising flaws in
the player do not kill the server completely (if possible). We therefore 
place the protections into the administrative components (_tournament
manager_, _referee_). For now these protections include 

- catching all exceptions and 
- limiting time consumption 

of any call to a _player_ component. Any errors of this kind are currently
logged to `error`. 

The client connects to a remote server and then waits for "remote function
calls", which are translated into method calls for the local player
component. 

## Designing a Santorini Player 

### Local Interface 

To design a Santorini player, study 

- [Common/README.md](Common/README.md)

first. It specifies the complete interface between a Santorini player and
the _Admin_ gaming framework in Racket. For the interaction protocol, see 

- [Common/protocol.md](Common/protocol.md)

### Remote Interface 

The remote protocol is specified in 

- [Remote/protocol.md](Remote/protocol.md)

It implements the local interface via remote procedure/method calls that
use JSON to represent the methods and the arguments. 

For a sample server/client implementation, see 

- [Remote/README.md](Remote/README.md)

## Running a Monolithic Santorini Gaming Framework 

The _Admin_ and _Player_ directories implement the essential components for
a *monolithic* Santorini gaming framework. How these two interact is spelled
out in the files of the _Common_ directory. 

The distributed version of the gaming framework is derived from the
monolithic one via the _remote proxy design pattern_. 

The _Admin_ directory implements a _tournament manager_, which manages the
entire tournament. To conduct an individual encounter, it delegates to the
_referee_. 

The _Player_ directory implements two kinds of Santorini players, derived
from a `super` class: 

- `player` uses a simplistic "stay alive" strategy with a parametrized
  look-ahead strategy for a game tree. It delegates its decisions to a
  `strategy` component. This code is simplistic; it allows the team to run
  a complete tournament. 

- `textual` presents the game information to a human player who can then
  make decisions. The UX is an extreme simple read-eval-print loop. 

The _Common_ directory spells out all data representations that _Admin_ and
_Player_ must share and have a common understanding of. The first cluster
concerns the basic game pieces: 

- the Santorini `board`
- the `worker`s
- the `building`s
- the `direction`s 

The second cluster is about how the _Admin_ components may interact with
the _Player_: 

- the `player-interface`, including methods 
  - about the game name of a player (in case of name clashes) 
  - about the name of the opponent (called at the beginning of an encounter) 
  - for setting up the initial game board 
  - for taking a turn 
  - for being told about the result of an individual game 

In turn, these methods demand further common data representations, namely,
about 

- the information a player needs to place the next worker (`placements`)
- .. and the information that is returned 
- the information a player needs to choose its next turn 
- .. and the information about what it wishes to do (incl. giving up) 
- and the information it receives at the end of the game. 

The _Lib_ directory contains common pieces of functionality that should be
part of the programming language but aren't. 

## Observers 

A game-level observer protocol exists and can be used from the referee. 

## Not implemented 

- The client configuration element for observers is ignored. 
- The server isn't stress tested; scheduling might be an n^2 algorithm. 

## NSF 
- [X] protocol prototype 

## Basic Game 
- [x] common items: board, building, tokens ~~ no awareness of rules 
- [x] referee: rule checking, mechanics 
- [x] player: game tree generation, strategy, mechanics 
- [x] MISTAKE: action per turn for communication belongs into common ontology 

### refactorings 
- [x] check actions, not individuals moves in Admin 
  - [x] simplify the define-board syntax so that (list 2 "x") can be specified as 2x
- [x] re-arrange check-build so that it takes the existing board/token/move/build-plan
- [ ] can we hide coordinates completely? 
  - [x] rename token to worker 
  - [x] MAJOR DESIGN FLAW: workers should not come with locations at all, refactor 

- [X] extract patterns from key board functions 
- [X] A DESIGN FLAW: buildings should not come with coordinates 
- [X] the player's interface (not the implementation) also belongs into Common/

- [X] improve interface between referee and player 

- [X] factor out strategy for placing workers 

### testing 
- [X] complete tests for strategies 
- [X] set up tests for players, just mechanics 
- [X] set up tests for referee, competed 

## Larger Follow-up Steps 
- [X] board printer 
- [X] action printer 
- [X] make referee robust against failures in player code;
      it's okay to assume correctness of Common code 
  - [X] added generic xsend method (time, exns, raises)
- [X] improve strategy unrolling so that 
  - [X] initialization can also use it: FAILED! 
  - [X] place as far away as possible from other player, as close as possible to own
- [X] best-of N run in referee, instead of a single run 
- [X] tournament for P players and O observers 
  - [X] DESIGN FLAW: player must consume the strategy itself 
    - [x] turns out, this is a mistake but it's not quite an undo
                because what I want is configure the same mechanical player
                with different strategies and then load those dynamically 
  - [X] DESIGN FLAW: a strategy module must export a strategy, no funny names;
          otherwiseit can't be loaded dynamically 
- [ ] distributed version 
  - [X] json for testing individual steps: board 
  - [X] json for testing individual steps: action
  - [X] json for testing individual steps: placements and places 
  - [X] protocol for communication (first draft)
  - [X] remote proxy for players 
  - [X] fix up common and use for remote proxy 
  - [X] fix up referee interface (if needed) for next step 
  - [X] remote proxy for tournament administrators s 
  - [x} communicate change of name
    - [x ] testing: communicate change of name
  - [x] how to communicate final results 
    - [X] test communication of final result (does it go out;
      does it arrive; don't tell cheaters; what if a player fails during
      this step?)
  - [ ] test server with players that fail protocol 

- [X] GUI via observer pattern
- [ ] GUI for watching a game, a round, or a tournament (observer pattern) 
- [ ] ?? GUI for playing? 

- [X] configurations for tournament done properly 

## Refactorings 

- [x] move protocol from strategy to player
- [X] separate protocol from contract 
- [X] create player superclass and create subclasses for failing and plain strategy plus remote
- [X] display to log-error 

## Bugs 

- [X] bug in printing, re width of names 
- [x] bug in equality of boards; extra buildings of height 0 don't matter 
- [X] mistake in dealing with "giving up" action, use string instead of worker
- [X] actions: check-action did not check all the conditions (contract system hooray!)
- [X] strategy: take-turn produces #f if the player gets stuck 2 turns down

- [X] OUCH: a bad contract and default values introduced a bug into the program 

- [X] referee: bug in error reporting of rule violators for bad moves/build-ups
- [X] rule-checker: you can't build on a place occupied by another worker 

- [X] referee and actions: did not ensure that the player moved one of its own workers
- [X] referee: the error reporting was correct, my expected answer was wrong; see above 

- [X] strategy: failed to check whether the opponent could win 
- [X} strategy: failed to count properly 

- [X] referee and tournament-manager: there is one piece of game state
  besides the board, namely, the names assigned to the players (in case of
  overlapping names). Instead of using the assigned name from the schedule, 
  the tournament manager asked the player to "know" their names and thus
  used _externally supplied_ software components to keep track of game
  state (which they may or may not, or may not do so honestly). 

- [X] setup new board per game -- This was a major logical bug and it
  wasn't discovered because the tests didn't inspect the best-of 3
  properly. 

- [X] don't use message-based communication in remote proxy
  the server may take its time. 

- [X] make sure player names are player names   
- [X] transmit worker names in protocols 
- [X] failing players with proper descriptions and more options 
- [X] protocol violation for giving-up action 

## Racket Issues 

- in class contracts, field sub-contracts can't refer to other fields 

