## TODO List 

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
  - [ ] json for testing individual steps: placements and places 
  - [X] protocol for communication (first draft)
  - [ ] remote proxy for players 
  - [ ] remote proxy for tournament administrators s 

- [ ] GUI for watching the game (an observer) 
- [ ] ?? GUI for playing? 

## Bugs 

- [X] bug in printing, re width of names 
- [x] bug in equality of boards; extra buildings of height 0 don't matter 
- [X] mistake in dealing with "giving up" action, use string instead of worker
- [X] actions: check-action did not check all the conditions (contract system hooray!)
- [X] strategy: take-turn produces #f if the player gets stuck 2 turns down

- [X] OUCH: a bad contract and default values introduced a bug into the program 
