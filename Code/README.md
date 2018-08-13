## TODO List 

## Basic Game 
- [x] common items: board, building, tokens ~~ no awareness of rules 
- [x] administrator: rule checking, mechanics 
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

### testing 
- [ ] set up tests for players 
- [ ] set up tests for admins 

## Larger Follow-up Steps 
- [X] board printer 
- [X] action printer 
- [ ] GUI for watching the game (an observer) 
- [ ] ?? GUI for playing? 
- [ ] best-of N run in admin, instead of a single run 
- [ ] tournament for P players 
- [ ] distributed version 

## Bugs 

[X] bug in printing, re width of names 
[x] bug in equality of boards; extra buildings of height 0 don't matter 

