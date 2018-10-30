## The Local Interaction Protocol (Monolithic) 

The interaction between Racket players and the Racket _Admin_ framework is
governed by the set of following diagrams. 

### Tournament Set-up 

```
   manager <-------------- player (p1)  . . . player (pn)
     |                        |        |      |
     |                        |        |      |
     |------------------------|--------|----->|   playing-as(string) % in case of name clash 
     |                        |        |      |
     |------------------------|------->|      |   playing-as(string) % in case of name clash 
     .                        .               .
     |                        |               |
     |               referee  |               |
     |--new(p1,p2)-------+    |               |
     |                   |    |               |
     .                   .    .               .   an encounter between player p1 and p2     
     |<================= |    |               |   result: string or string plus termination notice
     |                   _    |               |
     |                        |               |
     |                        |               |
     |               referee  |               |
     |--new(p1,p3)-------+    |               |
     |                   |    |               |
     .                   .    .               .   an encounter between player p1 and p3
     |<================= |    |               |   result: string or string plus termination notice
     |                   _    |               |
     |                        |               |
     | ---------------------> |               |   end-of-game(results/c)
     |                        |               |
     .                        .               .
     | -------------------------------------> |   end-of-game(results/c)
     .                        .               .   for all surviving players 
     |                        |               |
```

Terminated players no longer compete and their past games are re-evaluated. See _Admin_ for policy
and its implementation.  


### A Referee Interaction 

```
  referee             player: p1          player: p2
     |                   |                    |
     |-----------------> |                    |   other-name(string)
     |                   |                    |   (the name of the other player,
     |                   |                    |   which is also the name of its workers)
     |--------------------------------------> |   other-name(string)
     |                   |                    |
     |-----------------> |                    |   placement(placements/c)
     | <================ |                    |   place/c
     | -------------------------------------> |   placement(placements/c)
     | <===================================== |   place/c
     |-----------------> |                    |   placement(placements/c)
     | <================ |                    |   place/c
     | -------------------------------------> |   placement(placements/c)
     | <===================================== |   place/c
     |                   |                    |
     |-----------------> |                    |   take-turn(board/c)
     | <================ |                    |   action/c
     | -------------------------------------> |   take-turn(board/c)
     | <===================================== |   action/c
     |                   |                    |
     .                   .                    .
     |-----------------> |                    |   take-turn(board/c)
     | <================ |                    |   action/c
     | -------------------------------------> |   take-turn(board/c)
     | <===================================== |   action/c
```

An interaction ends normally if a player wins or a player gives up. 

An interaction is terminated if a player breaks the rules, raises an exception, or takes too long
to complete an interaction. 
