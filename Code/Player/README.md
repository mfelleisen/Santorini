## 

A Player implements the mechanics of the Common/player-interface, meaning 

- it could realize a MVC architecture to interact with a human being 
- it could perform remote procedure calls to other computers where a player
  is implemented 
- it could defer to an AI strategy. 

Since there is clearly code that overlaps in all cases, the Player folder
comes with a `super` class plus two concrete implementations: `player`, 
which defers to an AI, and `textual`, which defers to a human being. 
Furthermore, the folder also provides player implementations that break at
specific points.

In addition to the player implementations, the folder includes modules for
implementing an AI `strategy`. 

```
                        +----------------------+
                        |       super%         |
                        +----------------------+
                                  |
                                  ^
                                  |
            +----------------------------------+
            |                     |            |
   +----------------------+       |     +---------+     +------------------+
   |       textual%       |       |     | player% |*--->| strategy%        |
   +----------------------+       |     +---------+     +------------------+
                                  |                     | mover-generating |
                                  |                     | (separate module)|
                                  |                     +------------------+
                                  |             
                                  |             
            +---------------------------------------------------------------+
            |                     |                      |                  |
+-----------------------+ +------------------+ +-------------------+ +--------------+
| failing-inf-placement | | failing-inf-turn | | failing-placement | | failing-turn |
+-----------------------+ +------------------+ +-------------------+ +--------------+        

failing.rkt : is a mixin function that generates the failing sub-classes
            : it is also used for creating mock players in unit tests 
```
