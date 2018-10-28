## The Admin Arrangement 

The game administration framework consists of two components: 

-- tournament-manager 
-- referee 

### Managing a Tournament 

A _tournament-manager_ runs a tournament of N players, every player against
every other player. Specifically, the first player is schedule to play
against the remaining players, then the second player runs against all
remaining others (the first vs the second is already scheduled), and so
on. Each encounter is a "best of 3" series of games. 

To ensure that the tournament makes sense, the manager assigns a new name
to all but one of the players that share a name.  To this end, it expects
the _player_ component to come with the functionality to "play as".

To run an encounter, the _tournament-manager_ delegates to a _referee_. The
latter reports back a result that indicates the winner and optionally the 

The tournament manager comes with the functionality to read a tournament
configuration file, which 

```
{ "players"   : [[Kind, Name, PathString],
                 ..., 
                 [Kind, Name, PathString]],
  "observers" : [[Name, PathString],
                 ..., 
                [Name, PathString]]
}

```
where 
- `Kind` is either "good", "breaker", "infinite"
- `Name` is a JSON String
- `PathString` is a Linux Path to an executable that implements the respective player or observer


### Managing an Encounter 

A _referee_ monitors an encounter between two players. It either runs a
single game or a series of games until either one of the players has won
more than half the games or one of the players has misbehaved (raised an
exception, timed out, broke rules, etc). In the former case, it informs the
tournament manager (or caller) of the winner's name; in the latter case it
additionally informs the context that a failure happened. 

The _referee_ is created with the two players and their (possibly assigned)
names. 

The _state of a game_ consists of the game board and the (assigned) names
of the players. 


### Additional Files for Testing 

- `run-a-game` ~~ run a single game with an observer 
- `run-me-vs-someone` ~~ as the name says 
- `run-a-tournament` ~~ run a tournament -




