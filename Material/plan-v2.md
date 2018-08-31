# Stages of Santorini 

_Plan top down, build bottom up, and fix as you go._

- Iterate. 
  - Plan the long term. 
  - Build bottom up according to the following bullets. 
  - Pick goal points that deliver complete demos. 
  - Build partial demos in between. 

## Warning 

You might find full implementations of Santorini out on the wild west
web. If you steal code, beware that you will need to *code walk* it. 
Also know that the rules posted here are subtly different from the official
ones and that, at some point, these rules may change dramatically---just
like the requirements for real-world software on your future co-ops or
jobs. 


## Goal 

All your hacker friends love Santorini. But just so you know, they hate
*your* programming language. So you think there is a dot.com in there.

You want to create a set-up where you and your friends and others can run
this game on-line via automated agents. You will run the santorini.com
server, and they will connect with players to this server to play Santorini
tournaments.

A tournament pitches every player against every other player. Since a
single game of Santorini isn't an indicator of the software agent's
"intelligence", each meeting of two players is played as a round of "best
of N" games. 

## Plan 

So that's the goal, now we need a plan. 

Let's consider some basic facts of life: 

- don't trust anyone over 21 or software that someone else has produced 
- meaning, this setup needs a referee for each game 
- and this referee deals with all game actions, 
- making sure that they are correctly executed and in the correct order 

### Stage 1 

This suggests we need at least three pieces to get to the first complete
demo: 

- a referee software component that can set up and run a game for two players
- player software 
- the common pieces so that the two can communicate in a meaningful manner 

For simplicity, we assume that these pieces of software are written in the
same language and that some configuration script dynamically links two 
players (specified in a configuration file or even on the command line) 
via a referee and runs a round of games. 

We are also happy with the referee delivering the result of a game to the
command line.

Because this step is relatively large, we break it up into several steps.

### Stage 2

To make this game even remotely interesting and good for a stage
development process, we need a non-trivial but deterministic strategy for
the players. 

### Stage 3

Once we have a referee working with two players, we can move on to a
tournament. To keep things simple, we still assume that all components are
written in the same language and that we configure them at start-up
time. But we also wish to bring this closer to the world-wide santorini.com
setting and thus act as if the components need to receive meta-information
from the tournament manager . 

- a tournament manager 
- an enhancement of the existing players to receive 'end of game' information
- an enhancement of the referee to run "best of N" rounds of games 

### Stage 4 

Thanks to software design patterns, it is a short step from a tournament
running on a single machine with all components written in the same
language, to a tournament software system that connects distributed agents
written in a variety of languages via TCP in a server-client system. What
we need are:  

- a server script 
- a client script 
- remote proxies for the tournament manager and the players 

And voila, everything will work just fine. 

## Testing 

Since you and your friends have chosen different languages but want to make
sure your development efforts are in sync, we need a way of specifying
tests, that is,

- a common data format for specifying inputs and expected outputs for each major component
- scripts that test (say from the command line) that your components work 

We will use one of the following three: 

- JSON 
- XML 
- S-expressions. 

Please vote for your favorite poison. It's all the same in Racket.

## Rule Changes 

- infinite EAST and SOUTH direction 

## Watching the Game. 

Hey, e-sports is a big thing now, but how are you going to get fans to
watch if there is no information about the games? 

So at some point, we also need to develop visualization software so that
observers can watch games or tournaments. 

Use software design patterns that come with the correct words, and
everything will be cool. 

## The End.
