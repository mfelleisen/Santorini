## The Remote Proxies for TCP

The monolithic Santorini protocol in `Admin`, `Player`, and `Common` (plus
`Lib`) is turned into a server-client arrangement via the remote proxy
pattern: 

- [`player`](player.rkt) implements a remote proxy for a player on the
  server side 

- [`tournament-manager`](tournament-manager.rkt) implements a relay,
  connecting the remote-proxy player to the actual player by translating
  TCP messages into method calls for the player and return values from such
  calls into TCP messages for the server. 

To turn these components into a complete arrangement, the following files
provide the remaining functionality: 

- [`server`](server.rkt) implements a server that collects player connections,
  creates remote-proxy players, and---when there are enough---hands these
  players to a `tournament-manager`, which in turn runs a round-robin tournament
  for all players. The configuration for a server looks roughly like this: 

```
{ 
  "min players" : Natural, 
  "port"        : Natural, 
  "waiting for" : PositiveNumberOfSeconds, 
  "repeat"      : 0or1
}

```

- [`client`](client.rkt) is a module for launching a number of players as
  clients to the server. The players are specified in a configuration file. 
  The latter have this shape: 

```
{ 
  "players"   : [[Kind, Name, PathString], ..., [Kind, Name, PathString]],
  "observers" : [[Name, PathString], ..., [Name, PathString]],
  "ip"        : String,
  "port"      : Number 
}
```

The TCP/JSON protocol is specified in a [separate file](protocol.md).
