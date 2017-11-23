Rock Scissors Paper Lizard Spock
================================

Create a system that can simulate a rock scissors paper lizard spock (RSPLS)
tournament.

# Rules of the Game

A game will start, and both player will select either Rock, Scissors, Paper,
Lizard or Spock.

The rules for determining the winner are as follows:

- Scissors cuts Paper
- Paper covers Rock
- Rock crushes Lizard
- Lizard poisons Spock
- Spock smashes Scissors
- Scissors decapitates Lizard
- Lizard eats Paper
- Paper disproves Spock
- Spock vaporizes Rock
- Rock crushes Scissors

![diagram](http://i0.kym-cdn.com/photos/images/newsfeed/000/521/596/ff1.jpg)

## Basic Mode

Write a program that:

- Take a list of player names
- Simulates games of LPSLS between the all players (everyone plays everyone)
- Each match consists of best of 5 (configurable) games between each player
- Determines the winner based on number of match wins, and then game wins

Example - Start Options:
``` bash
$ ./rpsls --players holsee, zulu, oscar --best-of 5
```

Example - Verbose logging output:
```
Game: holsee v zulu, holsee picked rock, zulu picked lizard, rock crushes lizard
Match: holsee 1 - 0 zulu

Game: holsee v zulu, holsee picked rock, zulu picked spock, spock vaporizes rock
Match: holsee 1 - 1 zulu

Game: holsee v zulu, holsee picked rock, zulu picked lizard, rock crushes lizard
Match: holsee 2 - 1 zulu

Game: holsee v zulu, holsee picked rock, zulu picked lizard, rock crushes lizard
Match: holsee 3 - 1 zulu, holsee wins the match!
```

Example - Results table:
```
| player | match wins | game wins |
| ------ | ---------- | --------- |
| holsee |  2         |  6        |
| zulu   |  1         |  4        |
| ada    |  1         |  3        |
```


## Hard Mode Additions:

### Strategies

Design your player bot so that it can implement different strategies, and adapt
based on opponents historical selections.

### Distributed RSPLS!

Extract the game coordination logic into a server component that can coordinate
remote actors as players within a tournament.

This should allow any player bot, written in any language to play each other in
a tournament.

E.g.

[Server] Game Coordinator:
- broadcast new tournament
- schedule games
- start game
- compute winner
- store results
- broadcast results

[Client] Player Bot:
- listen for new tournament
- register in tournament
- listen for game start
- provide selection (RPSLS)
- obtain result
- listen for tournament results

