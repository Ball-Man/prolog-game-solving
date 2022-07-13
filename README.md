# Simple Game Solving with Prolog
Through AND/OR game exploration.

## Prerequisites
SWI-Prolog version 8.4.2

## How to play
To run the included example simply run:
```
swipl main.pl
```
This will launch an m-n-k game (4-3-3). This specific game instance is a bit slow, it may require some seconds to make the first move (first move is for the AI). From the second one on, speed improves rapidly.

When requested, input shall be given in the form: `X-Y.` (notice the final dot). Such a move would place a stone at column `X` and row `Y`.

To quit the game, input `quit.`.


## How to change game
Nim game is also included in this repo. To play it, simply replace `main.pl` and `game.pl` with the ones in the `nim/` folder.

Moves have similar format: `H-N.` where `H` is the heap index, `N` is the number of coins to remove.

More games can be added by developing a custom `game` module.

## Feeling bored?
If you don't want to think your moves, you can play a legal move automatically by inputting a capital capital letter as input (will be interpreted as a Prolog variable and automatically unified with a legal move).

Eg. `A.`
