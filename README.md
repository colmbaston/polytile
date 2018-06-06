# polytile

A Haskell program for tiling one-sided polyominoes on a rectangular grid.
The program is capable of handing any polyomino, but currently only accepts the seven one-sided tetrominos (polyominos of size four) from the command-line.
I originally wrote this program because I was too lazy to actually solve the tetromino tiling puzzles in [the Talos Principle](http://www.croteam.com/talosprinciple/), but not lazy enough to just follow a guide.

## Usage

The program should be called with the first two command-line arguments representing the size of the grid, width and height respectively, and the rest being a list of polyominoes to tile.

```
> ./polytile 8 6 I J J J J L O O T T Z Z
Searching for tilings...

 o o o o - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -

 - - - - - - - -
 o - - - - - - -
 o - - - - - - -
 o o - - - - - -
 - - - - - - - -
 - - - - - - - -

 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 o o - - - - - -
 o o - - - - - -

 - - - - - - - -
 - o o - - - - -
 - o o - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -

 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - o o - - - -
 - - o - - - - -
 - - o - - - - -

 - - - - o - - -
 - - - o o - - -
 - - - o - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -

 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - o - - - -
 - - - o o o - -

 - - - - - - - -
 - - - - - o - -
 - - - - o o - -
 - - - - o - - -
 - - - - - - - -
 - - - - - - - -

 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - o - -
 - - - - o o o -
 - - - - - - - -

 - - - - - o o o
 - - - - - - - o
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - -

 - - - - - - - -
 - - - - - - o -
 - - - - - - o o
 - - - - - - o -
 - - - - - - - -
 - - - - - - - -

 - - - - - - - -
 - - - - - - - -
 - - - - - - - -
 - - - - - - - o
 - - - - - - - o
 - - - - - - o o
```

Currently available polyominoes are:

* I:

```
 o
 o
 o
 o
```

* J:

```
   o
   o
 o o
```

* L:

```
 o
 o
 o o
```

* O:

```
 o o
 o o
```

* S:

```
   o o
 o o
```

* T:

```
 o o o
   o
```

* Z:

```
 o o
   o o
```
