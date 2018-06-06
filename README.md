# polytile

A program for tiling one-sided polyominoes on a rectangular grid, written in Haskell. The program is capable of handing any polyomino, but currently only accepts the seven one-sided tetrominos (polyominos of size four) from the command-line. I originally wrote this program because it seemed like a more interesting puzzle than actually solving the tetromino tiling puzzles in [the Talos Principle](http://www.croteam.com/talosprinciple/).

## Usage

The program should be called with the first two command-line arguments representing the size of the grid, width and height respectively, and the rest being a list of polyominoes to tile.

```
> ./polytile 7 4 I I L S T T Z
Searching for tilings...

 - - - - - - -
 - - - - - - -
 - - - - o - -
 - - - o o o -

 - - - - - - -
 - - - - - - -
 - - o o - - -
 - o o - - - -

 - - - - - - -
 - - - - - - o
 - - - - - o o
 - - - - - - o

 - - - - - - -
 - - o o o o -
 - - - - - - -
 - - - - - - -

 - - - - - - -
 - o - - - - -
 o o - - - - -
 o - - - - - -

 - - - o o o o
 - - - - - - -
 - - - - - - -
 - - - - - - -

 o o o - - - -
 o - - - - - -
 - - - - - - -
 - - - - - - -
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

*Z:

```
 o o
   o o
```
