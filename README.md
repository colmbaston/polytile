# polytile

A Haskell program for tiling one-sided [polyominoes](https://en.wikipedia.org/wiki/Polyomino) on a rectangular grid.
The program is capable of handling any polyomino, simply representing them as sets of coordinates internally, but currently only accepts the seven one-sided tetrominos (polyominos of size four) from the command-line.
I originally wrote this because I was too lazy to actually solve the tetromino tiling puzzles in [the Talos Principle](http://www.croteam.com/talosprinciple/), but not lazy enough to just follow a guide.

## Usage

The program should be called with the first two command-line arguments representing the size of the grid, width and height respectively, and the rest being a list of polyominoes to tile.

```
> ./polytile 6 6 L L L L O S S S S
Searching for tilings...

 ■ ■ ■ □ □ □
 ■ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □

 □ □ □ □ □ □
 □ ■ ■ □ □ □
 ■ ■ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □

 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 ■ □ □ □ □ □
 ■ □ □ □ □ □
 ■ ■ □ □ □ □

 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ ■ □ □ □ □
 □ ■ ■ □ □ □
 □ □ ■ □ □ □

 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ ■ ■ □ □
 □ □ ■ ■ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □

 □ □ □ ■ □ □
 □ □ □ ■ ■ □
 □ □ □ □ ■ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □

 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ ■ ■
 □ □ □ ■ ■ □
 □ □ □ □ □ □

 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ ■
 □ □ □ ■ ■ ■

 □ □ □ □ ■ ■
 □ □ □ □ □ ■
 □ □ □ □ □ ■
 □ □ □ □ □ □
 □ □ □ □ □ □
 □ □ □ □ □ □

```

The currently available polyominoes are:

* I

```
 □ □ □
 □ ■ □
 □ ■ □
 □ ■ □
 □ ■ □
 □ □ □
```

* J

```
 □ □ □ □
 □ □ ■ □
 □ □ ■ □
 □ ■ ■ □
 □ □ □ □
```

* L

```
 □ □ □ □
 □ ■ □ □
 □ ■ □ □
 □ ■ ■ □
 □ □ □ □
```

* O

```
 □ □ □ □
 □ ■ ■ □
 □ ■ ■ □
 □ □ □ □
```

* S

```
 □ □ □ □ □
 □ □ ■ ■ □
 □ ■ ■ □ □
 □ □ □ □ □
```

* T

```
 □ □ □ □ □
 □ ■ ■ ■ □
 □ □ ■ □ □
 □ □ □ □ □
```

* Z

```
 □ □ □ □ □
 □ ■ ■ □ □
 □ □ ■ ■ □
 □ □ □ □ □
```
