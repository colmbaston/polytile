# polytile

A Haskell program for tiling one-sided [polyominoes](https://en.wikipedia.org/wiki/Polyomino) on a rectangular grid.
The program is capable of handling any polyomino, simply representing them as sets of coordinates internally, but currently only accepts the seven one-sided tetrominos (polyominos of size four) from the command-line.


I originally wrote this because I was too lazy to actually solve the sigil puzzles in the (otherwise incredible) [Talos Principle](http://www.croteam.com/talosprinciple/), but not lazy enough to just look up a guide.
I've included a Bash script, `talos.sh`, that will run `polytile` on all 35 sigil puzzles from game.

## Usage

```
./polytile [-c] [-u] <width> <height> <polyominoes...>
```

For best results, if your terminal will allow it, set the `-c` option to output ANSI colour codes, allowing the output to be condensed into a single drawing, and set the `-u` option to output Unicode characters.

```
> ./polytile -u 6 6 L L L L O S S S S
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

## Polyominoes

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
