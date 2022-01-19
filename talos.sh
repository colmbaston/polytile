#!/bin/bash

# the location of the polytile executable
EXE=$1

echo "Demo:"
./$EXE -c -u 4 5 L S T T Z
./$EXE -c -u 7 4 I I L S T T Z

echo "Hub Gates (Green):"
./$EXE -c -u 4 3 J J Z
./$EXE -c -u 4 4 I J L Z
./$EXE -c -u 4 5 I L T T Z
./$EXE -c -u 4 6 J J L T T Z

echo "Tools (Yellow):"
./$EXE -c -u 4 3 L T T
./$EXE -c -u 4 4 L T T Z
./$EXE -c -u 4 5 L S T T Z
./$EXE -c -u 4 5 J S T T Z
./$EXE -c -u 4 6 I L O T T Z

echo "Tower Gates (Red):"
./$EXE -c -u 4 4 L L Z Z
./$EXE -c -u 6 6 L L L L O T T T T
./$EXE -c -u 8 5 I I I I J J L L S Z
./$EXE -c -u 6 8 J L O O S S T T T T Z Z
./$EXE -c -u 8 7 I I J L O O O O S T T T T Z

echo "Messenger A (Blue):"
./$EXE -c -u 4 5  I J J O O
./$EXE -c -u 6 6  I J L L O O S T T
./$EXE -c -u 6 6  J J L L T T Z Z Z
./$EXE -c -u 5 8  I I L L O S T T Z Z
./$EXE -c -u 4 10 I L L O O S T T T T

echo "Messenger B (Blue):"
./$EXE -c -u 4 5  I L T T Z
./$EXE -c -u 7 4  I I L S T T Z
./$EXE -c -u 4 10 I I J S T T T T Z Z
./$EXE -c -u 8 6  I J J J L L O O S S T T
./$EXE -c -u 8 6  I J J J O O S T T Z Z Z

echo "Messenger C (Blue):"
./$EXE -c -u 4 5 J L L T T
./$EXE -c -u 4 5 L S T T Z
./$EXE -c -u 7 4 I J L T T Z Z
./$EXE -c -u 6 6 I J J L L O O Z Z
./$EXE -c -u 8 6 I J J J L L O O T T Z Z

echo "Bonus Gates (Stars):"
./$EXE -c -u 8 5 J L S S T T T T Z Z
./$EXE -c -u 8 5 I I J L L O T T T T
./$EXE -c -u 8 5 I I J L O O S T T Z

echo "Messenger Ending (Grey):"
./$EXE -c -u 6 6 L L L L O S S S S

echo "Road to Gehenna:"
./$EXE -c -u 7 4 I J J L L T T
./$EXE -c -u 5 8 I I L S S S T T T T
