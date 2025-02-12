#!/bin/bash

# the location of the polytile executable
exe=$1

echo "Demo:"
./$exe -c -u 4 5 L S T T Z
./$exe -c -u 7 4 I I L S T T Z

echo "Hub Gates (Green):"
./$exe -c -u 4 3 J J Z
./$exe -c -u 4 4 I J L Z
./$exe -c -u 4 5 I L T T Z
./$exe -c -u 4 6 J J L T T Z

echo "Tools (Yellow):"
./$exe -c -u 4 3 L T T
./$exe -c -u 4 4 L T T Z
./$exe -c -u 4 5 L S T T Z
./$exe -c -u 4 5 J S T T Z
./$exe -c -u 4 6 I L O T T Z

echo "Tower Gates (Red):"
./$exe -c -u 4 4 L L Z Z
./$exe -c -u 6 6 L L L L O T T T T
./$exe -c -u 8 5 I I I I J J L L S Z
./$exe -c -u 6 8 J L O O S S T T T T Z Z
./$exe -c -u 8 7 I I J L O O O O S T T T T Z

echo "Messenger A (Blue):"
./$exe -c -u 4 5  I J J O O
./$exe -c -u 6 6  I J L L O O S T T
./$exe -c -u 6 6  J J L L T T Z Z Z
./$exe -c -u 5 8  I I L L O S T T Z Z
./$exe -c -u 4 10 I L L O O S T T T T

echo "Messenger B (Blue):"
./$exe -c -u 4 5  I L T T Z
./$exe -c -u 7 4  I I L S T T Z
./$exe -c -u 4 10 I I J S T T T T Z Z
./$exe -c -u 8 6  I J J J L L O O S S T T
./$exe -c -u 8 6  I J J J O O S T T Z Z Z

echo "Messenger C (Blue):"
./$exe -c -u 4 5 J L L T T
./$exe -c -u 4 5 L S T T Z
./$exe -c -u 7 4 I J L T T Z Z
./$exe -c -u 6 6 I J J L L O O Z Z
./$exe -c -u 8 6 I J J J L L O O T T Z Z

echo "Bonus Gates (Stars):"
./$exe -c -u 8 5 J L S S T T T T Z Z
./$exe -c -u 8 5 I I J L L O T T T T
./$exe -c -u 8 5 I I J L O O S T T Z

echo "Messenger Ending (Grey):"
./$exe -c -u 6 6 L L L L O S S S S

echo "Road to Gehenna:"
./$exe -c -u 7 4 I J J L L T T
./$exe -c -u 5 8 I I L S S S T T T T
