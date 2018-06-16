#!/bin/bash

echo "Hub Gates (Green):"
./polytile -c -u 4 3 J J Z
./polytile -c -u 4 4 I J L Z
./polytile -c -u 4 5 I L T T Z
./polytile -c -u 4 6 J J L T T Z

echo "Tools (Yellow):"
./polytile -c -u 4 3 L T T
./polytile -c -u 4 4 L T T Z
./polytile -c -u 4 5 L S T T Z
./polytile -c -u 4 5 J S T T Z
./polytile -c -u 4 6 I L O T T Z

echo "Tower Gates (Red):"
./polytile -c -u 4 4 L L Z Z
./polytile -c -u 6 6 L L L L O T T T T
./polytile -c -u 8 5 I I I I J J L L S Z
./polytile -c -u 6 8 J L O O S S T T T T Z Z
./polytile -c -u 8 7 I I J L O O O O S T T T T Z

echo "Messenger A (Blue):"
./polytile -c -u 4 5  I J J O O
./polytile -c -u 6 6  I J L L O O S T T
./polytile -c -u 6 6  J J L L T T Z Z Z
./polytile -c -u 5 8  I I L L O S T T Z Z
./polytile -c -u 4 10 I L L O O S T T T T

echo "Messenger B (Blue):"
./polytile -c -u 4 5  I L T T Z
./polytile -c -u 7 4  I I L S T T Z
./polytile -c -u 4 10 I I J S T T T T Z Z
./polytile -c -u 8 6  I J J J L L O O S S T T
./polytile -c -u 8 6  I J J J O O S T T Z Z Z

echo "Messenger C (Blue):"
./polytile -c -u 4 5 J L L T T
./polytile -c -u 4 5 L S T T Z
./polytile -c -u 7 4 I J L T T Z Z
./polytile -c -u 6 6 I J J L L O O Z Z
./polytile -c -u 8 6  I J J J L L O O T T Z Z

echo "Bonus Gates (Stars):"
./polytile -c -u 8 5  J L S S T T T T Z Z
./polytile -c -u 8 5  I I J L L O T T T T
./polytile -c -u 8 5  I I J L O O S T T Z

echo "Messenger Ending (Grey):"
./polytile -c -u 6 6  L L L L O S S S S

echo "Road to Gehenna:"
./polytile -c -u 7 4  I J J L L T T
./polytile -c -u 5 8  I I L S S S T T T T
