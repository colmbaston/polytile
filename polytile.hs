import           Data.List
import           Data.Tuple
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Bifunctor
import           Control.Arrow

import Data.Time.Clock

type XY    = (Int, Int)
type Piece = Set XY
type Grid  = Set XY

offset :: XY -> Piece -> Piece
offset (x,y) = S.mapMonotonic (bimap (+x) (+y))

normalise :: Piece -> Piece
normalise p = offset (bimap negate negate (minimum p)) p

i :: Piece
i = S.fromList [(0,0),(0,1),(0,2),(0,3)]

j :: Piece
j = S.fromList [(1,-2),(1,-1),(0,0),(1,0)]

l :: Piece
l = S.fromList [(0,0),(0,1),(0,2),(1,2)]

o :: Piece
o = S.fromList [(0,0),(1,0),(0,1),(1,1)]

s :: Piece
s = S.fromList [(1,-1),(2,-1),(0,0),(1,0)]

t :: Piece
t = S.fromList [(0,0),(1,0),(2,0),(1,1)]

z :: Piece
z = S.fromList [(0,0),(1,0),(1,1),(2,1)]

grid :: XY -> Grid
grid (x,y) = S.fromList [(x',y') | x' <- [0..x-1], y' <- [0..y-1]]

rotations :: Piece -> [Piece]
rotations = map head . group . sort . map normalise . take 4 . iterate (S.map (swap . fmap negate))

uniquePerms :: Ord a => [a] -> [[a]]
uniquePerms = go . group . sort
  where
    go :: [[a]] -> [[a]]
    go []  = [[]]
    go xss = do (y:ys,yss) <- selects xss
                (y:) <$> go (if null ys then yss else ys:yss)

    selects :: [a] -> [(a,[a])]
    selects []     = []
    selects (x:xs) = (x,xs) : map (fmap (x:)) (selects xs)
