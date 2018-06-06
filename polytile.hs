import           Data.Char
import           Data.List
import           Data.Tuple
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Bifunctor
import           System.Environment
import           System.Exit

type Poly = Set (Int, Int)
type Grid = Set (Int, Int)

offset :: (Int, Int) -> Poly -> Poly
offset (x,y) = S.mapMonotonic (bimap (+x) (+y))

normalise :: Poly -> Poly
normalise p = offset (bimap negate negate (minimum p)) p

polyominoes :: Map String Poly
polyominoes = M.fromAscList [("I", S.fromList [(0, 0),(0, 1),(0,2),(0,3)]),
                             ("J", S.fromList [(1,-2),(1,-1),(0,0),(1,0)]),
                             ("L", S.fromList [(0, 0),(0, 1),(0,2),(1,2)]),
                             ("O", S.fromList [(0, 0),(1, 0),(0,1),(1,1)]),
                             ("S", S.fromList [(1,-1),(2,-1),(0,0),(1,0)]),
                             ("T", S.fromList [(0, 0),(1, 0),(2,0),(1,1)]),
                             ("Z", S.fromList [(0, 0),(1, 0),(1,1),(2,1)])]

grid :: Int -> Int -> Grid
grid w h = S.fromList [(x,y) | x <- [0..w-1], y <- [0..h-1]]

rotations :: Poly -> [Poly]
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

tile :: Int -> Int -> [Poly] -> [[Poly]]
tile x y ps = uniquePerms ps >>= go (grid x y)
  where
    go :: Grid -> [Poly] -> [[Poly]]
    go _ []     = [[]]
    go g (p:ps) | sg < sp   = []
                | otherwise = do r <- offset (minimum g) <$> rotations p
                                 let g' = g S.\\ r
                                 if S.size g' + sp /= sg
                                   then []
                                   else (r:) <$> go g' ps
                where
                  sg = S.size g
                  sp = S.size p

usageFail :: IO a
usageFail = do p <- getProgName
               putStrLn ("Usage: " ++ p ++ " width height [space-separated, non-empty list of polyominoes]")
               putStrLn (" - Available polyominoes: " ++ unwords (M.keys polyominoes))
               exitFailure

parseArgs :: [String] -> IO (Int, Int, [Poly])
parseArgs (w:h:as) | all isDigit (w ++ h) && not (null as) = case mapM (`M.lookup` polyominoes) as of
                                                               Nothing -> usageFail
                                                               Just ps -> pure (read w, read h, ps)
parseArgs _        = usageFail

main :: IO ()
main = do (w, h, ps) <- getArgs >>= parseArgs
          if w * h < sum (map S.size ps)
            then putStrLn "Cannot tile: too many polyominoes!"
            else do putStrLn "Searching for tiling..."
                    case tile w h ps of
                      []    ->    putStrLn "No tilings found!"
                      (t:_) -> do putStrLn ""
                                  sequence_ (intersperse (putStrLn "") (map (drawPoly w h) t))
                                  putStrLn ""

drawPoly :: Int -> Int -> Poly -> IO ()
drawPoly w h p = mapM_ (putStrLn . (' ':)) [intersperse ' ' [if (x,y) `elem` p then 'o' else '-' | x <- [0..w-1]] | y <- [0..h-1]]
