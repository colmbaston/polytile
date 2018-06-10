import           Data.Char
import           Data.List
import           Data.Tuple
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Bifunctor
import           Data.Function
import           System.Environment
import           System.Exit

{-
    Polyominos, grids, and coordinate-based utility functions.
-}

type Poly = Set (Int, Int)
type Grid = Set (Int, Int)

polyominoes :: Map String Poly
polyominoes = M.fromList [("I", S.fromList [(0, 0),(0, 1),(0,2),(0,3)]),
                          ("J", S.fromList [(1,-2),(1,-1),(0,0),(1,0)]),
                          ("L", S.fromList [(0, 0),(0, 1),(0,2),(1,2)]),
                          ("O", S.fromList [(0, 0),(1, 0),(0,1),(1,1)]),
                          ("S", S.fromList [(1,-1),(2,-1),(0,0),(1,0)]),
                          ("T", S.fromList [(0, 0),(1, 0),(2,0),(1,1)]),
                          ("Z", S.fromList [(0, 0),(1, 0),(1,1),(2,1)])]

offset :: (Int, Int) -> Poly -> Poly
offset (x,y) = S.mapMonotonic (bimap (+x) (+y))

normalise :: Poly -> Poly
normalise p = offset (bimap negate negate (minimum p)) p

rotations :: Poly -> [Poly]
rotations = map head . group . sort . map normalise . take 4 . iterate (S.map (swap . fmap negate))

adjacent :: Poly -> Poly -> Bool
adjacent p = not . S.disjoint (S.foldl (\s -> S.union s . S.fromAscList . ortho) S.empty p S.\\ p)

ortho :: (Int, Int) -> [(Int, Int)]
ortho (x,y) = [(x-1,y),(x,y-1),(x,y+1),(x+1,y)]

place :: Poly -> Grid -> Maybe Grid
place p g | S.size g - S.size p == S.size g' = Just g'
          | otherwise                        = Nothing
          where
            g' = g S.\\ p

{-
    Core algorithm to find grid tilings.
-}

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

tilings :: Int -> Int -> [Poly] -> [[Poly]]
tilings w h = concatMap (go grid) . uniquePerms
  where
    grid :: Grid
    grid = S.fromAscList [(x,y) | x <- [0..w-1], y <- [0..h-1]]

    go :: Grid -> [Poly] -> [[Poly]]
    go _ []     = [[]]
    go g (p:ps) = do r <- offset (minimum g) <$> rotations p
                     case place r g of
                       Nothing -> []
                       Just g' -> (r:) <$> go g' ps

{-
    Command-line parsing, data structure setup and core IO.
-}

main :: IO ()
main = do (c, u, w, h, ps) <- getArgs >>= parseArgs
          let pa = sum (map S.size ps)
          let ga = w * h
          case compare pa ga of
            EQ  -> do putStrLn "Searching for tilings..."
                      case tilings w h ps of
                        []    ->    putStrLn "Exhaustive search completed. No tilings found!"
                        (t:_) -> do putStrLn ""
                                    (if c then drawTilingColour else drawTiling) (if u then ('\x25a0','\x25a1') else ('o','-')) w h t
                                    putStrLn ""
            cmp -> do putStrLn ("Too " ++ (if cmp == GT then "many" else "few") ++ " polyominoes to tile!")
                      putStrLn ("  Grid area:      " ++ show ga)
                      putStrLn ("  Polyomino area: " ++ show pa)
                      exitFailure

parseArgs :: [String] -> IO (Bool, Bool, Int, Int, [Poly])
parseArgs ("-c":as) = (\(_, u, w, h, ps) -> (True, u,    w, h, ps)) <$> parseArgs as
parseArgs ("-u":as) = (\(c, _, w, h, ps) -> (c,    True, w, h, ps)) <$> parseArgs as
parseArgs ( w:h:as) = if all isDigit (w ++ h) && not (null as)
                        then case mapM (`M.lookup` polyominoes) as of
                               Nothing -> usageFail
                               Just ps -> pure (False, False, read w, read h, ps)
                        else usageFail
parseArgs _         = usageFail

usageFail :: IO a
usageFail = do putStr "Usage: "
               getProgName >>= putStr
               putStrLn  " [-c] [-u] <width> <height> <polyominoes...>"
               putStrLn  "  Set -c to output ANSI colour codes."
               putStrLn  "  Set -u to output Unicode characters."
               putStrLn ("  Available polyominoes: " ++ unwords (M.keys polyominoes))
               exitFailure

{-
    Draw tiled grids to the terminal, making sure to handle colour and Unicode options.
-}

drawTiling :: (Char, Char) -> Int -> Int -> [Poly] -> IO ()
drawTiling (a,b) w h = sequence_ . intersperse (putStrLn "") . map drawPoly
  where
    drawPoly :: Poly -> IO ()
    drawPoly p = putStr (unlines [((' ':) . unwords) [if (x,y) `elem` p then [a] else [b] | x <- [0..w-1]] | y <- [0..h-1]])

drawTilingColour :: (Char, Char) -> Int -> Int -> [Poly] -> IO ()
drawTilingColour (a,b) w h t = putStr (unlines [((' ':) . unwords) [colourChar (M.lookup (x,y) colourMap) | x <- [0..w-1]] | y <- [0..h-1]])
  where
    colourMap :: Map (Int, Int) Int
    colourMap = (M.unions . zipWith (\n -> M.fromSet (const n) . S.unions) [1..]) (colouring t)

    colourChar :: Maybe Int -> String
    colourChar Nothing  = "\ESC[1;37m" ++ b : "\ESC[0m"
    colourChar (Just 1) = "\ESC[1;31m" ++ a : "\ESC[0m"
    colourChar (Just 2) = "\ESC[1;32m" ++ a : "\ESC[0m"
    colourChar (Just 3) = "\ESC[1;36m" ++ a : "\ESC[0m"
    colourChar (Just 4) = "\ESC[1;33m" ++ a : "\ESC[0m"
    colourChar (Just 5) = "\ESC[1;35m" ++ a : "\ESC[0m"
    colourChar _        = error "exhausted colours!"

{-
    Find a colouring such that no two adjacent polyominoes share the same colour.
-}

colouring :: [Poly] -> [[Poly]]
colouring = map (map snd) . groupBy ((==) `on` fst) . sort . foldl step []
  where
    step :: [(Int,Poly)] -> Poly -> [(Int,Poly)]
    step xs p = ((\n -> (n,p) : xs) . head . flip dropWhile [1..] . flip elem . map fst) (filter (adjacent p . snd) xs)
