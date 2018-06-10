import           Data.Char
import           Data.List
import           Data.Tree
import           Data.Maybe
import           Data.Tuple
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Bifunctor
import           Data.Function
import           System.Environment
import           System.Exit
import           Control.Monad
import           Control.Concurrent

{-
    Polyominos, grids, and coordinate-based utility functions.
-}

type Poly = Set (Int, Int)
type Grid = Set (Int, Int)

polyominoes :: Map String Poly
polyominoes = M.fromAscList [("I", S.fromList [(0, 0),(0, 1),(0,2),(0,3)]),
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

uniquePermTree :: Int -> Int -> [Poly] -> Tree [Poly]
uniquePermTree w h = go grid [] . group . sort
  where
    grid :: Grid
    grid = S.fromAscList [(x,y) | x <- [0..w-1], y <- [0..h-1]]

    go :: Grid -> [Poly] -> [[Poly]] -> Tree [Poly]
    go g xs pss = Node xs (do (y:ys,yss) <- selects pss
                              r <- offset (minimum g) <$> rotations y
                              case place r g of
                                Nothing -> []
                                Just g' -> [go g' (xs ++ [r]) (if null ys then yss else ys:yss)])

selects :: [a] -> [(a,[a])]
selects []     = []
selects (x:xs) = (x,xs) : map (fmap (x:)) (selects xs)

tiling :: Int -> Int -> [Poly] -> Maybe [Poly]
tiling w h = go . uniquePermTree w h
  where
    go :: Tree [Poly] -> Maybe [Poly]
    go (Node ps []) | sum (map S.size ps) == w * h = Just ps
                    | otherwise                    = Nothing
    go (Node _  ts) = msum (map go ts)

{-
    Command-line parsing, data structure setup and core IO.
-}

main :: IO ()
main = do (c, u, t, w, h, ps) <- getArgs >>= parseArgs
          let draw x = putStrLn "" >> (if c then drawTilingColour else drawTiling)
                                        (if u then ('\x25a0','\x25a1') else ('o','-'))
                                          w h x >> putStrLn ""
          putStrLn "Searching for tilings..."
          let pa = sum (map S.size ps)
          let ga = w * h
          case compare pa ga of
            EQ  -> do (if t >= 0
                         then animate draw (t * 1000) w h ps
                         else pure (tiling w h ps)) >>= maybe (putStrLn "Exhaustive search completed. No tilings found!") draw
                      when (t >= 0) (putStr "\ESC[J")
            cmp -> do putStrLn ("Too " ++ (if cmp == GT then "many" else "few") ++ " polyominoes to tile!")
                      putStrLn ("  Grid area:      " ++ show ga)
                      putStrLn ("  Polyomino area: " ++ show pa)
                      exitFailure

animate :: ([Poly] -> IO ()) -> Int -> Int -> Int -> [Poly] -> IO (Maybe [Poly])
animate draw t w h = go . uniquePermTree w h
  where
    go :: Tree [Poly] -> IO (Maybe [Poly])
    go (Node ps ts) = do draw ps
                         threadDelay t
                         putStr ("\ESC[" ++ show (h+2) ++ "A")
                         case ts of
                           []     -> pure (if sum (map S.size ps) == w * h
                                             then Just ps
                                             else Nothing)
                           (x:xs) -> do y <- go x
                                        case y of
                                          Nothing -> go (Node ps xs)
                                          z       -> pure z

parseArgs :: [String] -> IO (Bool, Bool, Int, Int, Int, [Poly])
parseArgs ("-a":t:as) | all isDigit t = (\(_, u, _, w, h, ps) -> (True, u, read t, w, h, ps)) <$> parseArgs as
parseArgs ("-c":as)   =                 (\(_, u, t, w, h, ps) -> (True, u,      t, w, h, ps)) <$> parseArgs as
parseArgs ("-u":as)   =                 (\(c, _, t, w, h, ps) -> (c,    True,   t, w, h, ps)) <$> parseArgs as
parseArgs (w:h:as)    = if all isDigit (w ++ h) && not (null as)
                           then case mapM (`M.lookup` polyominoes) as of
                                  Nothing -> usageFail
                                  Just ps -> pure (False, False, -1, read w, read h, ps)
                           else usageFail
parseArgs _           = usageFail

usageFail :: IO a
usageFail = do putStr "Usage: "
               getProgName >>= putStr
               putStrLn  " [-a MILLISECONDS] [-c] [-u] <width> <height> <polyominoes...>"
               putStrLn  "  Set -a to animate, specifying the number of milliseconds between frames."
               putStrLn  "  Set -c to output coloured characters. This is implied by -a."
               putStrLn  "  Set -u to output Unicode characters."
               putStrLn  "  Options -a and -c require an ANSI-compliant terminal."
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
drawTilingColour (a,b) w h t = putChar ' ' >> write (-1) [[fromMaybe 0 (M.lookup (x,y) colourMap) | x <- [0..w-1]] | y <- [0..h-1]]
  where
    colourMap :: Map (Int, Int) Int
    colourMap = (M.unions . zipWith (\n -> M.fromSet (const n) . S.unions) [1..]) colouring

    colouring :: [[Poly]]
    colouring = (map (map snd) . groupBy ((==) `on` fst) . sort)  (foldl step [] t)
      where
        step :: [(Int,Poly)] -> Poly -> [(Int,Poly)]
        step xs p = ((\n -> (n,p) : xs) . head . flip dropWhile [1..] . flip elem . map fst) (filter (adjacent p . snd) xs)

    write :: Int -> [[Int]] -> IO ()
    write _  []          =    putStr "\ESC[m"
    write m ([]:xss)     = do putStrLn " "
                              unless (null xss) (putChar ' ')
                              write m xss
    write m ((x:xs):xss) = do when (x /= m) (putStr (colourCode x))
                              putChar (if x == 0 then b else a)
                              unless (null xs) (putChar ' ')
                              write x (xs:xss)

    colourCode :: Int -> String
    colourCode 0 = "\ESC[1;37m"
    colourCode 1 = "\ESC[1;31m"
    colourCode 2 = "\ESC[1;32m"
    colourCode 3 = "\ESC[1;36m"
    colourCode 4 = "\ESC[1;33m"
    colourCode 5 = "\ESC[1;35m"
    colourCode _ = error "exhausted colours!"
