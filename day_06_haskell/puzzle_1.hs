import System.IO (readFile)
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = U | R | D | L deriving (Eq, Show)

turnRight :: Direction -> Direction
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

move :: Direction -> (Int, Int) -> (Int, Int)
move U (x,y) = (x, y-1)
move D (x,y) = (x, y+1)
move L (x,y) = (x-1, y)
move R (x,y) = (x+1, y)

inBounds :: (Int, Int) -> Int -> Int -> Bool
inBounds (x,y) w h = x >= 0 && x < w && y >= 0 && y < h

isBlocked :: [[Char]] -> (Int, Int) -> Bool
isBlocked grid (x,y) = (grid !! y) !! x == '#'

findGuard :: [[Char]] -> ((Int, Int), Direction)
findGuard [] = error "Empty grid"
findGuard grid@(firstRow:_) =
    let h = length grid
        w = length firstRow
        positions = [(x,y) | y <- [0..h-1], x <- [0..w-1]]
        isGuard c = c `elem` "^v<>"
        possibleGuards = [p | p@(x,y) <- positions, isGuard (grid !! y !! x)]
        guardPos = case possibleGuards of
                     []     -> error "No guard found"
                     (gp:_) -> gp
        dirChar = (grid !! snd guardPos) !! fst guardPos
        dir = case dirChar of
                '^' -> U
                'v' -> D
                '<' -> L
                '>' -> R
                _   -> error "Invalid guard direction"
    in (guardPos, dir)

simulate :: [[Char]] -> Int
simulate [] = 0
simulate grid@(firstRow:_) = explore initialPos initialDir (Set.singleton initialPos)
  where
    (initialPos, initialDir) = findGuard grid
    w = length firstRow
    h = length grid

    explore :: (Int, Int) -> Direction -> Set (Int, Int) -> Int
    explore pos dir visited =
      let frontPos = move dir pos
      in if not (inBounds frontPos w h)
           then Set.size visited
           else if isBlocked grid frontPos
                  then explore pos (turnRight dir) visited
                  else explore frontPos dir (Set.insert frontPos visited)

main :: IO ()
main = do
    grid <- lines <$> readFile "input.txt"
    print (simulate grid)
