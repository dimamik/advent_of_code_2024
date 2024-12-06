{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import qualified Data.Set as Set

simulate :: [[Char]] -> Int -> Int -> Int -> Bool
simulate grid startRow startCol startDir =
    let rows = length grid
        cols = length (head grid)
        directions = [(-1,0),(0,1),(1,0),(0,-1)]

        move visited (r,c,d) =
            let (dr,dc) = directions !! d
                nr = r + dr
                nc = c + dc
            in if nr < 0 || nr >= rows || nc < 0 || nc >= cols
               then True
               else case (grid !! nr) !! nc of
                      '#' -> turnRight visited (r,c,d)
                      _   -> stepForward visited (nr,nc,d)

        turnRight visited (r,c,d) =
            let d' = (d + 1) `mod` 4
                next = (r,c,d')
            in if next `Set.member` visited then False else move (Set.insert next visited) next

        stepForward visited next =
            if next `Set.member` visited then False else move (Set.insert next visited) next

    in move (Set.singleton (startRow,startCol,startDir)) (startRow,startCol,startDir)

main :: IO ()
main = do
    grid <- lines <$> readFile "input.txt"
    let rows = length grid
        cols = length (head grid)
        dirMap = zip "^>v<" [0..]

        findStart g =
            let tryRows r
                  | r >= rows = Nothing
                  | otherwise =
                      case lookupDirection (g !! r) of
                        Just (c,d) -> Just (r,c,d)
                        Nothing    -> tryRows (r+1)
                lookupDirection row =
                    let indexed = zip [0..] row
                        found   = [(c,d) | (c,ch) <- indexed, Just d <- [lookup ch dirMap]]
                    in case found of
                         (x:_) -> Just x
                         []    -> Nothing
            in tryRows 0

        Just (startRow,startCol,startDir) = findStart grid
        baseGrid = modifyGrid grid startRow startCol '.'
        candidates =
            [ (r,c)
            | r <- [0..rows-1], c <- [0..cols-1]
            , (r,c) /= (startRow,startCol)
            , (baseGrid !! r) !! c == '.' ]

        count =
            length
            [ ()
            | (r,c) <- candidates
            , let blocked = modifyGrid baseGrid r c '#'
            , not (simulate blocked startRow startCol startDir)
            ]

    print count

modifyGrid :: [[Char]] -> Int -> Int -> Char -> [[Char]]
modifyGrid g r c ch =
    let (above, row:below) = splitAt r g
        (left, _:right)    = splitAt c row
    in above ++ [left ++ ch:right] ++ below
