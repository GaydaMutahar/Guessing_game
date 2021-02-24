-- Author: Gayda Mutahar 
-- Purpose: Project 1 submission (Logical guessing game)
-- Declerative Programming COMP90048
-- The University of Melbourne 

{- 
This program is a simple two-player logical guessing game.
The game is played on a 4×8 grid, and involves one player,
the searcher trying to find the locations of three battleships hidden by 
the other player, the hider. The searcher continues to guess until 
they find all the hidden ships. A guess consists of three different locations,
and the game continues until the exact locations of the three hidden ships are
guessed in a single guess. After each guess, the hider responds with 
three numbers, which represent the game feedback.

-}
module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char
import Data.List

{-  location section  -}


{-
  type definition for our location representation
  it's based on pair of ints, but defined as a new
  data type in order to be able to introduce own
  instance for Show type class
-}
data Location = Loc (Int, Int) deriving (Eq, Ord)

{-
  Show type class instance definition for our Location
  type which turns it into a string in chess-like notation
-}
instance Show Location where
  show (Loc (col, row)) =
    let
      colCode = ord 'A' - 1 + col
      colChar = chr colCode
    in
      colChar : show row

{-
  a function that consumes a string and returns
  just valid location, if the string described correct location
  in a chess-like notation, or Nothing otherwise
-}
toLocation :: String -> Maybe Location
toLocation [colChar, rowChar] =
  let
    colIndex = ord colChar - ord 'A' + 1
    rowIndex = ord rowChar - ord '0'
  in
    if checkBounds colIndex rowIndex
      then Just (Loc (colIndex, rowIndex))
      else Nothing
toLocation _ = Nothing

{-
  a helper function that checks if two indices
  are within bounds of our 8x4 game board
-}
checkBounds :: Int -> Int -> Bool
checkBounds colIndex rowIndex =
  colIndex >= 1 && rowIndex >= 1 &&
  colIndex <= 8 && rowIndex <= 4

{-
  a helper function that computes distance on
  our game board, according to specification
-}
distance :: Location -> Location -> Int
distance (Loc (col1, row1)) (Loc (col2, row2)) =
  let
    colDist = abs (col1 - col2)
    rowDist = abs (row1 - row2)
  in
    max colDist rowDist


{-  feedback section  -}

--After each guess, the hider responds with three numbers as a feedback:
-- 1s number represents the number of correct guesses
-- 2nd second number is the number of guesses that were exactly one space away from a ship
-- 3rd shows the number of guesses that were exactly two spaces away from a ship.
{-
  feedback function
  It is working by using nSpacesAway helper function and invoke it for parameters 0, 1, 2,
  obtaining the feedback for given targets and guess
-}
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback targets guess = (nSpacesAway 0, nSpacesAway 1, nSpacesAway 2)
  where
    {-
      a helper function that counts number of guesses for
      which the closest target is exactly n spaces away
    -}
    nSpacesAway :: Int -> Int
    nSpacesAway n =
      count (\gloc -> closestDist gloc == n) guess

-- closestDist: a helper function that computes distance to the closest target, 
-- for given location
   
    closestDist :: Location -> Int
    closestDist gloc =
      minimum (map (distance gloc) targets)

{-count
  a helper function that computes number of list elements
  that satisfy given predicate
-}
count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count predicate (loc:locs) =
  if predicate loc
    then 1 + countTail
    else countTail
  where
    countTail = count predicate locs


{- game heuristics section -}

{-
  type definition for internal game state
  here we store list of unvisited locations, that will
  be tried in the next game steps
-}
type GameState = [[Location]]

{-
  provides initial guess and game state for our game heuristic
  we take all possible guesses and return first of them
  as a first guess and rest of them as list of untried guesses
-}
initialGuess :: ([Location], GameState)
initialGuess = (head allGuessess, tail allGuessess)

{-
  a helper function that generates all of the possible
  locations on 8x4 board
-}
allLocations :: [Location]
allLocations = [Loc (x, y) | x <- [1..8], y <- [1..4]]

{-
  a helper function that generates all of the possible
  (4960) unique guesses

  in order to have unique guesses, we
  - make sure we generate only unique triples
  - we sort triples
  - we eliminate duplicated triples using haskell nub function
-}
allGuessess :: [[Location]]
allGuessess =
  nub [ sort [loc1, loc2, loc3] |
    loc1 <- allLocations,
    loc2 <- allLocations,
    loc1 /= loc2,
    loc3 <- allLocations,
    loc2 /= loc3,
    loc1 /= loc3
  ]
{-
nextGuess

  a function that computes next guess and game state based
  on the feedback of previous guess and game state. 
  It adds to the game state only the guesss locations that have the
  same feedback scores with the previous guess
-}

nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess (prevGuess, state) feedbackScore = 
     let
        validGuess = [guessesToTry | guessesToTry <- state, 
        feedback guessesToTry prevGuess == feedbackScore]
        newState =  validGuess \\ [prevGuess] 
        bestNextGuess' = bestGuess newState
      in 
       (bestNextGuess', newState)

    
{-
bestGuess

This function is responsilbe for generating a best guess by given the game state 
which should have the remainig possible candidates of the targets. 
The best guess is meant to be the guess that has by average the lowest number of
remaining candidates locations. Reducing the number of remaining possible targets 
by the greatest amount.
-}
bestGuess :: GameState -> [Location]
bestGuess gamestate = 
    let
      bestguess = fst (head srtremGuess)
      srtremGuess = sortOn snd remPossibleGuess 
      remPossibleGuess = [(targLoc, remGuss)| targLoc <- gamestate, let newState = delete targLoc gamestate , let remGuss = avgRemGuesses targLoc newState]
    in 
     bestguess
        

{- 
avgRemGuesses

A helper function that generates the average number of remaining target candidates.
It is given a possible target location and the game state to generate a sorted list of possible feedback scores for the given target location, groups these scores, then count the number of possible targets that have been tested. 
The lower average number of remaining candidates indicates how much the next guess can be good. As this helps to eliminate the game state better.
The average is calculated by summing the probability of feedback score of a target multibly by the total number of remaining scores. 
-}  
avgRemGuesses :: [Location] -> GameState -> Float
avgRemGuesses target gameState = 
    let
        feedbackScores  = sort [fscores | guess <- gameState , let fscores = feedback target guess]
        totalRemCan = (realToFrac.length) feedbackScores
        remCan =  (realToFrac.length) groupScores 
        groupScores = (group.nub) feedbackScores
    in
      sum [(remCan / totalRemCan) * totalRemCan]

