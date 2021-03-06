module Main where

import Prelude
import Data.Array (foldl)
import Data.Array.NonEmpty (NonEmptyArray, index)
import Data.Foldable (minimum)
import Data.List (List(..), fromFoldable, null)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.String.NonEmpty (fromString)
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  input <- split (Pattern "\n") <$> readTextFile UTF8 "input.txt"
  log <<< show $ solveInput input
  where
  solveInput :: Array String -> Maybe Int
  solveInput [ pubs, beers ] = do
    nEPubs <- fromString pubs
    let
      p = toNonEmptyCharArray nEPubs

      b = convertStringToListOfChars beers
    -- máme dva stringy, t a w, které odpovídají dvěma řádkům v souboru input.txt
    -- ?callSolveHere
    Nothing

  solveInput _ = Nothing

data Pubs
  = NonEmptyArray Char

data Beers
  = List Char

{- 
solve vrátí
- Nothing pokud nemůžeme daná piva vypít
- (Just l) pokud je můžeme vypít na nějaké cestě délky l
 -}
solveInterface :: NonEmptyArray Char -> List Char -> Maybe (List Int)
solveInterface p b =
  let
    solution = solve 0 0 p b
  in
    if null solution then
      Nothing
    else
      Just solution

solve :: Int -> Int -> NonEmptyArray Char -> List Char -> List Int
solve sum _ _ Nil = Cons sum Nil

solve sum currentPos pubs (Cons first rest) =
  let
    nextPos = indexor30000 pubs first 0 Nil
  in
    if null nextPos then
      Nil
    else
      foldl go Nil nextPos
  where
  go acc pos =
    let
      distance = abs (pos - currentPos)
    in
      solve (sum + distance) pos pubs rest <> acc

indexor30000 :: NonEmptyArray Char -> Char -> Int -> List Int -> List Int
indexor30000 pubs pub i acc =
  let
    b = index pubs i
  in
    case b of
      Nothing -> acc
      Just pubOnI ->
        if pub == pubOnI then
          indexor30000 pubs pub (i + 1) (Cons i acc)
        else
          indexor30000 pubs pub (i + 1) (acc)

convertStringToListOfChars :: String -> List Char
convertStringToListOfChars str = fromFoldable $ toCharArray str --credits to Omar Mefire

-- #########################################################################
-- Call for all distances
getAllDistances :: Maybe (List Int)
getAllDistances = do
  nEPubs <- fromString "ABCB"
  let
    pubs = toNonEmptyCharArray nEPubs
  let
    beers = convertStringToListOfChars "ABC"
  solveInterface pubs beers

-- Call for the shortest distance
getShortestDistance :: Maybe Int
getShortestDistance = case getAllDistances of
  Nothing -> Nothing
  Just a -> minimum a
