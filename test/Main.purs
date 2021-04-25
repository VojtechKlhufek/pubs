module Test.Main where

{-
import Prelude
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (unsafeFromString)
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Effect (Effect)
import Main (Pubs(..), solve)
import Partial.Unsafe (unsafePartial)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

data Pubs
  = NonEmptyArray Char

main :: Effect Unit
main =
  runTest do
    suite "diagonal" do
      let
        pubs = Pubs $ nonempty "ABCDEFGHIJKLMNO"
      test "AHOJ" do
        let
          beers = nonempty "AHOJ"
        Assert.equal (Just 19) -- solve pubs beers
      test "FENA" do
        let
          beers = nonempty "FENA"
        Assert.equal (Just 26) -- solve pubs beers
  where
  nonempty = toNonEmptyCharArray <<< (unsafePartial $ unsafeFromString)
 -}
