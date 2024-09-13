{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module QuasiQuoter where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit ((@=?), testCase)

import Data.Bytes.QuasiQuoter

test_unit :: [TestTree]
test_unit =
  [ testCase "some byte sequence" $ do
    "" @=? ([bin||] :: BS.ByteString)
    "" @=? ([bin||] :: BL.ByteString)
    "" @=? ([hex||] :: BS.ByteString)
    "" @=? ([hex||] :: BL.ByteString)
    " " @=? ([bin|00100000|] :: BS.ByteString)
    " " @=? ([bin|00100000|] :: BL.ByteString)
    "Hello" @=? ([hex|48 65 6c 6c 6f|] :: BS.ByteString)
    "Hello" @=? ([hex|48 65 6c 6c 6f|] :: BL.ByteString)
  , testCase "some non-digit characters" $ do
    "" @=? ([bin|2|] :: BS.ByteString)
    "" @=? ([hex|gg|] :: BS.ByteString)
  , testCase "fromDigitWithBase rejects illegal base" $ do
    Just 0 @=? fromDigitWithBase 1 '0'
    Just 35 @=? fromDigitWithBase 36 'z'
    Nothing @=? fromDigitWithBase 0 '0'
    Nothing @=? fromDigitWithBase (negate 1) '0'
    Nothing @=? fromDigitWithBase 37 'z'
  ]

test_prop :: [TestTree]
test_prop =
  [ testProperty "fromDigitWithBase" $ property $ do
    base <- forAll $ Gen.int $ Range.linear 1 36
    d <- forAll $ Gen.int $ Range.linear 0 36
    let c = if d < 10
          then Char.chr (d + Char.ord '0')
          else Char.chr (d - 10 + Char.ord 'a')
    fromDigitWithBase base c === if base <= d then Nothing else Just d
  ]
