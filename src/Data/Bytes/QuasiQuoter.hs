{-# LANGUAGE TemplateHaskellQuotes #-}
-- |
-- Module:     Data.Bytes.QuasiQuoter
-- Copyright:  (c) 2024 0y2k
-- License:    Apache-2.0
-- Maintainer: 0y2k <0x0y2k@gmail.com>
--
-- Offers QuasiQuoters for byte sequence.

module Data.Bytes.QuasiQuoter
  ( FromBytes(..)
  , fromDigitWithBase
  , strWithBase
  , bin
  , hex
  ) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Maybe (mapMaybe)
import Language.Haskell.TH.Quote

-- $setup
-- >>> :seti -XQuasiQuotes

-- | Offers @StrictByteString -> a@.
class FromBytes a where
  fromBytes :: BS.ByteString -> a

instance FromBytes BS.ByteString where
  fromBytes = id
instance FromBytes BL.ByteString where
  fromBytes = BL.fromStrict

ord_0, ord_A, ord_a :: Int
ord_0 = ord '0'
ord_A = ord 'A'
ord_a = ord 'a'

-- | Accepts @\'0\'..\'9\', \'A\'..\'Z\', \'a\'..\'z\'@.
-- @1 <= base <= 36@
fromDigitWithBase :: Int -> Char -> Maybe Int
fromDigitWithBase base
  | 0 < base && base <= 10 = \c -> let i = ord c in
    if ord_0 <= i && i - ord_0 < base
      then Just $ i - ord_0
      else Nothing
  | 10 < base && base <= 36 = \c -> let i = ord c in
    if ord_0 <= i && i - ord_0 < base
      then Just $ i - ord_0
      else if ord_A <= i && i - ord_A < base - 10
        then Just $ i - ord_A + 10
        else if ord_a <= i && i - ord_a < base - 10
          then Just $ i - ord_a + 10
          else Nothing
  | otherwise = const Nothing

strWithBase' :: Int -> String -> BS.ByteString
strWithBase' b
  | b < 1 || 5 < b = error "strWithBase: illegal b"
  | otherwise = BS.pack . g . f . mapMaybe (fromDigitWithBase base)
 where
  base = 1 `shiftL` b
  f [] = []
  f (i:is) = (map (\s -> testBit i s) $ reverse [0..pred b]) ++ f is
  g [] = []
  g [x0] = [h x0 `shiftL` 7]
  g [x0,x1]
    = [ h x0 `shiftL` 7
    .|. h x1 `shiftL` 6]
  g [x0,x1,x2]
    = [ h x0 `shiftL` 7
    .|. h x1 `shiftL` 6
    .|. h x2 `shiftL` 5]
  g [x0,x1,x2,x3]
    = [ h x0 `shiftL` 7
    .|. h x1 `shiftL` 6
    .|. h x2 `shiftL` 5
    .|. h x3 `shiftL` 4]
  g [x0,x1,x2,x3,x4]
    = [ h x0 `shiftL` 7
    .|. h x1 `shiftL` 6
    .|. h x2 `shiftL` 5
    .|. h x3 `shiftL` 4
    .|. h x4 `shiftL` 3]
  g [x0,x1,x2,x3,x4,x5]
    = [ h x0 `shiftL` 7
    .|. h x1 `shiftL` 6
    .|. h x2 `shiftL` 5
    .|. h x3 `shiftL` 4
    .|. h x4 `shiftL` 3
    .|. h x5 `shiftL` 2]
  g [x0,x1,x2,x3,x4,x5,x6]
    = [ h x0 `shiftL` 7
    .|. h x1 `shiftL` 6
    .|. h x2 `shiftL` 5
    .|. h x3 `shiftL` 4
    .|. h x4 `shiftL` 3
    .|. h x5 `shiftL` 2
    .|. h x6 `shiftL` 1]
  g (x0:x1:x2:x3:x4:x5:x6:x7:xs)
    = ( h x0 `shiftL` 7
    .|. h x1 `shiftL` 6
    .|. h x2 `shiftL` 5
    .|. h x3 `shiftL` 4
    .|. h x4 `shiftL` 3
    .|. h x5 `shiftL` 2
    .|. h x6 `shiftL` 1
    .|. h x7) : g xs
  h = fromIntegral . fromEnum

-- | Internal function for quasiquotes. @1 <= b <= 5@
strWithBase :: FromBytes a => Int -> String -> a
strWithBase b = fromBytes . strWithBase' b

qqWithBase :: Int -> QuasiQuoter
qqWithBase b = QuasiQuoter
  (\s -> [e|strWithBase b s|])
  (error "qq-bytes: cannot use this quasiquoter as a pattern")
  (error "qq-bytes: cannot use this quasiquoter as a type")
  (error "qq-bytes: cannot use this quasiquoter as a dec")

-- | Binary string into byte sequence.
-- Skips non-binary character.
-- Internally means @'strWithBase' 1@.
--
-- >>> [bin|00100000|] :: BS.ByteString
-- " "
bin :: QuasiQuoter
bin = qqWithBase 1

-- | Hex string into byte sequence.
-- Skips non-hex character.
-- Internally means @'strWithBase' 4@.
--
-- >>> [hex|48 65 6c 6c 6f|] :: BS.ByteString
-- "Hello"
hex :: QuasiQuoter
hex = qqWithBase 4
