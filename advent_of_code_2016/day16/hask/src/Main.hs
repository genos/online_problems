{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable         (traverse_)

input :: ByteString
input = "10001001100000001"

bitFlip :: Char -> Char
bitFlip '0' = '1'
bitFlip _   = '0'

dragon :: ByteString -> ByteString
dragon β = β `BC.append` ('0' `BC.cons` BC.reverse (BC.map bitFlip β))

dragonTest :: Bool
dragonTest = all
  (\(α, β) -> dragon α == β)
  [ ("1"           , "100")
  , ("0"           , "001")
  , ("11111"       , "11111000000")
  , ("111100001010", "1111000010100101011110000")
  ]

checkStep :: ByteString -> ByteString
checkStep β = go β ""
 where
  {-# INLINABLE go #-}
  go :: ByteString -> ByteString -> ByteString
  go α ω | BC.null α = ω
         | otherwise = go α' ω'
   where
    γ  = α `BC.index` 0
    δ  = α `BC.index` 1
    ϵ  = if γ == δ then '1' else '0'
    α' = BC.drop 2 α
    ω' = ω `BC.snoc` ϵ

checkStepTest :: Bool
checkStepTest = all (\(α, β) -> checkStep α == β)
                    [("110010110100", "110101"), ("110101", "100")]

shrink :: ByteString -> ByteString
shrink β | odd $ BC.length β = β
         | otherwise         = shrink $ checkStep β

expand :: Int -> ByteString -> ByteString
expand λ β | BC.length β >= λ = β
           | otherwise        = expand λ $ dragon β

checkSum :: Int -> ByteString -> ByteString
checkSum λ = shrink . BC.take λ . expand λ

main :: IO ()
main = do
  traverse_ print                              [dragonTest, checkStepTest]
  traverse_ (BC.putStrLn . (`checkSum` input)) [272, 35651584]
