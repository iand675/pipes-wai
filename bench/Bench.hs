{-# LANGUAGE OverloadedStrings #-}
module Main where
import Criterion.Main
import qualified Data.ByteString.Lazy as L
import Pipes

setupProduce :: IO (L.ByteString, L.ByteString, L.ByteString)
setupProduce = return ("", "", "")

main :: IO ()
main = defaultMain [
    env setupProduce $ \ ~(small, medium, large) ->
    bgroup "small" []
  , bgroup "medium" []
  , bgroup "large" []
  ]
