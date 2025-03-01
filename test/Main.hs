module Main (main) where

import Test.Hspec
import qualified WebView.ColonnadeSpec

main :: IO ()
main = hspec $ do
  describe "WebView.Colonnade" WebView.ColonnadeSpec.spec
