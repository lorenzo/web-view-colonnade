{-# LANGUAGE OverloadedStrings #-}
module WebView.ColonnadeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import WebView.Colonnade
import qualified Web.View.View as V
import qualified Web.View.Element as E
import Web.View (renderText)
import qualified Colonnade as C
import qualified Colonnade.Encode as E
import qualified Data.Text as T
import Web.View.Types (Attributes(..))
import qualified Data.Map as Map

import Data.String (IsString(fromString))

data Person = Person
  { name :: T.Text
  , age :: Int
  } deriving (Show, Eq)

instance Arbitrary Person where
  arbitrary = Person <$> (T.pack <$> listOf1 (elements ['a'..'z']))
                    <*> choose (0, 120)

spec :: Spec
spec = do
  describe "Cell" $ do
    it "implements IsString" $ do
      let s = "test"
          cell = fromString s :: Cell
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      T.isInfixOf (T.pack s) result `shouldBe` True
      T.isInfixOf "<td" result `shouldBe` True
      T.isInfixOf "</td>" result `shouldBe` True

    it "implements Semigroup" $ do
      let s1 = "test1"
          s2 = "test2"
          cell1 = fromString s1 :: Cell
          cell2 = fromString s2 :: Cell
          combined = cell1 <> cell2
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) combined)
      T.isInfixOf (T.pack s1 <> T.pack s2) result `shouldBe` True
      T.isInfixOf "<td" result `shouldBe` True
      T.isInfixOf "</td>" result `shouldBe` True

    it "implements Monoid" $ do
      let s = "test"
          cell = fromString s :: Cell
      let result1 = renderText (htmlFromCell (\_ -> V.tag "td" mempty) (cell <> mempty))
          result2 = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      result1 `shouldBe` result2

  describe "encodeHtmlTable" $ do
    let personColonnade = mconcat
          [ C.headed "Name" (E.text . name)
          , C.headed "Age" (E.text . T.pack . show . age)
          ]
        people = [Person "Alice" 30, Person "Bob" 25]

    it "generates correct HTML structure" $ do
      let html = encodeHtmlTable mempty personColonnade people
      let result = renderText html
      T.isInfixOf "<table" result `shouldBe` True
      T.isInfixOf "<thead" result `shouldBe` True
      T.isInfixOf "<tbody" result `shouldBe` True
      T.isInfixOf "<th>Name</th>" result `shouldBe` True
      T.isInfixOf "<th>Age</th>" result `shouldBe` True
      T.isInfixOf "<td>Alice</td>" result `shouldBe` True
      T.isInfixOf "<td>30</td>" result `shouldBe` True
      T.isInfixOf "<td>Bob</td>" result `shouldBe` True
      T.isInfixOf "<td>25</td>" result `shouldBe` True

    it "preserves table attributes" $ do
      let attr = "class"
          val = "test-table"
          html = encodeHtmlTable (Attributes [] (Map.singleton attr val)) personColonnade []
          rendered = renderText html
      T.isInfixOf val rendered `shouldBe` True

  describe "encodeCellTable" $ do
    let personColonnade = mconcat
          [ C.headed "Name" (\p -> Cell (Attributes [] (Map.singleton "class" "name")) (E.text $ name p))
          , C.headed "Age" (\p -> Cell (Attributes [] (Map.singleton "class" "age")) (E.text . T.pack . show $ age p))
          ]
        people = [Person "Alice" 30, Person "Bob" 25]

    it "preserves cell attributes" $ do
      let html = encodeCellTable mempty personColonnade people
          rendered = renderText html
      T.isInfixOf "name" rendered `shouldBe` True
      T.isInfixOf "age" rendered `shouldBe` True

  describe "Cell constructors" $ do
    it "charCell creates valid HTML" $ do
      let c = 'a'
          cell = charCell c
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      T.isInfixOf (T.singleton c) result `shouldBe` True
      T.isInfixOf "<td" result `shouldBe` True
      T.isInfixOf "</td>" result `shouldBe` True

    it "stringCell creates valid HTML" $ do
      let s = "test"
          cell = stringCell s
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      T.isInfixOf (T.pack s) result `shouldBe` True
      T.isInfixOf "<td" result `shouldBe` True
      T.isInfixOf "</td>" result `shouldBe` True

    it "textCell creates valid HTML" $ do
      let t = "test" :: T.Text
          cell = textCell t
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      T.isInfixOf t result `shouldBe` True
      T.isInfixOf "<td" result `shouldBe` True
      T.isInfixOf "</td>" result `shouldBe` True

  

  describe "encodeTable" $ do
    let personColonnade = mconcat
          [ C.headed "Name" (E.text . name)
          , C.headed "Age" (E.text . T.pack . show . age)
          ]
        people = [Person "Alice" 30, Person "Bob" 25]

    it "applies all attribute functions" $ do
      let html = encodeTable
            (E.headednessPure (Attributes [] (Map.singleton "class" "head"), Attributes [] (Map.singleton "class" "head-row")))
            (Attributes [] (Map.singleton "class" "body"))
            (\_ -> Attributes [] (Map.singleton "class" "row"))
            (\_ -> V.tag "td" mempty)
            (Attributes [] (Map.singleton "class" "table"))
            personColonnade
            people
          rendered = renderText html
      T.isInfixOf "table" rendered `shouldBe` True
      T.isInfixOf "head" rendered `shouldBe` True
      T.isInfixOf "head-row" rendered `shouldBe` True
      T.isInfixOf "body" rendered `shouldBe` True
      T.isInfixOf "row" rendered `shouldBe` True
