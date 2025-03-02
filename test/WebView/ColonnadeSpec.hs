{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module WebView.ColonnadeSpec (spec) where

import Test.Hspec
import WebView.Colonnade
import qualified Web.View.View as V
import qualified Web.View.Style as V
import qualified Web.View.Element as E
import Web.View (renderText)
import qualified Colonnade as C
import qualified Colonnade.Encode as E
import qualified Data.Text as T
import Data.String (IsString(fromString))
import Data.String.Interpolate (i)

data Person = Person
  { name :: T.Text
  , age :: Int
  } deriving (Show, Eq)

spec :: Spec
spec = do
  describe "Cell" $ do
    it "implements IsString" $ do
      let s = "test"
          cell = fromString s :: Cell c
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      result `shouldBe` [i|<td>#{s}</td>|]

    it "implements Semigroup" $ do
      let s1 = "test1"
          s2 = "test2"
          cell1 = fromString s1 :: Cell c
          cell2 = fromString s2 :: Cell c
          combined = cell1 <> cell2
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) combined)
      result `shouldBe` [i|<td>
  #{s1}#{s2}</td>|]

    it "implements Monoid" $ do
      let s = "test"
          cell = fromString s :: Cell c
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
      let html = encodeHtmlTable (V.extClass "table") personColonnade people
      let result = renderText html
      result `shouldBe` [i|<table class='table'>
  <thead>
    <tr>
      <th>Name</th>
      <th>Age</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Alice</td>
      <td>30</td>
    </tr>
    <tr>
      <td>Bob</td>
      <td>25</td>
    </tr>
  </tbody>
</table>|]

    it "preserves table attributes" $ do
      let attr = "class"
          val = "test-table"
          html = encodeHtmlTable (V.att attr val) personColonnade []
          rendered = renderText html
      rendered `shouldBe` [i|<table class='#{val}'>
  <thead>
    <tr>
      <th>Name</th>
      <th>Age</th>
    </tr>
  </thead>
  <tbody></tbody>
</table>|]

  describe "encodeCellTable" $ do
    let personColonnade = mconcat
          [ C.headed "Name" (\p -> Cell (V.extClass "name") (E.text $ name p))
          , C.headed "Age" (\p -> Cell (V.extClass "age") (E.text . T.pack . show $ age p))
          ]
        people = [Person "Alice" 30, Person "Bob" 25]

    it "preserves cell attributes" $ do
      let html = encodeCellTable mempty personColonnade people
          rendered = renderText html
      rendered `shouldBe` [i|<table>
  <thead>
    <tr>
      <th>Name</th>
      <th>Age</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td class='name'>Alice</td>
      <td class='age'>30</td>
    </tr>
    <tr>
      <td class='name'>Bob</td>
      <td class='age'>25</td>
    </tr>
  </tbody>
</table>|]

  describe "Cell constructors" $ do
    it "charCell creates valid HTML" $ do
      let c = 'a'
          cell = charCell c
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      result `shouldBe` [i|<td>#{c}</td>|]

    it "stringCell creates valid HTML" $ do
      let s = "test"
          cell = stringCell s
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      result `shouldBe` [i|<td>#{s}</td>|]

    it "textCell creates valid HTML" $ do
      let t = "test" :: T.Text
          cell = textCell t
      let result = renderText (htmlFromCell (\_ -> V.tag "td" mempty) cell)
      result `shouldBe` [i|<td>#{t}</td>|]

  describe "encodeTable" $ do
    let personColonnade = mconcat
          [ C.headed "Name" (E.text . name)
          , C.headed "Age" (E.text . T.pack . show . age)
          ]
        people = [Person "Alice" 30, Person "Bob" 25]

    it "applies all attribute functions" $ do
      let html = encodeTable
            (E.headednessPure (V.extClass "head", V.extClass "head-row"))
            (V.extClass "body")
            (\_ -> V.extClass "row")
            (\tagFn content -> tagFn (V.extClass "i") content)
            (V.extClass "table")
            personColonnade
            people
          rendered = renderText html
      rendered `shouldBe` [i|<table class='table'>
  <thead class='head'>
    <tr class='head-row'>
      <th class='i'>Name</th>
      <th class='i'>Age</th>
    </tr>
  </thead>
  <tbody class='body'>
    <tr class='row'>
      <td class='i'>Alice</td>
      <td class='i'>30</td>
    </tr>
    <tr class='row'>
      <td class='i'>Bob</td>
      <td class='i'>25</td>
    </tr>
  </tbody>
</table>|]
