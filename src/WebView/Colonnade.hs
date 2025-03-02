{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

{- | Build HTML tables using @web-view@ and @colonnade@. This module provides
  functionality similar to @lucid-colonnade@ and @blaze-colonnade@ but for the
  web-view library.
-}
module WebView.Colonnade
  ( -- * Apply
    encodeHtmlTable
  , encodeCellTable
  , encodeCellTableSized
  , encodeTable
  
    -- * Cell
  , Cell(..)
  , charCell
  , stringCell
  , textCell
  , htmlCell
  , htmlFromCell
  ) where

import Colonnade (Colonnade)
import qualified Web.View.View as V
import qualified Web.View.Element as E
import qualified Colonnade.Encode as E
import qualified Data.Text as T
import Data.String (IsString(..))
import Data.Foldable (for_)
import Web.View.Types (Attributes)

-- | A table cell with attributes and content
data Cell = Cell
  { cellAttributes :: !(Attributes ())  -- ^ Attributes for the td/th element
  , cellHtml :: !(V.View () ())      -- ^ Content inside the cell
  }

instance IsString Cell where
  fromString = stringCell

instance Semigroup Cell where
  Cell attrs1 content1 <> Cell attrs2 content2 = 
    Cell (attrs1 <> attrs2) (content1 >> content2)

instance Monoid Cell where
  mempty = Cell mempty (pure ())
  mappend = (<>)

-- | Create a cell from HTML content
htmlCell :: V.View () () -> Cell
htmlCell content = Cell mempty content

-- | Create a cell from a string
stringCell :: String -> Cell
stringCell = htmlCell . E.text . T.pack

-- | Create a cell from a character
charCell :: Char -> Cell
charCell = stringCell . pure

-- | Create a cell from text
textCell :: T.Text -> Cell
textCell = htmlCell . E.text

-- | Convert a cell to an HTML element
htmlFromCell :: (Attributes () -> V.View () () -> V.View () ()) -> Cell -> V.View () ()
htmlFromCell f (Cell attrs content) = f attrs content

-- | Encode a table with HTML content
encodeHtmlTable ::
  forall h f a.
  (E.Headedness h, Foldable f) =>
  -- | Attributes of @<table>@ element
  Attributes () ->
  -- | How to encode data as columns
  Colonnade h a (V.View () ()) ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeHtmlTable tableAttrs colonnade xs =
  V.tag "table" (\a -> tableAttrs <> a) $ do
    case E.headednessExtract @h of
      Nothing -> pure ()
      Just _ -> do
        V.tag "thead" mempty $
          V.tag "tr" mempty $
            E.headerMonadicGeneral_ colonnade (\content ->
              V.tag "th" mempty content)
    V.tag "tbody" mempty $
      for_ xs $ \x ->
        V.tag "tr" mempty $
          E.rowMonadic colonnade (\content ->
            V.tag "td" mempty content) x

-- | Encode a table with cells that may have attributes
encodeCellTable ::
  forall h f a.
  (E.Headedness h, Foldable f) =>
  -- | Attributes of @<table>@ element
  Attributes () ->
  -- | How to encode data as columns
  Colonnade h a Cell ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeCellTable tableAttrs colonnade xs =
  V.tag "table" (\a -> tableAttrs <> a) $ do
    case E.headednessExtract @h of
      Nothing -> pure ()
      Just _ -> do
        V.tag "thead" mempty $
          V.tag "tr" mempty $
            E.headerMonadicGeneral_ colonnade (\cell ->
              htmlFromCell (\attrs -> V.tag "th" (\a -> attrs <> a)) cell)
    V.tag "tbody" mempty $
      for_ xs $ \x ->
        V.tag "tr" mempty $
          E.rowMonadic colonnade (\cell ->
            htmlFromCell (\attrs -> V.tag "td" (\a -> attrs <> a)) cell) x

-- | Encode a table with sized columns
encodeCellTableSized ::
  forall h f a.
  (E.Headedness h, E.Headedness (E.Sized Int h), Foldable f) =>
  -- | Attributes of @<table>@ element
  Attributes () ->
  -- | How to encode data as columns
  Colonnade (E.Sized Int h) a Cell ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeCellTableSized tableAttrs colonnade xs =
  V.tag "table" (\a -> tableAttrs <> a) $ do
    case E.headednessExtract @(E.Sized Int h) of
      Nothing -> pure ()
      Just _ -> do
        V.tag "thead" mempty $
          V.tag "tr" mempty $
            E.headerMonadicGeneral_ colonnade (\cell ->
              htmlFromCell (\attrs -> V.tag "th" (\a -> attrs <> a)) cell)
    V.tag "tbody" mempty $
      for_ xs $ \x ->
        V.tag "tr" mempty $
          E.rowMonadic colonnade (\cell ->
            htmlFromCell (\attrs -> V.tag "td" (\a -> attrs <> a)) cell) x


{- | Encode a table with tiered header rows.
<table>
    <thead>
        <tr class='category'>
            <th colspan="2">Personal</th>
            <th colspan="1">Work</th>
        </tr>
        <tr class="subcategory">
            <th colspan="1">Name</th>
            <th colspan="1">Age</th>
            <th colspan="1">Dept.</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Thaddeus</td>
            <td>34</td>
            <td class='sales'>Sales</td>
        </tr>
    </tbody>
</table>
encodeCappedCellTable ::
-}

-- | Encode a table with full control over attributes and structure
encodeTable ::
  (E.Headedness h, Foldable f) =>
  -- | Attributes and structure for header section
  h (Attributes (), Attributes ()) ->
  -- | Attributes for tbody element
  Attributes () ->
  -- | Attributes for each tr element
  (a -> Attributes ()) ->
  -- | Cell wrapper function
  (Attributes () -> V.View () () -> V.View () ()) ->
  -- | Table attributes
  Attributes () ->
  -- | How to encode data as columns
  Colonnade h a (V.View () ()) ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeTable mheadAttrs bodyAttrs trAttrs wrapper tableAttrs colonnade xs =
  V.tag "table" (\a -> tableAttrs <> a) $ do
    case E.headednessExtract of
      Nothing -> pure ()
      Just extract -> case extract mheadAttrs of
        (headAttrs, headTrAttrs) -> do
          V.tag "thead" (\a -> headAttrs <> a) $
            V.tag "tr" (\a -> headTrAttrs <> a) $
              E.headerMonadicGeneral_ colonnade (wrapper mempty)
    V.tag "tbody" (\a -> bodyAttrs <> a) $
      for_ xs $ \x ->
        V.tag "tr" (\a -> trAttrs x <> a) $
          E.rowMonadic colonnade (wrapper mempty) x
