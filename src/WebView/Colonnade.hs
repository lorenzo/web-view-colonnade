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
  , encodeCappedTable
  , encodeCappedCellTable
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

-- | A table cell with attributes and content
data Cell = Cell
  { cellAttributes :: ![(T.Text, T.Text)]  -- ^ Attributes for the td/th element
  , cellHtml :: !(V.View () ())           -- ^ Content inside the cell
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
htmlFromCell :: ([(T.Text, T.Text)] -> V.View () () -> V.View () ()) -> Cell -> V.View () ()
htmlFromCell f (Cell attrs content) = f attrs content

-- | Encode a table with HTML content
encodeHtmlTable ::
  forall h f a.
  (E.Headedness h, Foldable f) =>
  -- | Attributes of @<table>@ element
  [(T.Text, T.Text)] ->
  -- | How to encode data as columns
  Colonnade h a (V.View () ()) ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeHtmlTable tableAttrs colonnade xs =
  V.tag "table" (foldMap (\(k, v) -> V.att k v) tableAttrs) $ do
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
  [(T.Text, T.Text)] ->
  -- | How to encode data as columns
  Colonnade h a Cell ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeCellTable tableAttrs colonnade xs =
  V.tag "table" (foldMap (\(k, v) -> V.att k v) tableAttrs) $ do
    case E.headednessExtract @h of
      Nothing -> pure ()
      Just _ -> do
        V.tag "thead" mempty $
          V.tag "tr" mempty $
            E.headerMonadicGeneral_ colonnade (\cell ->
              htmlFromCell (\attrs -> V.tag "th" (foldMap (\(k, v) -> V.att k v) attrs)) cell)
    V.tag "tbody" mempty $
      for_ xs $ \x ->
        V.tag "tr" mempty $
          E.rowMonadic colonnade (\cell ->
            htmlFromCell (\attrs -> V.tag "td" (foldMap (\(k, v) -> V.att k v) attrs)) cell) x

-- | Encode a table with sized columns
encodeCellTableSized ::
  forall h f a.
  (E.Headedness h, E.Headedness (E.Sized Int h), Foldable f) =>
  -- | Attributes of @<table>@ element
  [(T.Text, T.Text)] ->
  -- | How to encode data as columns
  Colonnade (E.Sized Int h) a Cell ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeCellTableSized tableAttrs colonnade xs =
  V.tag "table" (foldMap (\(k, v) -> V.att k v) tableAttrs) $ do
    case E.headednessExtract @(E.Sized Int h) of
      Nothing -> pure ()
      Just _ -> do
        V.tag "thead" mempty $
          V.tag "tr" mempty $
            E.headerMonadicGeneral_ colonnade (\cell ->
              htmlFromCell (\attrs -> V.tag "th" (foldMap (\(k, v) -> V.att k v) attrs)) cell)
    V.tag "tbody" mempty $
      for_ xs $ \x ->
        V.tag "tr" mempty $
          E.rowMonadic colonnade (\cell ->
            htmlFromCell (\attrs -> V.tag "td" (foldMap (\(k, v) -> V.att k v) attrs)) cell) x

-- | Encode a table with capped columns and HTML content
encodeCappedTable ::
  (E.Headedness h, Foldable f) =>
  -- | Maximum number of columns
  Int ->
  -- | Attributes of @<table>@ element
  [(T.Text, T.Text)] ->
  -- | How to encode data as columns
  Colonnade h a (V.View () ()) ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeCappedTable _ tableAttrs colonnade xs =
  -- TODO: Implement proper column capping functionality
  -- We need to find a way to limit the number of columns in the Colonnade
  -- For now, we're just passing through all columns
  encodeHtmlTable tableAttrs colonnade xs

-- | Encode a table with capped columns and cells that may have attributes
encodeCappedCellTable ::
  (E.Headedness h, Foldable f) =>
  -- | Maximum number of columns
  Int ->
  -- | Attributes of @<table>@ element
  [(T.Text, T.Text)] ->
  -- | How to encode data as columns
  Colonnade h a Cell ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeCappedCellTable _ tableAttrs colonnade xs =
  -- TODO: Implement proper column capping functionality
  -- We need to find a way to limit the number of columns in the Colonnade
  -- For now, we're just passing through all columns
  encodeCellTable tableAttrs colonnade xs

-- | Encode a table with full control over attributes and structure
encodeTable ::
  (E.Headedness h, Foldable f) =>
  -- | Attributes and structure for header section
  h ([(T.Text, T.Text)], [(T.Text, T.Text)]) ->
  -- | Attributes for tbody element
  [(T.Text, T.Text)] ->
  -- | Attributes for each tr element
  (a -> [(T.Text, T.Text)]) ->
  -- | Cell wrapper function
  ([(T.Text, T.Text)] -> V.View () () -> V.View () ()) ->
  -- | Table attributes
  [(T.Text, T.Text)] ->
  -- | How to encode data as columns
  Colonnade h a (V.View () ()) ->
  -- | Collection of data
  f a ->
  V.View () ()
encodeTable mheadAttrs bodyAttrs trAttrs wrapper tableAttrs colonnade xs =
  let
    makeAtts attrs = foldMap (\(k, v) -> V.att k v) attrs
  in V.tag "table" (makeAtts tableAttrs) $ do
    case E.headednessExtract of
      Nothing -> pure ()
      Just extract -> case extract mheadAttrs of
        (headAttrs, headTrAttrs) -> do
          V.tag "thead" (makeAtts headAttrs) $
            V.tag "tr" (makeAtts headTrAttrs) $
              E.headerMonadicGeneral_ colonnade (wrapper [])
    V.tag "tbody" (makeAtts bodyAttrs) $
      for_ xs $ \x ->
        V.tag "tr" (makeAtts (trAttrs x)) $
          E.rowMonadic colonnade (wrapper []) x
