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
data Cell c = Cell
  { cellAttributes :: Attributes c  -- ^ Attributes for the td/th element
  , cellHtml :: V.View c ()      -- ^ Content inside the cell
  }

instance IsString (Cell c) where
  fromString = stringCell

instance Semigroup (Cell c) where
  Cell attrs1 content1 <> Cell attrs2 content2 = 
    Cell (attrs1 <> attrs2) (content1 >> content2)

instance Monoid (Cell c) where
  mempty = Cell mempty (pure ())
  mappend = (<>)

-- | Create a cell from HTML content
htmlCell :: V.View c () -> Cell c
htmlCell content = Cell mempty content

-- | Create a cell from a string
stringCell :: String -> Cell c
stringCell = htmlCell . E.text . T.pack

-- | Create a cell from a character
charCell :: Char -> Cell c
charCell = stringCell . pure

-- | Create a cell from text
textCell :: T.Text -> Cell c
textCell = htmlCell . E.text

-- | Convert a cell to an HTML element
htmlFromCell :: (Attributes c -> V.View c () -> V.View c ()) -> (Cell c) -> V.View c ()
htmlFromCell f (Cell attrs content) = f attrs content

-- | Encode a table with HTML content
encodeHtmlTable ::
  forall h f x c.
  (E.Headedness h, Foldable f) =>
  -- | Attributes of @<table>@ element
  Attributes c ->
  -- | How to encode data as columns
  Colonnade h x (V.View c ()) ->
  -- | Collection of data
  f x ->
  V.View c ()
encodeHtmlTable =
  encodeTable
    (E.headednessPure (mempty, mempty))
    mempty
    (const mempty)
    (\_ content -> content)

-- | Encode a table with cells that may have attributes
encodeCellTable ::
  forall h f x c.
  (E.Headedness h, Foldable f) =>
  -- | Attributes of @<table>@ element
  Attributes c ->
  -- | How to encode data as columns
  Colonnade h x (Cell c) ->
  -- | Collection of data
  f x ->
  V.View c ()
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
  forall h f x c.
  (E.Headedness h, E.Headedness (E.Sized Int h), Foldable f) =>
  -- | Attributes of @<table>@ element
  Attributes c ->
  -- | How to encode data as columns
  Colonnade (E.Sized Int h) x (Cell c) ->
  -- | Collection of data
  f x ->
  V.View c ()
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

-- | Encode a table with full control over attributes and structure
encodeTable ::
  (E.Headedness h, Foldable f) =>
  -- | Attributes and structure for header section
  h (Attributes c, Attributes c) ->
  -- | Attributes for tbody element
  Attributes c ->
  -- | Attributes for each tr element
  (x -> Attributes c) ->
  -- | Cell wrapper function
  (Attributes c -> V.View c () -> V.View c ()) ->
  -- | Table attributes
  Attributes c ->
  -- | How to encode data as columns
  Colonnade h x (V.View c ()) ->
  -- | Collection of data
  f x ->
  V.View c ()
encodeTable mheadAttrs bodyAttrs trAttrs wrapper tableAttrs colonnade xs =
  V.tag "table" (\a -> tableAttrs <> a) $ do
    case E.headednessExtract of
      Nothing -> pure ()
      Just extract -> case extract mheadAttrs of
        (headAttrs, headTrAttrs) -> do
          V.tag "thead" (\a -> headAttrs <> a) $
            V.tag "tr" (\a -> headTrAttrs <> a) $
              E.headerMonadicGeneral_ colonnade (\content ->
                wrapper mempty (V.tag "th" mempty content))
    V.tag "tbody" (\a -> bodyAttrs <> a) $
      for_ xs $ \x ->
        V.tag "tr" (\a -> trAttrs x <> a) $
          E.rowMonadic colonnade (\content ->
            wrapper mempty (V.tag "td" mempty content)) x
