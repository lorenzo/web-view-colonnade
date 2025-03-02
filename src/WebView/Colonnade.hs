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
import Web.View.Types (Mod)

-- | A table cell with attributes and content
data Cell c = Cell
  { cellAttributes :: Mod c  -- ^ Attributes for the td/th element
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
htmlFromCell :: (Mod c -> V.View c () -> V.View c ()) -> (Cell c) -> V.View c ()
htmlFromCell f (Cell attrs content) = f attrs content

-- | Encode a table with HTML content
encodeHtmlTable ::
  forall h f x c.
  (E.Headedness h, Foldable f) =>
  -- | Attributes of @<table>@ element
  Mod c ->
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
    (\tagFn content -> tagFn mempty content)

-- | Encode a table with cells that may have attributes
encodeCellTable ::
  forall h f x c.
  (E.Headedness h, Foldable f) =>
  -- | Attributes of @<table>@ element
  Mod c ->
  -- | How to encode data as columns
  Colonnade h x (Cell c) ->
  -- | Collection of data
  f x ->
  V.View c ()
encodeCellTable =
  encodeTable
    (E.headednessPure (mempty, mempty))
    mempty
    (const mempty)
    htmlFromCell

{- | Encode a table. This handles a very general case and
  is seldom needed by users. One of the arguments provided is
  used to add attributes to the generated @\<tr\>@ elements.
-}
encodeTable ::
  forall h f x v c.
  (E.Headedness h, Foldable f) =>
  -- | Attributes and structure for header section
  h (Mod c, Mod c) ->
  -- | Attributes for tbody element
  Mod c ->
  -- | Attributes for each tr element
  (x -> Mod c) ->
  -- | Cell wrapper function
  ((Mod c -> V.View c () -> V.View c ()) -> v -> V.View c ()) ->
  -- | Table attributes
  Mod c ->
  -- | How to encode data as columns
  Colonnade h x v ->
  -- | Collection of data
  f x ->
  V.View c ()
encodeTable mtheadAttrs tbodyAttrs trAttrs wrapContent tableAttrs colonnade xs =
  V.tag "table" tableAttrs $ do
    d1 <- case E.headednessExtractForall of
      Nothing -> pure mempty
      Just extractForall -> do
        let (theadAttrs, theadTrAttrs) = extract mtheadAttrs
        V.tag "thead" theadAttrs $
          V.tag "tr" theadTrAttrs $ do
            foldlMapM' (wrapContent (V.tag "th") . extract . E.oneColonnadeHead) (E.getColonnade colonnade)
        where
          extract :: forall y. h y -> y
          extract = E.runExtractForall extractForall
    d2 <- encodeBody trAttrs wrapContent tbodyAttrs colonnade xs
    pure (d1 <> d2)

foldlMapM' :: forall g b a m. (Foldable g, Monoid b, Monad m) => (a -> m b) -> g a -> m b
foldlMapM' f xs = foldr f' pure xs mempty
 where
  f' :: a -> (b -> m b) -> b -> m b
  f' x k bl = do
    br <- f x
    let !b = mappend bl br
    k b

encodeBody ::
  (Foldable f) =>
  -- | Attributes of each @\<tr\>@ element
  (a -> Mod c) ->
  -- | Wrap content and convert to 'Html'
  ((Mod c -> V.View c () -> V.View c ()) -> v -> V.View c ()) ->
  -- | Attributes of @\<tbody\>@ element
  Mod c ->
  -- | How to encode data as a row
  Colonnade h a v ->
  -- | Collection of data
  f a ->
  V.View c ()
encodeBody trAttrs wrapContent tbodyAttrs colonnade xs = do
  V.tag "tbody" tbodyAttrs $ do
    for_ xs $ \x -> do
      V.tag "tr" (trAttrs x) $ do
        E.rowMonadic colonnade (wrapContent (V.tag "td")) x