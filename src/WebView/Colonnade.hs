{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}


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

{- | Build HTML tables using @web-view@ and @colonnade@. This module provides
  functionality similar to @lucid-colonnade@ and @blaze-colonnade@ but for the
  web-view library.

  = Usage

  We start with a few necessary imports and some example data types:

  >>> :set -XOverloadedStrings
  >>> import Data.Monoid (mconcat,(<>))
  >>> import Data.Char (toLower)
  >>  import qualified Data.Text as T
  >>> import Data.Profunctor (Profunctor(lmap))
  >>> import Colonnade (Colonnade,Headed,Headless,headed)
  >>> import Web.View
  >>> import qualified Web.View.Style as V 
  >>> import qualified Web.View.Types as V
  >>> data Department = Management | Sales | Engineering deriving (Show,Eq)
  >>> data Employee = Employee { name :: T.Text, department :: Department, age :: Int }

  We define some employees that we will display in a table:

  >>> :{
  let employees =
        [ Employee "Thaddeus" Sales 34
        , Employee "Lucia" Engineering 33
        , Employee "Pranav" Management 57
        ]
  :}

  Let's build a table that displays the name and the age
  of an employee. Additionally, we will emphasize the names of
  engineers using a @\<strong\>@ tag.

  >>> :{
  let tableEmpA :: Colonnade Headed Employee (View c ())
      tableEmpA = mconcat
        [ headed "Name" $ \emp -> case department emp of
            Engineering -> el bold (text (name emp))
            _ -> text (name emp)
        , headed "Age" (text . T.pack . show . age)
        ]
  :}

  The type signature of @tableEmpA@ is inferrable but is written
  out for clarity in this example. Note that the first
  argument to 'headed' can be passed as a string literal due to the @OverloadedStrings@ extension.
  Let's continue:

  >>> let customAttrs = V.extClass "stylish-table" <> V.att "id" "main-table"
  >>> renderText (encodeHtmlTable customAttrs tableEmpA employees)
  <table class='stylish-table' id='main-table'>
    <thead>
      <tr>
        <th>Name</th>
        <th>Age</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Thaddeus</td>
        <td>34</td>
      </tr>
      <tr>
        <td><div class="bold">Lucia</div></td>
        <td>33</td>
      </tr>
      <tr>
        <td>Pranav</td>
        <td>57</td>
      </tr>
    </tbody>
  </table>

  Excellent. As expected, Lucia's name is wrapped in a @\<strong\>@ tag
  since she is an engineer.

  One limitation of using @View@ as the content
  type of a 'Colonnade' is that we are unable to add attributes to
  the @\<td\>@ and @\<th\>@ elements. This library provides the 'Cell' type
  to work around this problem. A 'Cell' is just a @V.View@ content and a set
  of attributes to be applied to its parent @\<th\>@ or @\<td\>@. To illustrate
  its use, another employee table will be built. This table will
  contain a single column indicating the department of each employee. Each
  cell will be assigned a class name based on the department. Let's build a table 
  that encodes departments:

  >>> :{
  let tableDept :: Colonnade Headed Department (Cell c)
      tableDept = mconcat
        [ headed "Dept." $ \d -> Cell
            (V.extClass (V.ClassName $ T.pack (map Data.Char.toLower (show d))))
            (E.text (T.pack (show d)))
        ]
  :}

  Again, @OverloadedStrings@ plays a role, this time allowing the
  literal @"Dept."@ to be accepted as a value of type 'Cell'. To avoid
  this extension, 'stringCell' could be used to upcast the 'String'.
  To try out our 'Colonnade' on a list of departments, we need to use
  'encodeCellTable' instead of 'encodeHtmlTable':

  >>> let twoDepts = [Sales,Management]
  >>> renderText (encodeCellTable customAttrs tableDept twoDepts)
  <table class='stylish-table' id='main-table'>
    <thead>
      <tr>
        <th>Dept.</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td class='sales'>Sales</td>
      </tr>
      <tr>
        <td class='management'>Management</td>
      </tr>
    </tbody>
  </table>

  The attributes on the @\<td\>@ elements show up as they are expected to.
  Now, we take advantage of the @Profunctor@ instance of 'Colonnade' to allow
  this to work on @Employee@\'s instead:

  >>> :t lmap
  lmap :: Profunctor p => (a -> b) -> p b c -> p a c
  >>> let tableEmpB = lmap department tableDept
  >>> :t tableEmpB
  tableEmpB :: Colonnade Headed Employee (Cell c)
  >>> renderText (encodeCellTable customAttrs tableEmpB employees)
  <table class='stylish-table' id='main-table'>
    <thead>
      <tr>
        <th>Dept.</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td class='sales'>Sales</td>
      </tr>
      <tr>
        <td class='engineering'>Engineering</td>
      </tr>
      <tr>
        <td class='management'>Management</td>
      </tr>
    </tbody>
  </table>

  This table shows the department of each of our three employees, additionally
  making a lowercased version of the department into a class name for the @\<td\>@.
  This table is nice for illustrative purposes, but it does not provide all the
  information that we have about the employees. If we combine it with the
  earlier table we wrote, we can present everything in the table. One small
  roadblock is that the types of @tableEmpA@ and @tableEmpB@ do not match, which
  prevents a straightforward monoidal append:

  >>> :t tableEmpA
  tableEmpA :: Colonnade Headed Employee (V.View c ())
  >>> :t tableEmpB
  tableEmpB :: Colonnade Headed Employee (Cell c)

  We can upcast the content type with 'fmap':

  >>> let tableEmpC = fmap htmlCell tableEmpA <> tableEmpB
  >>> :t tableEmpC
  tableEmpC :: Colonnade Headed Employee (Cell c)
  >>> renderText (encodeCellTable customAttrs tableEmpC employees)
  <table class='stylish-table' id='main-table'>
    <thead>
      <tr>
        <th>Name</th>
        <th>Age</th>
        <th>Dept.</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Thaddeus</td>
        <td>34</td>
        <td class='sales'>Sales</td>
      </tr>
      <tr>
        <td><strong>Lucia</strong></td>
        <td>33</td>
        <td class='engineering'>Engineering</td>
      </tr>
      <tr>
        <td>Pranav</td>
        <td>57</td>
        <td class='management'>Management</td>
      </tr>
    </tbody>
  </table>
-}

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