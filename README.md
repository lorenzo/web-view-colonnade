# Web View Colonnade

Build HTML tables using [colonnade](https://hackage.haskell.org/package/colonnade) and rendering with the [web-view](https://hackage.haskell.org/package/web-view) library in Haskell.

This library provides functionality similar to `lucid-colonnade` and `blaze-colonnade`, but for the web-view library. It lets you build complex HTML tables with minimal boilerplate.

## Installation

Add the following to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - colonnade
  - web-view
  - web-view-colonnade
```

## Examples

### Basic Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Colonnade as C
import qualified Web.View.View as V
import qualified Web.View.Element as E
import Web.View (renderText)
import WebView.Colonnade

-- Define a data type to represent our data
data Person = Person
  { name :: T.Text
  , age :: Int
  }

-- Define some data
people :: [Person]
people =
  [ Person "Alice" 30
  , Person "Bob" 25
  , Person "Carol" 35
  ]

-- Define the table structure
personTable :: C.Colonnade C.Headed Person (V.View c ())
personTable = mconcat
  [ C.headed "Name" (E.text . name)
  , C.headed "Age" (E.text . T.pack . show . age)
  ]

main :: IO ()
main = do
  -- Render a table with custom attributes
  let html = encodeHtmlTable (V.extClass "person-table") personTable people
  putStrLn $ renderText html
```

This produces:

```html
<table class='person-table'>
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
    <tr>
      <td>Carol</td>
      <td>35</td>
    </tr>
  </tbody>
</table>
```

### Cell Attributes

The `Cell` type allows you to add attributes to individual table cells:

```haskell
personCellTable :: C.Colonnade C.Headed Person (Cell c)
personCellTable = mconcat
  [ C.headed "Name" (\p -> Cell (V.extClass "name-cell") (E.text $ name p))
  , C.headed "Age" (\p -> Cell (V.extClass "age-cell") (E.text . T.pack . show $ age p))
  ]

main :: IO ()
main = do
  let html = encodeCellTable (V.extClass "person-table") personCellTable people
  putStrLn $ renderText html
```

This will render each cell with the appropriate class attributes.

## Contributing

We welcome contributions to web-view-colonnade! This project uses a nix-based devenv shell for development.

### Setting Up the Development Environment

1. Make sure you have [Nix](https://nixos.org/download.html) installed
2. Install [devenv](https://devenv.sh/getting-started/)
3. Clone this repository:
   ```
   git clone https://github.com/lorenzo/web-view-colonnade.git
   cd web-view-colonnade
   ```
4. Start the development shell:
   ```
   devenv shell
   ```
5. Build the project:
   ```
   cabal build
   ```
6. Run the tests:
   ```
   cabal test
   ```

### Pull Requests

1. Fork the repository
2. Create a new branch for your feature
3. Add tests for your new feature
4. Ensure all tests pass
5. Submit a pull request

## License

This project is licensed under the MIT license - see the LICENSE file for details.
