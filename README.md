[![Hackage](https://img.shields.io/hackage/v/servant-pandoc.svg)](https://hackage.haskell.org/package/servant-pandoc) [![Build Status](https://travis-ci.org/mpickering/servant-pandoc.svg)](https://travis-ci.org/mpickering/servant-pandoc)

An extension to [servant-docs] that allows you to use [Pandoc] to
render your Servant API documentation.

[servant-docs]: http://hackage.haskell.org/package/servant-docs
[Pandoc]: http://pandoc.org/

How to use this package
=======================

Generate documentation directly
-------------------------------

A very simple program to render the API documentation as a mediawiki
document might look as follows.

```haskell
import Text.Pandoc
import Servant.Docs.Pandoc
import Servant.Docs
import Data.Default (def)

myApi :: Proxy MyAPI myApi = Proxy

writeDocs :: API -> IO ()
writeDocs api = writeFile "api.mw" (writeMediaWiki def (pandoc api))
```

Create a Pandoc filter
----------------------

The `makeFilter` function allows you to make a filter which can be
used directly with pandoc from the command line. This filter will just
append the API documentation to the end of the document. Example
usage:

```haskell
-- api.hs
main :: IO ()
main = makeFilter (docs myApi)
```

Then to run this:

```sh
pandoc -o api.pdf --filter=api.hs manual.md
```

### Custom filters

A more sophisticated filter might be to actually convert introduction
and note bodies to Markdown before processing (note: this is not
enabled by default as the `pandoc` library is GPL-licensed, whereas
this library uses `pandoc-types` which is BSD3-licensed):

```haskell
import Data.Monoid         (mconcat, (<>))
import Servant.Docs.Pandoc (pandoc)
import Text.Pandoc         (readMarkdown)
import Text.Pandoc.JSON    (Block(Para, Plain), Inline(Str), Pandoc(Pandoc),
                            toJSONFilter)
import Text.Pandoc.Options (def)
import Text.Pandoc.Walk    (walkM)

main :: IO ()
main = toJSONFilter append
  where
    append :: Pandoc -> Pandoc
    append = (<> mconcat (walkM parseMarkdown (pandoc myApi)))

parseMarkdown :: Block -> [Block]
parseMarkdown bl = case bl of
                     Para  [Str str] -> toMarkdown str
                     Plain [Str str] -> toMarkdown str
                     _               -> [bl]
  where
    toMarkdown = either (const [bl]) unPandoc . readMarkdown def

    unPandoc (Pandoc _ bls) = bls
```
