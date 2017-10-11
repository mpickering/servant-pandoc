There are two ways in which to use this module.

The first is to use the renderer directly with the pandoc API. A very
simple program to render the API documentation as a mediawiki document
might look as follows.

```haskell
import Text.Pandoc
import Servant.Docs.Pandoc
import Servant.Docs
import Data.Default (def)

myApi :: Proxy MyAPI myApi = Proxy

writeDocs :: API -> IO ()
writeDocs api = writeFile "api.mw" (writeMediaWiki def (pandoc api))
```

The second approach is to use `makeFilter` to make a filter which can
be used directly with pandoc from the command line. This filter will
just append the API documentation to the end of the document. Example
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
