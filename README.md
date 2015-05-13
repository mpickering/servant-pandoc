There are two ways in which to use this module.

The first is to use the renderer directly with the pandoc API. A very
simple program to render the API documentation as a mediawiki document
might look as follows.

```
 import Text.Pandoc import Servant.Docs.Pandoc import Servant.Docs
 import Data.Default (def)

 myApi :: Proxy MyAPI myApi = Proxy

 writeDocs :: API -\> IO () writeDocs api = writeFile "api.mw"
 (writeMediaWiki def (pandoc api))
```

The second approach is to use `makeFilter` to make a filter which can be
used directly with pandoc from the command line. This filter will just
append the API documentation to the end of the document. Example usage

```
-- api.hs
main :: IO ()
main = makeFilter (docs myApi)

> pandoc -o api.pdf --filter=api.hs manual.md
````
