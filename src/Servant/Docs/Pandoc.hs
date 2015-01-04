{-# LANGUAGE OverloadedStrings #-}
----------------------------------
-- | There are two ways in which to use this module.
--
-- The first is to use the renderer directly with the pandoc API.
-- A very simple
-- program to render the API documentation as a mediawiki document might look as
-- follows.
--
-- > import Text.Pandoc
-- > import Servant.Docs.Pandoc
-- > import Servant.Docs
-- > import Data.Default (def)
-- >
-- > myApi :: Proxy MyAPI
-- > myApi = Proxy
-- >
-- > writeDocs :: API -> IO ()
-- > writeDocs api = writeFile "api.mw" (writeMediaWiki def (pandoc api))
--
-- The second approach is to use `makeFilter` to make a filter which can be
-- used directly with pandoc from the command line. This filter will just
-- append the API documentation to the end of the document.
-- Example usage
--
-- > -- api.hs
-- > main :: IO ()
-- > main = makeFilter (docs myApi)
--
-- >> pandoc -o api.pdf --filter=api.hs manual.md
module Servant.Docs.Pandoc (pandoc, makeFilter) where

import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Blocks, Inlines)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.JSON (toJSONFilter)
import Servant.Docs
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, unpack)
import Data.List (intercalate)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B (unpack)


-- | Helper function which can be used to make a pandoc filter which
-- appends the generate docs to the end of the document.
--
-- This function is exposed for convenience. More experienced authors can
-- of course define a more complicated filter to inject the API
-- documentation.
makeFilter :: API -> IO ()
makeFilter api = toJSONFilter inject
  where
    inject :: Pandoc -> Pandoc
    inject p = p <> pandoc api


-- | Generate a `Pandoc` representation of a given
-- `API`.
pandoc :: API -> Pandoc
pandoc = B.doc . mconcat . map (uncurry printEndpoint) . HM.toList

  where printEndpoint :: Endpoint -> Action -> Blocks
        printEndpoint endpoint action =
          B.header 1 str <>
          capturesStr (action ^. captures) <>
          headersStr (action ^. headers) <>
          paramsStr (action ^. params) <>
          rqbodyStr (action ^. rqbody) <>
          responseStr (action ^. response)

          where str :: Inlines
                str = B.str (show (endpoint^.method)) <> B.space <> B.code (intercalate "/" (endpoint ^. path))

        capturesStr :: [DocCapture] -> Blocks
        capturesStr [] = mempty
        capturesStr l =
          B.header 2 "Captures" <>
          B.bulletList (map captureStr l)
        captureStr cap =
          B.plain $ B.emph (B.doubleQuoted . B.str$ (cap ^. capSymbol)) <>  ":" <> B.space <>  B.str (cap ^. capDesc)

        headersStr :: [Text] -> Blocks
        headersStr [] = mempty
        headersStr l =  B.bulletList (map (B.para . headerStr) l)

          where headerStr hname = "This endpoint is sensitive to the value of the" <> B.space <>
                                    (B.strong . B.str  $ unpack hname) <> B.space <> "HTTP header."

        paramsStr :: [DocQueryParam] -> Blocks
        paramsStr [] = mempty
        paramsStr l =
          B.header 2 "GET Parameters" <>
          B.bulletList (map paramStr l)

        paramStr param =
            B.plain (B.str (param ^. paramName)) <>
              B.definitionList (
                [(B.strong "Values",
                    [B.plain (B.emph
                      (foldr1 (\a b -> a <> B.str "," <> B.space <> b) (map B.str values)))])
                | not (null values) || param ^. paramKind /= Flag]
                ++
              [(B.strong "Description",
                  [B.plain $ B.str (param ^. paramDesc)])])
              <>
              B.bulletList (
                [B.plain $ "This parameter is a" <>
                         B.space <>
                         B.strong "list" <>
                         ". All GET parameters with the name" <>
                         B.space <>
                         B.str (param ^. paramName) <>
                         B.space <>
                         B.code "[]" <> B.space <>
                         "will forward their values in a list to the handler."
                         | param ^. paramKind == List]
                ++
              [B.plain $ "This parameter is a" <>
                          B.space <>
                          B.strong "flag." <>
                          B.space <>
                          "This means no value is expected to be associated to this parameter."
              | param ^. paramKind == Flag]
          )


          where values = param ^. paramValues

        rqbodyStr :: Maybe ByteString -> Blocks
        rqbodyStr Nothing = mempty
        rqbodyStr (Just b) =
          B.header 2 "Request Body" <>
          jsonStr b

        jsonStr :: ByteString -> Blocks
        jsonStr b =
          B.codeBlockWith ("",["javascript"],[]) (B.unpack b)

        responseStr :: Response -> Blocks
        responseStr resp =
          B.header 2 "Response"  <>
          B.bulletList (
            [B.plain $ "Status code" <> B.space <> (B.str . show) (resp ^. respStatus)]
              ++
            case resp ^. respBody of
              [] -> [B.plain "No response body"]
              xs -> map renderResponse xs)
          where
            renderResponse ("", r) =
              B.plain "Response body at below" <> jsonStr r
            renderResponse (ctx, r) =
              B.plain (B.str . unpack $ ctx) <> jsonStr r

