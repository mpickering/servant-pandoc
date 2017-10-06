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

import           Control.Lens               (mapped, view, (%~), (^.))
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B (unpack)
import           Data.CaseInsensitive       (foldedCase)
import           Data.Foldable              (foldMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (sort)
import           Data.Monoid                (mconcat, mempty, (<>))
import           Data.String.Conversions    (convertString)
import           Data.Text                  (Text, unpack)
import           Network.HTTP.Media         (MediaType)
import qualified Network.HTTP.Media         as M
import           Servant.Docs
import           Servant.Docs.Internal      (DocAuthentication,
                                             authDataRequired, authInfo,
                                             authIntro)
import           Text.Pandoc.Builder        (Blocks, Inlines)
import qualified Text.Pandoc.Builder        as B
import           Text.Pandoc.Definition     (Pandoc)
import           Text.Pandoc.JSON           (toJSONFilter)

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
pandoc api = B.doc $ intros <> mconcat endpoints

  where printEndpoint :: Endpoint -> Action -> Blocks
        printEndpoint endpoint action = mconcat
          [ B.header 1 hdrStr
          , notesStr    (action ^. notes)
          , authStr     (action ^. authInfo)
          , capturesStr (action ^. captures)
          , headersStr  (action ^. headers)
          , paramsStr   (action ^. params)
          , rqbodyStrs  (action ^. rqtypes) (action ^. rqbody)
          , responseStr (action ^. response)
          ]
          where
            hdrStr :: Inlines
            hdrStr = mconcat [ strShow (endpoint ^. method)
                             , B.space
                             , B.code (showPath (endpoint ^. path))
                             ]

        intros = if null (api ^. apiIntros) then mempty else intros'
        intros' = foldMap printIntro (api ^. apiIntros)
        printIntro i =
          B.header 1 (B.str $ i ^. introTitle) <>
          foldMap (B.para . B.text) (i ^. introBody)
        endpoints = map (uncurry printEndpoint) . sort . HM.toList $ api ^. apiEndpoints

        notesStr :: [DocNote] -> Blocks
        notesStr = foldMap noteStr

        noteStr :: DocNote -> Blocks
        noteStr nt = B.header 2 (B.str (nt ^. noteTitle)) <> paraText (nt ^. noteBody)

        authStr :: [DocAuthentication] -> Blocks
        authStr []    = mempty
        authStr auths = mconcat
          [ B.header 2 "Authentication"
          , paraText (mapped %~ view authIntro $ auths)
          , B.para "Clients must supply the following data"
          , B.bulletList (map (B.plain . B.text) (mapped %~ view authDataRequired $ auths))
          ]

        capturesStr :: [DocCapture] -> Blocks
        capturesStr [] = mempty
        capturesStr l =
          B.header 2 "Captures" <>
          B.bulletList (map captureStr l)

        captureStr cap =
          B.plain $ B.emph (B.str $ cap ^. capSymbol) <> ":" <> B.space <> B.text (cap ^. capDesc)

        headersStr :: [Text] -> Blocks
        headersStr [] = mempty
        headersStr l =  B.header 2 "Headers" <> B.bulletList (map (B.para . headerStr) l)

          where headerStr hname = "This endpoint is sensitive to the value of the" <> B.space <>
                                    (B.strong . B.str $ unpack hname) <> B.space <> "HTTP header."

        paramsStr :: [DocQueryParam] -> Blocks
        paramsStr [] = mempty
        paramsStr l =
          B.header 2 "Query Parameters" <>
          B.bulletList (map paramStr l)

        paramStr :: DocQueryParam -> Blocks
        paramStr param =
            B.plain (B.str (param ^. paramName)) <>
              B.definitionList (
                [(B.strong "Values",
                    [B.plain (B.emph
                      (foldr1 (\a b -> a <> "," <> B.space <> b) (map B.str values)))])
                | not (null values), param ^. paramKind /= Flag]
                ++
              [(B.strong "Description",
                  [B.plain $ B.str (param ^. paramDesc)])])
              <>
              B.bulletList (
                [B.plain $ "This parameter is a" <>
                         B.space <>
                         B.strong "list" <>
                         ". All query parameters with the name" <>
                         B.space <>
                         B.str (param ^. paramName) <>
                         B.space <>
                         B.code "[]" <> B.space <>
                         "will forward their values in a list to the handler."
                         | param ^. paramKind == List]
                ++
              [B.plain $ "This parameter is a" <>
                          B.space <>
                          B.strong "flag" <>
                          ". This means no value is expected to be associated to this parameter."
              | param ^. paramKind == Flag]
          )
          where values = param ^. paramValues

        rqbodyStrs :: [MediaType] -> [(Text, MediaType, ByteString)] -> Blocks
        rqbodyStrs [] [] = mempty
        rqbodyStrs types bs =
          B.header 1 "Request Body" <>
          formatTypes types <>
          B.bulletList (map bodyStr bs)

        formatTypes [] = mempty
        formatTypes ts = B.bulletList
                           [ B.plain "Supported content types are:"
                           , B.bulletList (map (B.plain . B.code . show) ts)
                           ]

        bodyStr :: (Text, MediaType, ByteString) -> Blocks
        bodyStr (t, media, bs) = mconcat
                                   [ B.plain . mconcat $
                                     [ "Example ("
                                     , B.text (convertString t)
                                     , "): "
                                     , B.code (show media)
                                     ]
                                   , codeStr media bs
                                   ]

        codeStr :: MediaType -> ByteString -> Blocks
        codeStr media b =
          B.codeBlockWith ("",[markdownForType media],[]) (B.unpack b)

        responseStr :: Response -> Blocks
        responseStr resp =
          B.header 1 "Response"  <>
          B.bulletList (
            (B.plain $ "Status code" <> B.space <> (B.str . show) (resp ^. respStatus)) :
            formatTypes (resp ^. respTypes) :
            case resp ^. respBody of
              [] -> [B.plain "No response body"]
              [("", t, r)] -> [B.plain "Response body as below.", codeStr t r]
              xs -> concatMap renderResponse xs)
          where
            renderResponse :: (Text, MediaType, ByteString) -> [Blocks]
            renderResponse (ctx, t, r) = [B.plain (B.str (convertString ctx)), codeStr t r]

        -- Pandoc has a wide range of syntax highlighting available,
        -- many (all?) of which seem to correspond to the sub-type of
        -- their corresponding media type.
        markdownForType :: MediaType -> String
        markdownForType mt =
          case M.subType mt of
            "x-www-form-urlencoded" -> "html"
            t                       -> convertString (foldedCase t)

strShow :: (Show a) => a -> Inlines
strShow = B.str . show

paraText :: [String] -> Blocks
paraText = foldMap (B.para . B.text)

-- Duplicate of Servant.Docs.Internal
showPath :: [String] -> String
showPath [] = "/"
showPath ps = concatMap ('/':) ps
