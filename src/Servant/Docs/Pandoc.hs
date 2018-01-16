{-# LANGUAGE OverloadedStrings #-}
----------------------------------
-- | There are two ways in which to use this module.
--
-- The first is to use the renderer directly with the pandoc API.  A
-- very simple program to render the API documentation as a mediawiki
-- document might look as follows.
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
--
-- A more sophisticated filter can be used to actually convert
-- introduction and note bodies into Markdown for pandoc to be able to
-- process:
--
-- > import Data.Monoid         (mconcat, (<>))
-- > import Servant.Docs.Pandoc (pandoc)
-- > import Text.Pandoc         (readMarkdown)
-- > import Text.Pandoc.JSON    (Block(Para, Plain), Inline(Str), Pandoc(Pandoc),
-- >                             toJSONFilter)
-- > import Text.Pandoc.Options (def)
-- > import Text.Pandoc.Walk    (walkM)
-- >
-- > main :: IO ()
-- > main = toJSONFilter append
-- >   where
-- >     append :: Pandoc -> Pandoc
-- >     append = (<> mconcat (walkM parseMarkdown (pandoc myApi)))
-- >
-- > parseMarkdown :: Block -> [Block]
-- > parseMarkdown bl = case bl of
-- >                      Para  [Str str] -> toMarkdown str
-- >                      Plain [Str str] -> toMarkdown str
-- >                      _               -> [bl]
-- >   where
-- >     toMarkdown = either (const [bl]) unPandoc . readMarkdown def
-- >
-- >     unPandoc (Pandoc _ bls) = bls
module Servant.Docs.Pandoc (pandoc, makeFilter) where

import Servant.Docs          (API, Action, DocCapture, DocNote, DocQueryParam,
                              Endpoint, ParamKind(Flag, List), Response,
                              apiEndpoints, apiIntros, capDesc, capSymbol,
                              captures, headers, introBody, introTitle, method,
                              noteBody, noteTitle, notes, paramDesc, paramKind,
                              paramName, paramValues, params, path, respBody,
                              respStatus, respTypes, response, rqbody, rqtypes)
import Servant.Docs.Internal (DocAuthentication, authDataRequired, authInfo,
                              authIntro)

import           Control.Lens               (mapped, view, (%~), (^.))
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.CaseInsensitive       (foldedCase)
import           Data.Foldable              (fold, foldMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (sort)
import           Data.List.NonEmpty         (NonEmpty, groupWith)
import qualified Data.List.NonEmpty         as NE
import           Data.Monoid                (mconcat, mempty, (<>))
import           Data.String.Conversions    (convertString)
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as T
import           Network.HTTP.Media         (MediaType)
import qualified Network.HTTP.Media         as M

import           Text.Pandoc.Builder    (Blocks, Inlines)
import qualified Text.Pandoc.Builder    as B
import           Text.Pandoc.Definition (Pandoc)
import           Text.Pandoc.JSON       (toJSONFilter)

--------------------------------------------------------------------------------

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

-- | Define these values for consistency rather than magic numbers.
topLevel, endpointLevel, sectionLevel :: Int
topLevel      = 1
endpointLevel = topLevel      + 1
sectionLevel  = endpointLevel + 1

-- | Generate a `Pandoc` representation of a given
-- `API`.
pandoc :: API -> Pandoc
pandoc api = B.doc $ intros <> mconcat endpoints

  where printEndpoint :: Endpoint -> Action -> Blocks
        printEndpoint endpoint action = mconcat
          [ B.header endpointLevel hdrStr
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
            hdrStr = mconcat [ B.str (convertString (endpoint ^. method))
                             , B.space
                             , B.code (showPath (endpoint ^. path))
                             ]

        intros = if null (api ^. apiIntros) then mempty else intros'
        intros' = foldMap printIntro (api ^. apiIntros)
        printIntro i =
          B.header topLevel (B.str $ i ^. introTitle) <>
          paraStr (i ^. introBody)
        endpoints = map (uncurry printEndpoint) . sort . HM.toList $ api ^. apiEndpoints

        notesStr :: [DocNote] -> Blocks
        notesStr = foldMap noteStr

        noteStr :: DocNote -> Blocks
        noteStr nt = B.header sectionLevel (B.text (nt ^. noteTitle)) <> paraStr (nt ^. noteBody)

        authStr :: [DocAuthentication] -> Blocks
        authStr []    = mempty
        authStr auths = mconcat
          [ B.header sectionLevel "Authentication"
          , paraStr (mapped %~ view authIntro $ auths)
          , B.para "Clients must supply the following data"
          , B.bulletList (map (B.plain . B.str) (mapped %~ view authDataRequired $ auths))
          ]

        capturesStr :: [DocCapture] -> Blocks
        capturesStr [] = mempty
        capturesStr l =
          B.header sectionLevel "Captures" <>
          B.bulletList (map captureStr l)

        captureStr cap =
          B.plain $ B.emph (B.str $ cap ^. capSymbol) <> ":" <> B.space <> B.text (cap ^. capDesc)

        headersStr :: [Text] -> Blocks
        headersStr [] = mempty
        headersStr l =  B.header sectionLevel "Headers" <> B.bulletList (map (B.para . headerStr) l)

          where headerStr hname = "This endpoint is sensitive to the value of the" <> B.space <>
                                    (B.strong . B.str $ unpack hname) <> B.space <> "HTTP header."

        paramsStr :: [DocQueryParam] -> Blocks
        paramsStr [] = mempty
        paramsStr l =
          B.header sectionLevel "Query Parameters" <>
          B.bulletList (map paramStr l)

        paramStr :: DocQueryParam -> Blocks
        paramStr param =
            B.plain (B.str (param ^. paramName)) <>
              B.definitionList (
                [(B.strong "Values",
                    [B.plain (B.emph
                      (foldr1 (\a b -> a <> "," <> B.space <> b) (map B.str values)))])
                | not (null values) || param ^. paramKind /= Flag]
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
          B.header sectionLevel "Request Body" <>
          B.bulletList (formatTypes types : formatBodies bs)

        formatTypes [] = mempty
        formatTypes ts = mconcat
                           [ B.plain "Supported content types are:"
                           , B.bulletList (map (B.plain . B.code . show) ts)
                           ]

        -- This assumes that when the bodies are created, identical
        -- labels and representations are located next to each other.
        formatBodies :: [(Text, M.MediaType, ByteString)] -> [Blocks]
        formatBodies bds = map formatBody bodyGroups
          where
            bodyGroups :: [(Text, NonEmpty M.MediaType, ByteString)]
            bodyGroups =
              map (\grps -> let (t,_,b) = NE.head grps in (t, fmap (\(_,m,_) -> m) grps, b))
              . groupWith (\(t,_,b) -> (t,b))
              $ bds

        formatBody :: (Text, NonEmpty M.MediaType, ByteString) -> Blocks
        formatBody (t, medias, b) = mconcat
                                      [ B.para . mconcat $
                                        [ title
                                        , " ("
                                        , mediaList medias
                                        , "): "
                                        ]
                                      , codeStr media b
                                      ]
          where
            mediaList = fold . NE.intersperse ", " . fmap (B.code . show)

            media = NE.head medias

            title
              | T.null t  = "Example"
              | otherwise = B.text (convertString t)

        codeStr :: MediaType -> ByteString -> Blocks
        codeStr media b =
          B.codeBlockWith ("",[markdownForType media],[]) (B.unpack b)

        responseStr :: Response -> Blocks
        responseStr resp =
          B.header sectionLevel "Response"  <>
          B.bulletList (
            B.plain ("Status code" <> B.space <> (B.str . show) (resp ^. respStatus)) :
            formatTypes (resp ^. respTypes) :
            case resp ^. respBody of
              [] -> [B.plain "No response body"]
              [("", t, r)] -> [B.plain "Response body as below.", codeStr t r]
              xs -> formatBodies xs)

        -- Pandoc has a wide range of syntax highlighting available,
        -- many (all?) of which seem to correspond to the sub-type of
        -- their corresponding media type.
        markdownForType :: MediaType -> String
        markdownForType mt =
          case M.subType mt of
            "x-www-form-urlencoded" -> "html"
            t                       -> convertString (foldedCase t)

paraStr :: [String] -> Blocks
paraStr = foldMap (B.para . B.str)

-- Duplicate of Servant.Docs.Internal
showPath :: [String] -> String
showPath [] = "/"
showPath ps = concatMap ('/':) ps
