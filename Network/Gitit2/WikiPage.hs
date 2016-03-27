{-# LANGUAGE DeriveGeneric #-}
module Network.Gitit2.WikiPage
       (
         extractCategories,
         PageFormat(..),
         WikiPage (..),
         readPageFormat,
         contentToWikiPage',
         GititToc (..)
       )
       where

import qualified Data.Aeson as ASON
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml
import           GHC.Generics
import           Text.HTML.SanitizeXSS (sanitizeAttribute)
import           Text.Pandoc
import           Text.Pandoc.Builder (toList, text)
import           Text.Pandoc.Error (handleError)


-- | Data type equivalent to Element where only sections and single links in paragraph have been kept
data GititToc = GititLink Int Text.Pandoc.Attr [Inline] Target
                --        lvl attributes       label    link
              | GititSec Int [Int] Text.Pandoc.Attr [Inline] [GititToc]
                --       lvl num   attributes       label    contents
                deriving (Eq, Read, Show, Generic)

instance ASON.FromJSON GititToc
instance ASON.ToJSON GititToc

data WikiPage = WikiPage {
    wpName        :: Text
  , wpFormat      :: PageFormat
  , wpTOC         :: Bool
  , wpLHS         :: Bool
  , wpTitle       :: [Inline]
  , wpCategories  :: [Text]
  , wpMetadata    :: M.Map Text Value
  , wpCacheable   :: Bool
  , wpContent     :: [Block]
  , wpTocHierarchy :: [GititToc]
} deriving (Show)

extractCategories :: M.Map Text Value -> [Text]
extractCategories metadata =
  case M.lookup ("categories" :: Text) metadata of
       Just (String t) -> T.words $ T.replace "," " " t
       _               -> []

-- | The Boolean is True for literate Haskell.
data PageFormat = Markdown Bool
                | RST Bool
                | LaTeX Bool
                | HTML Bool
                | Textile Bool
                | Org Bool
                deriving (Read, Show, Eq )

readPageFormat :: Text -> Maybe PageFormat
readPageFormat s =
  case T.toLower s' of
       "markdown"  -> Just $ Markdown lhs
       "textile"   -> Just $ Textile lhs
       "latex"     -> Just $ LaTeX lhs
       "html"      -> Just $ HTML lhs
       "rst"       -> Just $ RST lhs
       "org"       -> Just $ Org lhs
       _           -> Nothing
 where (s',rest) = T.break (=='+') s
       lhs = rest == "+lhs"

contentToWikiPage' :: Text -> ByteString -> ([Inline] -> String) -> PageFormat -> Bool -> WikiPage
contentToWikiPage' title contents converter defaultFormat simpleTitle =
  WikiPage {
             wpName        = title
           , wpFormat      = format
           , wpTOC         = toc
           , wpLHS         = lhs
           , wpTitle       = toList $ text $ T.unpack $ title
           , wpCategories  = extractCategories metadata
           , wpMetadata    = metadata
           , wpCacheable   = True
           , wpContent     = blocks
           , wpTocHierarchy = []
           }
  where
    (h,b) = stripHeader $ B.lines contents
    metadata :: M.Map Text Value
    metadata = if B.null h
                  then M.empty
                  else fromMaybe M.empty
                       $ decode $! BS.concat $ B.toChunks h
    formatStr = case M.lookup "format" metadata of
                       Just (String s) -> s
                       _               -> ""
    format = fromMaybe defaultFormat $ readPageFormat formatStr
    readerOpts literate = def{ readerSmart = True
                             , readerExtensions =
                                 if literate
                                    then Set.insert Ext_literate_haskell pandocExtensions
                                    else pandocExtensions }
    (reader, lhs) = case format of
                      Markdown l -> (readMarkdown (readerOpts l), l)
                      Textile  l -> (readTextile (readerOpts l), l)
                      LaTeX    l -> (readLaTeX (readerOpts l), l)
                      RST      l -> (readRST (readerOpts l), l)
                      HTML     l -> (readHtml (readerOpts l), l)
                      Org      l -> (readOrg (readerOpts l), l)
    fromBool (Bool t) = t
    fromBool _        = False
    toc = maybe False fromBool (M.lookup "toc" metadata)
    doc = handleError . reader $ toString b
    Pandoc _ blocks = sanitizePandoc $ addWikiLinks doc
    
    convertWikiLinks :: Inline -> Inline
    convertWikiLinks (Link attr ref ("", "")) = Link attr (linkTitle ref) (converter ref, "")
    convertWikiLinks (Image attr ref ("", "")) = Image attr ref (converter ref, "")
    convertWikiLinks x = x

    linkTitle [Str refStr] | simpleTitle = [Str $ T.unpack $ last . T.splitOn "/" $ T.pack refStr]
    linkTitle x = x

    addWikiLinks :: Pandoc -> Pandoc
    addWikiLinks = bottomUp (convertWikiLinks)

    stripHeader :: [ByteString] -> (ByteString,ByteString)
    stripHeader (x:xs)
      | isHeaderStart x = let (hs, bs) = break isHeaderEnd xs
                          in  case bs of
                                 []     -> (B.unlines (x:xs), B.empty)
                                 (_:ys) -> (B.unlines hs, B.unlines ys)
      | otherwise = (B.empty, B.unlines (x:xs))
     where isHeaderStart z = ["---"] == B.words z
           isHeaderEnd   z = ["..."] == B.words z
    stripHeader [] = (B.empty, B.empty)

    sanitizePandoc :: Pandoc -> Pandoc
    sanitizePandoc = bottomUp sanitizeBlock . bottomUp sanitizeInline
      where
        sanitizeBlock (RawBlock _ _) = Text.Pandoc.Null
        sanitizeBlock (CodeBlock (id',classes,attrs) x) =
          CodeBlock (id', classes, sanitizeAttrs attrs) x
        sanitizeBlock x = x
        sanitizeInline (RawInline _ _) = Str ""
        sanitizeInline (Code (id',classes,attrs) x) =
          Code (id', classes, sanitizeAttrs attrs) x
        sanitizeInline (Link attr lab (src,tit)) = Link attr lab (sanitizeURI src,tit)
        sanitizeInline (Image attr alt (src,tit)) = Image attr alt (sanitizeURI src,tit)
        sanitizeInline x = x
        sanitizeURI src = case sanitizeAttribute ("href", T.pack src) of
                               Just (_,z) -> T.unpack z
                               Nothing    -> ""
        sanitizeAttrs = mapMaybe sanitizeAttr
        sanitizeAttr (x,y) = case sanitizeAttribute (T.pack x, T.pack y) of
                                  Just (w,z) -> Just (T.unpack w, T.unpack z)
                                  Nothing    -> Nothing
