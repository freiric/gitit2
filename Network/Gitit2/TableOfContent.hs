module Network.Gitit2.TableOfContent
    ( extractToc
    , tocPlugin
    ) where

import           Control.Monad (liftM)
import           Control.Monad.State (evalStateT, StateT)
import           Data.FileStore (RevisionId)
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Network.Gitit2.Import
import           Network.Gitit2.Page (pageToText, textToPage)
import           Network.Gitit2.WikiPage (GititToc (..), WikiPage(..))
import           Network.URI (unEscapeString)
import           Text.Blaze.Html hiding (contents)
import qualified Text.Blaze.XHtml1.Transitional as H
import qualified Text.Blaze.XHtml1.Transitional.Attributes as A
import           Text.Pandoc
import           Text.Pandoc.Shared (Element(..), hierarchicalize) --, inDirectory, readDataFileUTF8, stringify)
import qualified Text.Pandoc.Writers.HTML as PWH (defaultWriterState, inlineListToHtml, unordList, WriterState)

extractTocAbs :: HasGitit master
              => Maybe Int
              -> ((Page -> Maybe RevisionId
                        -> GH master (Maybe ([a], [Text], Html)))
                  -> WriterOptions
                  -> Text
                  -> [a]
                  -> StateT PWH.WriterState (GH master) (Maybe Html))
              -> Text
              -> (Page -> Maybe RevisionId
                       -> GH master (Maybe ([a], [Text], Html)))
              -> [a]
              -> GH master (Maybe (WidgetT master IO ()))
extractTocAbs mbTocDepth tocFun prefix wikifyAndCache tocHierrarchy = do
  let opts = def { writerWrapText = WrapNone
                 , writerHtml5 = True
                 , writerHighlight = True
                 , writerHTMLMathMethod = MathML Nothing
                 }
  let tocFunS = tocFun wikifyAndCache
                       (maybe opts
                        (\tocDepth -> opts { writerTOCDepth = tocDepth})
                        mbTocDepth)
                prefix tocHierrarchy
  mbTocRendered <- evalStateT tocFunS PWH.defaultWriterState
  case mbTocRendered of
    Just tocRendered -> return $ Just $ toWidget tocRendered
    -- page has no titles and hence no toc
    Nothing -> return Nothing

extractToc :: HasGitit master
           => Maybe Int
           -> Text
           -> (Page -> Maybe RevisionId
                    -> GH master (Maybe ([GititToc], [Text], Html)))
           -> [GititToc]
           -> GH master (Maybe (WidgetT master IO ()))
extractToc mbTocDepth = extractTocAbs mbTocDepth tableOfContents

-- | Extends an HTML writer from a single GititToc to [GititToc]
tableOfContentsAbs :: (WriterOptions
                           -> Text
                           -> (Page -> Maybe RevisionId
                                    -> GH master (Maybe ([a], [Text], Html)))
                           -> a
                           -> StateT PWH.WriterState (GH master) (Maybe Html))
                   -> (Page -> Maybe RevisionId
                            -> GH master (Maybe ([a], [Text], Html)))
                   -> WriterOptions
                   -> Text
                   -> [a]
                   -> StateT PWH.WriterState (GH master) (Maybe Html)
tableOfContentsAbs _ _ _ _ [] = return Nothing
tableOfContentsAbs writer wikifyAndCache opts prefix sects = do
  contents  <- mapM (writer opts { writerIgnoreNotes = True } prefix wikifyAndCache) sects
  return $ case catMaybes contents of
             [] -> Nothing
             tocList -> Just $ PWH.unordList opts tocList

-- | HTML writer from [GititToc] to HTML
tableOfContents :: HasGitit master
                => (Page -> Maybe RevisionId
                         -> GH master (Maybe ([GititToc], [Text], Html)))
                -> WriterOptions
                -> Text
                -> [GititToc]
                -> StateT PWH.WriterState (GH master) (Maybe Html)
tableOfContents = tableOfContentsAbs gititTocToListItem

-- | Convert section number to string
showSecNum :: [Int] -> String
showSecNum = intercalate "." . map show

-- | HTML writer from a single GititToc to HTML
gititTocToListItem :: HasGitit master
                   => WriterOptions
                   -> Text
                   -> (Page -> Maybe RevisionId
                            -> GH master (Maybe ([GititToc], [Text], Html)))
                   -> GititToc
                   -> StateT PWH.WriterState (GH master) (Maybe Html)
-- Don't include the empty headers created in slide shows
-- shows when an hrule is used to separate slides without a new title:
gititTocToListItem _ _ _ (GititSec _ _ _ [Str "\0"] _) = return Nothing

gititTocToListItem opts prefix wikifyAndCache (GititSec lev num (id',classes,_) headerText subsecs)
  | lev <= writerTOCDepth opts = do
  let num' = zipWith (+) num (writerNumberOffset opts ++ repeat 0)
  let sectnum = if writerNumberSections opts && not (null num) &&
                   "unnumbered" `notElem` classes
                   then (H.span ! A.class_ "toc-section-number"
                        $ toHtml $ showSecNum num') >> preEscapedToHtml (" " :: String)
                   else mempty
  txt <- liftM (sectnum >>) $ PWH.inlineListToHtml opts headerText
  subHeads <- liftM catMaybes (mapM (gititTocToListItem opts prefix wikifyAndCache) subsecs)
  let subList = if null subHeads
                   then mempty
                   else PWH.unordList opts subHeads
  -- in reveal.js, we need #/apples, not #apples:
  let revealSlash = ['/' | writerSlideVariant opts == RevealJsSlides]
  if null id'
              then return $ Just $ H.a (toHtml txt) >> subList
              else do
                  toMaster <- lift getRouteToParent
                  let route = ViewR $ textToPage prefix
                  toUrl <- lift $ lift getUrlRender
                  return $ Just $ (H.a ! A.href
                                          (toValue $ T.unpack (toUrl $ toMaster route)
                                           ++ "#" ++ revealSlash ++ writerIdentifierPrefix opts ++ id')
                                        $ toHtml txt) >> subList

gititTocToListItem opts _ wikifyAndCache (GititLink lev attr ref (s, tit))
  | lev <= writerTOCDepth opts = do
  let target = textToPage $ T.pack $ inlinesToString ref
  linkText <- PWH.inlineListToHtml opts ref
  let s' = case s of
             '#':xs | writerSlideVariant opts == RevealJsSlides -> '#':'/':xs
             _ -> s
  let link = H.a ! A.href (toValue s') $ linkText
      link' = if ref == [Str (unEscapeString s)]
                 then link ! A.class_ "uri"
                 else link
      link'' = if null tit
                  then link'
                  else link' ! A.title (toValue tit)
  mbTocAndPageHtml <- lift $ wikifyAndCache target Nothing
  case mbTocAndPageHtml of
    Just (tocs, _, _) -> do
                          mbToc <- tableOfContents wikifyAndCache
                                   opts { writerTOCDepth = writerTOCDepth opts - lev }
                                   (pageToText target) tocs
                          case mbToc of
                            Just toc -> return $ Just $
                                        let (ident, classes, _) = attr in
                                        H.div ! A.id (toValue ("toc-" ++ ident))
                                             ! A.class_ (toValue ("toc-" ++  head classes)) $
                                        link'' >> toc
                            -- referred page has no toc
                            Nothing -> return $ Just link''
    -- referred page does not exist or could not be parsed
    Nothing -> return $ Just link''

gititTocToListItem _ _ _ _ = return Nothing

-- stolen from gitit ContentTransfomers.hs
-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
               Str s                   -> s
               Emph xs                 -> concatMap go xs
               Strong xs               -> concatMap go xs
               Strikeout xs            -> concatMap go xs
               Superscript xs          -> concatMap go xs
               Subscript xs            -> concatMap go xs
               SmallCaps xs            -> concatMap go xs
               Quoted DoubleQuote xs   -> '"' : (concatMap go xs ++ "\"")
               Quoted SingleQuote xs   -> '\'' : (concatMap go xs ++ "'")
               Cite _ xs               -> concatMap go xs
               Code _ s                -> s
               Space                   -> " "
               LineBreak               -> " "
               Math DisplayMath s      -> "$$" ++ s ++ "$$"
               Math InlineMath s       -> "$" ++ s ++ "$"
               RawInline (Format "tex") s -> s
               RawInline _ _           -> ""
               Link _ xs _             -> concatMap go xs
               Image _ xs _            -> concatMap go xs
               Note _                  -> ""
               Span _ xs               -> concatMap go xs

-- | Keeps only sections and links from Elements
stripElementsForToc :: Bool -> Int -> [Element] -> [GititToc]
stripElementsForToc extToc lev elements =
    concat  $ fmap (stripElementForToc extToc  lev) elements

stripElementForToc :: Bool -> Int -> Element -> [GititToc]
stripElementForToc extToc _ (Sec lev num attr headerText subsecs) =
    [GititSec lev num attr headerText (stripElementsForToc extToc (lev + 1) subsecs)]
stripElementForToc False _ _  = []
stripElementForToc True lev (Blk block) =
    fmap (\(attr, inlines, target) -> (GititLink lev attr inlines target)) (fetchLink block)


fetchLink :: Block -> [(Text.Pandoc.Attr, [Inline], Target)]
fetchLink = queryWith isLinkInPar
    where isLinkInPar (Div (ident,["subpage-link"],[]) [Para [Link attr inlines target]])
              = [((ident,["subpage-link"],[]), inlines, target)]
          isLinkInPar _ = []

tocPlugin :: Plugin master
tocPlugin = Plugin {
              unPlugin = addIdClassAndExtractToc
            }
    where
      addIdClassAndExtractToc wp = do
        conf <- getConfig
        blocks <- mapM addIdAndClassToParWikiLinks (wpContent wp)
        let tocHierarchy = stripElementsForToc (extended_toc conf) 0 $ hierarchicalize blocks
        return $ wp { wpContent     = blocks
                    , wpTocHierarchy = tocHierarchy
                    }

addIdAndClassToParWikiLinks :: Block -> GH master Block
addIdAndClassToParWikiLinks (Para [Link attr inlines target]) = do
  ident <- newIdent
  return $ Div (T.unpack ident,["subpage-link"],[]) [Para [Link attr inlines target]]
addIdAndClassToParWikiLinks x = return x
