{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Network.Gitit2.GititToc ( tableOfContents
                               , GititToc
                               , stripElementsForToc
                               )
where

import qualified Data.Aeson as ASON
import qualified Text.Blaze.XHtml1.Transitional as H
import qualified Text.Blaze.XHtml1.Transitional.Attributes as A
import Text.Pandoc
import Text.Pandoc.Shared (Element(..))
import Control.Monad.State
import qualified Text.Pandoc.Writers.HTML as PWH (WriterState, inlineListToHtml, unordList)
import Text.Blaze.Html hiding (contents)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.Monoid (mempty)
import Data.Typeable
import Data.Data
import GHC.Generics
import Network.URI ( unEscapeString )


-- data type equivalent to Element where only links inside blocks have been kept
data GititToc = GititLink [Inline] Target
              | GititSec Int [Int] Text.Pandoc.Attr [Inline] [GititToc]
                --    lvl  num attributes label    contents
                deriving (Eq, Read, Show, Typeable, Data, Generic)

instance ASON.FromJSON GititToc
instance ASON.ToJSON GititToc

stripElementsForToc :: [Element] -> [GititToc]
stripElementsForToc element = concat  $ fmap stripElementForToc element

stripElementForToc :: Element -> [GititToc]
stripElementForToc (Sec lev num attr headerText subsecs) =
    [GititSec lev num attr headerText (concat $ fmap stripElementForToc subsecs)]
stripElementForToc (Blk block) = fmap (uncurry GititLink) (fetchLink block)

fetchLink :: Block -> [([Inline], Target)]
fetchLink = queryWith isLinkInPar
    where isLinkInPar (Para [Link inlines target]) = [(inlines, target)]
          isLinkInPar _ = []

tableOfContentsAbs :: (WriterOptions -> a -> State PWH.WriterState (Maybe Html))
                   -> WriterOptions
                   -> [a]
                   -> State PWH.WriterState (Maybe Html)
tableOfContentsAbs _ _ [] = return Nothing
tableOfContentsAbs writer opts sects = do
  let opts'        = opts { writerIgnoreNotes = True }
  contents  <- mapM (writer opts') sects
  let tocList = catMaybes contents
  return $ if null tocList
              then Nothing
              else Just $ PWH.unordList opts tocList

tableOfContents :: WriterOptions -> [GititToc] -> State PWH.WriterState (Maybe Html)
tableOfContents = tableOfContentsAbs gititTocToListItem

-- | Convert section number to string
showSecNum :: [Int] -> String
showSecNum = intercalate "." . map show

gititTocToListItem :: WriterOptions -> GititToc -> State PWH.WriterState (Maybe Html)
-- Don't include the empty headers created in slide shows
-- shows when an hrule is used to separate slides without a new title:
gititTocToListItem _ (GititSec _ _ _ [Str "\0"] _) = return Nothing

gititTocToListItem opts (GititSec lev num (id',classes,_) headerText subsecs)
  | lev <= writerTOCDepth opts = do
  let num' = zipWith (+) num (writerNumberOffset opts ++ repeat 0)
  let sectnum = if writerNumberSections opts && not (null num) &&
                   "unnumbered" `notElem` classes
                   then (H.span ! A.class_ "toc-section-number"
                        $ toHtml $ showSecNum num') >> preEscapedToHtml (" " :: String)
                   else mempty
  txt <- liftM (sectnum >>) $ PWH.inlineListToHtml opts headerText
  subHeads <- liftM catMaybes (mapM (gititTocToListItem opts) subsecs)
  let subList = if null subHeads
                   then mempty
                   else PWH.unordList opts subHeads
  -- in reveal.js, we need #/apples, not #apples:
  let revealSlash = ['/' | writerSlideVariant opts == RevealJsSlides]
  return $ Just
         $ if null id'
              then H.a (toHtml txt) >> subList
              else (H.a ! A.href (toValue $ "#" ++ revealSlash ++
                    writerIdentifierPrefix opts ++ id')
                       $ toHtml txt) >> subList

gititTocToListItem opts (GititLink txt (s, tit)) = do
                        linkText <- PWH.inlineListToHtml opts txt
                        let s' = case s of
                                      '#':xs | writerSlideVariant opts ==
                                            RevealJsSlides -> '#':'/':xs
                                      _ -> s
                        let link = H.a ! A.href (toValue s') $ linkText
                        let link' = if txt == [Str (unEscapeString s)]
                                       then link ! A.class_ "uri"
                                       else link
                        return $ Just $ if null tit
                                    then link'
                                    else link' ! A.title (toValue tit)

gititTocToListItem _ _ = return Nothing
