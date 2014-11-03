{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Network.Gitit2.GititToc ( GititToc
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
import Network.Gitit2.Routes
import Data.Monoid (mappend)
import qualified Data.Text as T


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
    where isLinkInPar (Para [Link inlines ("!toc", _)]) = [(inlines, ("", ""))]
          isLinkInPar _ = []
