module Network.Gitit2.Cache ( cacheJSON
                            , tryPageCache
                            , tryJSONCache
                            , tryCache
                            , caching
                            , expireCache ) where

import           Blaze.ByteString.Builder (toLazyByteString)
import           Control.Applicative ((<$>))
import           Control.Monad (filterM, unless, void, when)
import qualified Data.Aeson as ASON
import           Data.ByteString.Lazy (hGetContents)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text.IO as TIO
import           Data.Time (diffUTCTime, getCurrentTime)
import           Network.Gitit2.Foundation (GH, cache_dir, feed_minutes, use_cache)
import           Network.Gitit2.Helper (getConfig)
import           Network.HTTP (urlDecode, urlEncode)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                   getDirectoryContents, getModificationTime, removeDirectoryRecursive)
import           System.FilePath ((</>), takeDirectory)
import           System.IO (withFile, IOMode(..))
import           Text.Blaze.Html hiding (contents)
import           Yesod (Content(ContentBuilder), liftIO, sendFile, toTypedContent, ToTypedContent, TypedContent(TypedContent))

cacheJSON :: ASON.ToJSON a => FilePath -> a -> GH master ()
cacheJSON cachepath a = do
  conf <- getConfig
  when (use_cache conf) $ liftIO $ do
                          let fullpath = cache_dir conf </> cachepath
                          createDirectoryIfMissing True $ takeDirectory fullpath
                          B.writeFile fullpath (ASON.encode a)

tryPageCache :: FilePath -> GH master (Maybe Html)
tryPageCache  = processDirCache
                (\ fullpath x -> do
                   pageString <- liftIO $ TIO.readFile $ fullpath </> x
                   return $ Just $ preEscapedToHtml pageString)

tryJSONCache :: ASON.FromJSON a => FilePath -> GH master (Maybe [a])
tryJSONCache = processDirCache
                 (\ fullpath x -> liftIO $
                     withFile (fullpath </> x) ReadMode $ \hnd -> do
                       pageString <- hGetContents hnd
                       return $ ASON.decode pageString)

tryCache :: FilePath -> GH master ()
tryCache = void .
           processDirCache
           (\ fullpath x -> do
              let ct = BSU.fromString $ urlDecode x
              sendFile ct $ fullpath </> x
              return Nothing)

processDirCache :: (FilePath ->FilePath -> GH master (Maybe a))
                -> FilePath
                -> GH master (Maybe a)
processDirCache process path = do
  conf <- getConfig
  if use_cache conf
     then do
       let fullpath = cache_dir conf </> path
       exists <- liftIO $ doesDirectoryExist fullpath
       if exists
          then (do
            files <- liftIO $ getDirectoryContents fullpath >>=
                               filterM (doesFileExist . (fullpath </>))
            case files of
                 (x:_) -> process fullpath x
                 _     -> return Nothing)
           else return Nothing
     else return Nothing

caching :: ToTypedContent a
        => FilePath -> GH master a -> GH master a
caching path handler = do
  conf <- getConfig
  if use_cache conf
     then do
       result <- handler
       cacheContent path $ toTypedContent result
       return result
     else handler

cacheContent :: FilePath -> TypedContent -> GH master ()
cacheContent path (TypedContent ct content) = do
  conf <- getConfig
  when (use_cache conf) $
       case content of
            ContentBuilder builder _ -> liftIO $ do
              let fullpath = cache_dir conf </> path </> urlEncode (BSU.toString ct)
              createDirectoryIfMissing True $ takeDirectory fullpath
              B.writeFile fullpath $ toLazyByteString builder
            _ -> liftIO $
              -- TODO replace w logging
              putStrLn $ "Can't cache " ++ path

expireCache :: FilePath -> GH master ()
expireCache path = do
  conf <- getConfig
  expireFeed (feed_minutes conf) (path </> "_feed")
  expireFeed (feed_minutes conf) "_feed"
  expireCategories
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> path
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ removeDirectoryRecursive fullpath

expireCategories :: GH master ()
expireCategories = do
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> "_categories"
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ removeDirectoryRecursive fullpath

-- | Expire the cached feed unless it is younger than 'minutes' old.
expireFeed :: Integer -> FilePath -> GH master ()
expireFeed minutes path = do
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> path
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ do
      seconds <- getModificationTime fullpath
      seconds' <- getCurrentTime
      unless (diffUTCTime seconds' seconds < realToFrac (minutes * 60))
        $ removeDirectoryRecursive fullpath
