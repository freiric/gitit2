{-# LANGUAGE TypeFamilies, MultiParamTypeClasses,
             OverloadedStrings, FlexibleInstances,
             FlexibleContexts, ScopedTypeVariables, TupleSections #-}
module Config (
               Conf
              , parseConfig
              , readMimeTypesFile
              , gititConfigFromConf
              , cfg_port
              , cfg_repository_path
              , cfg_repository_type
              , cfg_static_dir
              , cfg_max_upload_size
              , cfg_auth_github
              , foundationSettingsFromConf
              , FoundationSettings
              , appRoot
              , gitHub
              , clientId
              , clientSecret
)
where
import System.Environment
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Control.Monad (mzero, when, unless)
import Data.Yaml
import Control.Exception (catch, SomeException)
import Yesod
import Yesod.Static
import Network.Gitit2
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Data.FileStore

import Error

data Conf = Conf { cfg_port             :: Int
                 , cfg_approot          :: Text
                 , cfg_listen_address   :: String
                 , cfg_repository_path  :: FilePath
                 , cfg_repository_type  :: Text
                 , cfg_page_extension   :: FilePath
                 , cfg_default_format   :: Text
                 , cfg_static_dir       :: FilePath
                 , cfg_mime_types_file  :: Maybe FilePath
                 , cfg_use_mathjax      :: Bool
                 , cfg_feed_days        :: Integer
                 , cfg_feed_minutes     :: Integer
                 , cfg_pandoc_user_data :: Maybe FilePath
                 , cfg_use_cache        :: Bool
                 , cfg_cache_dir        :: FilePath
                 , cfg_front_page       :: Text
                 , cfg_help_page        :: Text
                 , cfg_max_upload_size  :: String
                 , cfg_latex_engine     :: Maybe FilePath
                 , cfg_auth_github      :: Maybe GitHub
                 }

data GitHub =  GitHub {
      clientId :: Text
    , clientSecret :: Text
} deriving (Show)

data FoundationSettings  = FoundationSettings {
      gitHub :: Maybe GitHub
    , appRoot:: Text
} deriving (Show)

foundationSettingsFromConf :: Conf -> FoundationSettings
foundationSettingsFromConf conf =
    FoundationSettings (cfg_auth_github conf) (cfg_approot conf)


instance FromJSON GitHub where
    parseJSON (Object o) = GitHub
                           <$> o .: "clientId"
                           <*> o .: "clientSecret"
    parseJSON _          = mzero

parseElem :: FromJSON a => [Object] -> Text -> Parser (Maybe a)
parseElem os text = foldr f (return Nothing) os
    where
      f o pm = do
        maybeParsedO <- o .:? text
        case maybeParsedO of
          Nothing -> pm
          _ -> return maybeParsedO

parseConfig :: [Object] -> Parser Conf
parseConfig os = Conf
  <$> os `parseElem` "port" .!= 50000
  <*> os `parseElem` "approot" .!= "http://localhost:50000"
  <*> os `parseElem` "listen_address" .!= "0.0.0.0"
  <*> os `parseElem` "repository_path" .!= "wikidata"
  <*> os `parseElem` "repository_type" .!= "git"
  <*> os `parseElem` "page_extension" .!= ".page"
  <*> os `parseElem` "default_format" .!= "markdown"
  <*> os `parseElem` "static_dir" .!= "static"
  <*> os `parseElem` "mime_types_file"
  <*> os `parseElem` "use_mathjax" .!= False
  <*> os `parseElem` "feed_days" .!= 14
  <*> os `parseElem` "feed_minutes" .!= 15
  <*> os `parseElem` "pandoc_user_data"
  <*> os `parseElem` "use_cache" .!= False
  <*> os `parseElem` "cache_dir" .!= "cache"
  <*> os `parseElem` "front_page" .!= "Front Page"
  <*> os `parseElem` "help_page" .!= "Help"
  <*> os `parseElem` "max_upload_size" .!= "1M"
  <*> os `parseElem` "latex_engine"
  <*> os `parseElem` "github"

-- | Ready collection of common mime types. (Copied from
-- Happstack.Server.HTTP.FileServe.)
mimeTypes :: M.Map String ContentType
mimeTypes = M.fromList
        [("xml","application/xml")
        ,("xsl","application/xml")
        ,("js","text/javascript; charset=UTF-8")
        ,("html","text/html; charset=UTF-8")
        ,("htm","text/html; charset=UTF-8")
        ,("css","text/css; charset=UTF-8")
        ,("gif","image/gif")
        ,("jpg","image/jpeg")
        ,("png","image/png")
        ,("txt","text/plain; charset=UTF-8")
        ,("doc","application/msword")
        ,("exe","application/octet-stream")
        ,("pdf","application/pdf")
        ,("zip","application/zip")
        ,("gz","application/x-gzip")
        ,("ps","application/postscript")
        ,("rtf","application/rtf")
        ,("wav","application/x-wav")
        ,("hs","text/plain; charset=UTF-8")]

-- | Read a file associating mime types with extensions, and return a
-- map from extensions tstypes. Each line of the file consists of a
-- mime type, followed by space, followed by a list of zersor more
-- extensions, separated by spaces. Example: text/plain txt text
readMimeTypesFile :: FilePath -> IO (M.Map String ContentType)
readMimeTypesFile f = catch
  ((foldr (gs. words) M.empty . lines) `fmap` readFile f)
  handleMimeTypesFileNotFound
     where gs[]     m = m  -- skip blank lines
           gs(x:xs) m = foldr (\ext -> M.insert ext $ B.pack x) m xs
           handleMimeTypesFileNotFound :: SomeException -> IO (M.Map String ContentType)
           handleMimeTypesFileNotFound e = do
             warn $ "Could not parse mime types file.\n" ++ show e
             return mimeTypes


gititConfigFromConf :: Conf -> IO GititConfig
gititConfigFromConf conf = do
  mimes <- case cfg_mime_types_file conf of
                Nothing -> return mimeTypes
                Just f  -> readMimeTypesFile f

  format <- case readPageFormat (cfg_default_format conf) of
                  Just f  -> return f
                  Nothing -> err 11 $ "Unknown default format: " ++
                                   T.unpack (cfg_default_format conf)

  -- clear cache
  when (cfg_use_cache conf) $ do
    let cachedir = cfg_cache_dir conf
    exists <- doesDirectoryExist cachedir
    when exists $ removeDirectoryRecursive cachedir
  let gCacheConfig = GititCacheConfig { use_cache = cfg_use_cache conf
                                      , cache_dir = cfg_cache_dir conf
                                      , feed_minutes  = cfg_feed_minutes conf
                                      }
  let gconfig = GititConfig{ mime_types = mimes
                           , default_format = format
                           , repository_path = cfg_repository_path conf
                           , page_extension = cfg_page_extension conf
                           , static_path = cfg_static_dir conf
                           , use_mathjax = cfg_use_mathjax conf
                           , feed_days  = cfg_feed_days conf
                           , pandoc_user_data = cfg_pandoc_user_data conf
                           , front_page = cfg_front_page conf
                           , help_page = cfg_help_page conf
                           , latex_engine = cfg_latex_engine conf
                           , gitit_cache_conf = gCacheConfig
                           }
  return gconfig
