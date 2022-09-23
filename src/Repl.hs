module Repl where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as B8
import Data.Text hiding (dropWhile, unwords)
import Data.Text qualified as T (dropWhile, unwords)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Shelly
import System.Environment (getArgs, withArgs)
import System.Environment.XDG.BaseDir (getUserDataDir)
import Text.Read (readMaybe)

import AppEnvironment
import LocalPrelude

data FileNote = FileNote
  { filePath :: FilePath
  , fileLine :: Int
  , gitRemote :: Maybe Text
  , gitBranch :: Maybe Text
  , noteTag :: Maybe Tag
  , noteText :: Text
  }
  deriving (Eq, Show)

-- db helpers, "it's a csv" edition
readCsv :: (MonadThrow m, MonadIO m) => AppT m [FileNote]
readCsv = do
  dbPath <- asks dbPath
  undefined