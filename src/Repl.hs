module Repl where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Attoparsec.Text qualified as P
import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Text hiding (dropWhile, unwords)
import Data.Text qualified as T (dropWhile, unwords)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Vector qualified as V (fromList, toList)
import GHC.Generics (Generic)
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
  deriving (Eq, Generic, Ord, Show)

instance FromNamedRecord FileNote
instance ToNamedRecord FileNote
instance DefaultOrdered FileNote

-- db helpers, "it's a csv" edition
readCsv :: (MonadThrow m, MonadIO m) => AppT m [FileNote]
readCsv = do
  dbPath <- asks dbPath
  
  csvs <- liftIO $ BL.readFile dbPath

  case decodeByName csvs of
    Left err -> throwError . Surprise $ pack err
    Right (_, v) -> pure $ V.toList v

-- feels fucked up that this is hardcoded, there's gotta be a better way
fileNotesHeader :: Header
fileNotesHeader = V.fromList
  [ "filePath"
  , "fileLine"
  , "gitRemote"
  , "gitBranch"
  , "noteTag"
  , "noteText"
  ]

writeCsv :: (MonadThrow m, MonadIO m) => [FileNote] -> AppT m ()
writeCsv fileNotes = do
  dbPath <- asks dbPath

  liftIO $ BL.writeFile dbPath $ encodeByName fileNotesHeader fileNotes


-- do work son
writeNote :: Monad m -> AppT m ()
writeNote = do
  env <- ask
  let input = input env

  currentNotes = readCsv

  newNote = FileNote
    { filePath = inFilePath input
    , fileLine = inFileLine input
    , gitRemote = curGitOrigin env
    , gitBranch = curGitBranch env
    , noteTag = 
    }