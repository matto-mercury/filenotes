module AppEnvironment where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as B8
import Data.Text hiding (dropWhile, filter, lines, unwords)
import Data.Text qualified as T (dropWhile, filter, lines, unwords)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Shelly
import System.Environment (getArgs, withArgs)
import System.Environment.XDG.BaseDir (getUserDataDir)
import Text.Read (readMaybe)

import LocalPrelude

-- not sure this is quite the error model I want but we'll figure it out
data StopCondition
  = Expected Text
  | Surprise Text
  deriving (Eq, Show)

newtype Tag = Tag { unTag :: Text }
  deriving (Eq, Show)

data Command
  = MakeNote (Maybe Tag) Text
  | DisplayNote -- display note on line, otherwise display notes in file
  deriving (Eq, Show)

data Input = Input
  { inFilePath :: FilePath
  , inFileLine :: Int
  , command :: Command
  }
  deriving (Show)

data Env = Env
  { dbPath :: FilePath
  , curGitOrigin :: Maybe Text 
  , curGitBranch :: Maybe Text
  , utcNow :: UTCTime
  , input :: Input
  }
  deriving (Show)

newtype AppT m a = AppT { runAppT :: ReaderT Env (ExceptT StopCondition m) a }
  deriving newtype
  ( MonadReader Env
  , MonadError StopCondition
  , Monad, Applicative, Functor, MonadIO, MonadThrow
  )

-- this and the next function are near-duplicates from ghast, do I need a lib
-- for parsing `git(1)` output?
findFetchRemote :: Text -> Maybe Text
findFetchRemote remotesText =
  let remotes = T.lines remotesText
      fetches = filter (isSuffixOf "(fetch)") remotes
   in
      case fetches of
        [] -> Nothing
        f:fs -> Just f

-- in ghast this had structure to it, here it's just a string
parseRemote :: P.Parser Text
parseRemote = do
  P.string "origin\tgit@github.com:"
  remote <- P.takeTill (== '.')
  pure remote

parseBranch :: P.Parser Text
parseBranch = "refs/heads/" *> P.takeText

parseArgs :: [String] -> Either String Input
parseArgs = \case
  [] -> Left "No arguments provided"
  [p] -> Left $ "No command provided, only prefix `" <> p <> "'"
  p:cs -> buildInput p cs

  where
    buildInput :: String -> [String] -> Either String Input
    buildInput pfx commands = do
      (filePath, fileLine) <- parseFilePathWithLine pfx
      command <- parseCommand $ unwords commands -- fuck it just cat 'em all
      pure Input {..}

    -- blargh. This could be attoparsec but the String/Text mismatch just kills me
    -- maybe later
    splitAt :: Char -> String -> Either String (String, String)
    splitAt c [] = Left "Empty input"
    splitAt c cs = go [] cs
      where
        go pfx [] = Left $ "Separator not found: `" <> [c] <> "'"
        go pfx (s:sfx)
          | s == c = Right (pfx, sfx)
          | otherwise = go (pfx <> [s]) sfx

    parseFilePathWithLine :: String -> Either String (FilePath, Int)
    parseFilePathWithLine pfx = do
      (before, after) <- splitAt ':' pfx
      line <- maybeToEither ("Line number isn't a number: `" <> after <> "'") $ readMaybe after
      pure (before, line)

    parseCommand :: String -> Either String Command
    parseCommand = \case
      "list" -> Right DisplayNote
      c -> tryMakeNote c

    tryMakeNote :: String -> Either String Command
    tryMakeNote note = do
      (tagTxt, noteTxt) <- splitAt ':' note
      let tag = Tag $ pack tagTxt
      let note = pack $ dropWhile (== ' ') noteTxt
      pure $ MakeNote (Just tag) note


readEnv :: IO Env
readEnv = do
  utcNow <- getCurrentTime

  (branchStr, remoteStr) <- shelly . silently $ do
    b <- run "git" ["symbolic-ref", "--quiet", "HEAD"]
    r <- run "git" ["remote", "-v"]
    pure (b, r)
  
  -- TODO: detect when ~/.local/share/filenotes doesn't exist and populate it
  dbDir <- getUserDataDir "filenotes"
  let dbPath = dbDir <> "/notes.txt"

  inputArgs <- getArgs

  pure $ either error id $ do
    gitBranch <- P.parseOnly parseBranch branchStr
    curGitOrigin <- P.parseOnly parseRemote `traverse` findFetchRemote remoteStr
    let curGitBranch = Just gitBranch 
    input <- parseArgs inputArgs

    pure Env {..}

-- entry point for realsies, basically copied from ghast
runAppEnv :: AppT IO () -> IO ()
runAppEnv app = do
  env <- readEnv
  eResult <- runExceptT $ runReaderT (runAppT app) env
  case eResult of
    Left x -> showStop x
    Right v -> pure v

showStop :: StopCondition -> IO () 
showStop stop = do
  case stop of
    Expected e -> putStrLn $ unpack e
    Surprise s -> error $ unpack s

-- entry point for repl testing
runWithArgs :: [String] -> AppT IO a -> IO a
runWithArgs args app = 
  withArgs args $ do
    env <- readEnv
    eResult <- runExceptT $ runReaderT (runAppT app) env
    case eResult of
      Left x -> error $ show x
      Right v -> pure v

