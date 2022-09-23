module LocalPrelude (module X, module LocalPrelude) where

import Data.Text as X (Text)
import Prelude as X hiding (putStrLn)

import Control.Monad.IO.Class
import Data.Text (pack)
import Prelude qualified as Pr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Pr.putStrLn

tshow :: Show a => a -> Text
tshow = pack . show

-- this comes in a lot of modules but I feel like importing one is overkill?
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither failure = \case
  Nothing -> Left failure
  Just success -> Right success