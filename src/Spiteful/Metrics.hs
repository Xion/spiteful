{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Spiteful.Metrics where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Hashable
import qualified Data.HashSet as HS
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Pipes
import qualified Pipes.Prelude as P
import System.IO.Unsafe (unsafePerformIO)

import Spiteful.Features
import Spiteful.Util (tshow)


type Label = Text

data Metric a = Metric { metLabel :: !Label
                       , metValue :: !(TVar a)
                       }

instance Show (Metric a) where
  show Metric{..} = "Metric <" ++ Text.unpack metLabel ++ ">"

instance Eq (Metric a) where
  (==) = (==) `on` metLabel

instance Hashable (Metric a) where
  hash = hash . metLabel
  hashWithSalt s = hashWithSalt s . metLabel

mkMetric :: Text -> a -> Metric a
mkMetric label value = Metric label $ unsafePerformIO $ newTVarIO value

mkCounter :: Text -> Metric Int
mkCounter label = mkMetric label 0

readMetric :: Metric a -> STM a
readMetric = readTVar . metValue

readMetricIO :: Metric a -> IO a
readMetricIO = readTVarIO . metValue


postsSeen :: Metric Int
postsSeen = mkCounter "Total posts seen"
{-# NOINLINE postsSeen #-}

postsUpvoted :: Metric Int
postsUpvoted = mkCounter "Posts upvoted"
{-# NOINLINE postsUpvoted #-}

postsDownvoted :: Metric Int
postsDownvoted = mkCounter "Posts downvoted"
{-# NOINLINE postsDownvoted #-}

commentsSeen :: Metric Int
commentsSeen = mkCounter "Total comments seen"
{-# NOINLINE commentsSeen #-}

commentsRepliedTo :: Metric Int
commentsRepliedTo = mkCounter "Comments replied to"
{-# NOINLINE commentsRepliedTo #-}

dontUpvotePostsSeen :: Metric Int
dontUpvotePostsSeen = mkCounter "'dont upvote' posts seen"
{-# NOINLINE dontUpvotePostsSeen #-}

ifThisGetsUpvotesPostsSeen :: Metric Int
ifThisGetsUpvotesPostsSeen = mkCounter "'if this gets upvotes' posts seen"
{-# NOINLINE ifThisGetsUpvotesPostsSeen #-}

daeCommentsSeen :: Metric Int
daeCommentsSeen = mkCounter "DAE comments seen"
{-# NOINLINE daeCommentsSeen #-}

upvoteIfPostsSeen :: Metric Int
upvoteIfPostsSeen = mkCounter "'upvote if' posts seen"
{-# NOINLINE upvoteIfPostsSeen #-}


-- | Pipe that counts all passing elements and stores the total in given TVar.
countAs :: (MonadIO m, Num n) => Metric n -> Pipe a a m r
countAs Metric{..} =
  P.chain $ \_ -> liftIO $ atomically $ modifyTVar' metValue (+1)


-- | Print statistics to stdout.
printStatistics :: Features -> IO ()
printStatistics features =
  mapM_ Text.putStrLn =<< formatStatistics joinKV sep features
  where
  joinKV (k, v) = k <> ": " <> tshow v
  sep = Text.replicate 20 "-"

-- | Serve statistics as an HTTP response body.
serveStatistics :: Features -> IO ByteString
serveStatistics features = do
  stats <- Text.unlines <$> formatStatistics joinKV sep features
  let body = preamble <> stats <> postamble
  return $ Text.encodeUtf8 body
  where
  joinKV (k, v) = "<li><strong>" <> k <> ":</strong> " <> tshow v <> "</li>"
  sep = "</ul><ul>"
  preamble = "<html><body><ul>"
  postamble = "</ul></body></html>"

formatStatistics :: ((Label, Int) -> Text) -> Text -> Features -> IO [Text]
formatStatistics formatKV sep features = do
  let fs = HS.toList features
  if null fs then return []
  else do
    lines <- (concat . ([sep]:) . map (map formatKV)) <$> mapM getStatistics fs
    return $ lines <> [sep]

-- TODO: don't repeat statistics that are used by multiple features
getStatistics :: Feature -> IO [(Label, Int)]
getStatistics f = readMetrics $ case f of
  FeatureDontUpvote -> [postsSeen, dontUpvotePostsSeen, postsUpvoted]
  FeatureUpvoteIf -> [postsSeen, upvoteIfPostsSeen, postsDownvoted]
  FeatureIfThisGetsUpvotes ->
    [postsSeen, ifThisGetsUpvotesPostsSeen, postsDownvoted]
  FeatureDAE -> [commentsSeen, daeCommentsSeen, commentsRepliedTo]
  where
  readMetrics :: [Metric a] -> IO [(Label, a)]
  readMetrics = mapM $ \m@Metric{..} -> (metLabel,) <$> readMetricIO m
