{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Spiteful.Metrics where

import Control.Arrow ((&&&), (***))
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.Aeson as Ae
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict ((!), HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Function (on)
import Data.List (sortOn)
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
{-# NOINLINE mkMetric #-}

mkCounter :: Text -> Metric Int
mkCounter label = mkMetric label 0
{-# NOINLINE mkCounter #-}

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

-- | Render statistics as a JSON document.
renderJSONStatistics :: Features -> IO ByteString
renderJSONStatistics features = do
  stats <- sortOn fst . HM.toList <$> getAllStatistics features
  let stats' :: HashMap Text (HashMap Label Int)
      stats' = HM.fromList $ map (maybe "" tshow *** HM.fromList) stats
  return $ LBS.toStrict (Ae.encode stats')

-- | Render statistics as an HTML document.
renderHTMLStatistics :: Features -> IO ByteString
renderHTMLStatistics features = do
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
    stats <- sortOn fst . HM.toList <$> getAllStatistics fs
    let lines = concatMap ((sep:) . map formatKV) . map snd $ stats
    return $ lines <> [sep]

getAllStatistics :: Foldable t
                 => t Feature -> IO (HashMap (Maybe Feature) [(Label, Int)])
getAllStatistics features = do
  let fs = toList features
  -- Get all metrics (grouped by feature) and their current values.
  groupedStats <- atomically $ mapM (mapM readMetric') $ map getMetrics fs
  let metrics :: HashMap Label Int
      metrics = HM.fromList $ concat groupedStats
  -- See how many features use each metric and divide them into unique & shared.
  let metricCounts = HM.fromListWith (+)
                     [ (m, 1) | m <- map fst . concat $ groupedStats ]
      sharedMetrics = HS.fromList . HM.keys $ HM.filter (> 1) metricCounts
      isUnique = not . (`HS.member` sharedMetrics)
  -- Reattach the metric values and form the final result.
  let sharedMV = HM.singleton Nothing $
                 map (id &&& (metrics !)) $ HS.toList sharedMetrics
      uniqueMV = HM.fromList $ zip (map Just fs)
                                   (map (filter $ isUnique . fst) groupedStats)
  return $ HM.filter (not . null) $ sharedMV <> uniqueMV
  where
  readMetric' :: Metric a -> STM (Label, a)
  readMetric' m@Metric{..} = (metLabel,) <$> readMetric m

getMetrics :: Feature -> [Metric Int]
getMetrics = \case
  FeatureDontUpvote -> [postsSeen, dontUpvotePostsSeen, postsUpvoted]
  FeatureUpvoteIf -> [postsSeen, upvoteIfPostsSeen, postsDownvoted]
  FeatureIfThisGetsUpvotes ->
    [postsSeen, ifThisGetsUpvotesPostsSeen, postsDownvoted]
  FeatureDAE -> [commentsSeen, daeCommentsSeen, commentsRepliedTo]
