{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Spiteful.Metrics where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.HashSet as HS
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Network.HTTP.Server
import Pipes
import qualified Pipes.Prelude as P
import System.IO.Unsafe (unsafePerformIO)

import Spiteful.Features
import Spiteful.Util (tshow)


postsSeen :: MVar Int
postsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE postsSeen #-}

postsUpvoted :: MVar Int
postsUpvoted = unsafePerformIO $ newMVar 0
{-# NOINLINE postsUpvoted #-}

postsDownvoted :: MVar Int
postsDownvoted = unsafePerformIO $ newMVar 0
{-# NOINLINE postsDownvoted #-}

commentsSeen :: MVar Int
commentsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE commentsSeen #-}

commentsRepliedTo :: MVar Int
commentsRepliedTo = unsafePerformIO $ newMVar 0
{-# NOINLINE commentsRepliedTo #-}

dontUpvotePostsSeen :: MVar Int
dontUpvotePostsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE dontUpvotePostsSeen #-}

ifThisGetsUpvotesPostsSeen :: MVar Int
ifThisGetsUpvotesPostsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE ifThisGetsUpvotesPostsSeen #-}

daeCommentsSeen :: MVar Int
daeCommentsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE daeCommentsSeen #-}

upvoteIfPostsSeen :: MVar Int
upvoteIfPostsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE upvoteIfPostsSeen #-}


-- | Pipe that counts all passing elements and stores the total in given MVar.
countAs :: (MonadIO m, Num n) => MVar n -> Pipe a a m r
countAs mvar = P.chain $ \_ -> liftIO $ do
  -- Can't use modifyMVar_ because it doesn't guarantee atomicity
  -- when there are multiple readers/writers.
  count <- takeMVar mvar
  putMVar mvar $ count + 1


-- | Print statistics to stdout.
printStatistics :: Features -> IO ()
printStatistics features =
  mapM_ Text.putStrLn =<< formatStatistics joinKV sep features
  where
  joinKV (k, v) = k <> ": " <> v
  sep = Text.replicate 20 "-"

-- | Serve statistics as an HTTP response.
serveStatistics :: Features -> IO (Response ByteString)
serveStatistics features = do
  stats <- Text.unlines <$> formatStatistics joinKV sep features
  let body = preamble <> stats <> postamble
  return $ (respond @ByteString OK) { rspBody = Text.encodeUtf8 body }
  where
  joinKV (k, v) = "<li><strong>" <> k <> ":</strong> " <> v <> "</li>"
  sep = "</ul><ul>"
  preamble = "<html><body><ul>"
  postamble = "</ul></body></html>"

formatStatistics :: ((Text, Text) -> Text) -> Text -> Features -> IO [Text]
formatStatistics formatKV sep features = do
  let fs = HS.toList features
  if null fs then return []
  else do
    lines <- (concat . ([sep]:) . map (map formatKV)) <$> mapM getStatistics fs
    return $ lines <> [sep]

-- TODO: don't repeat statistics that are used by multiple features
getStatistics :: Feature -> IO [(Text, Text)]
getStatistics = \case
  FeatureDontUpvote -> do
    postsSeen' <- readMVar postsSeen
    dontUpvotePostsSeen' <- readMVar dontUpvotePostsSeen
    postsUpvoted' <- readMVar postsUpvoted
    return [ ("Total posts seen", tshow postsSeen')
           , ("'dont upvote' posts seen", tshow dontUpvotePostsSeen')
           , ("Posts upvoted", tshow postsUpvoted')
           ]
  FeatureUpvoteIf -> do
    postsSeen' <- readMVar postsSeen
    upvoteIfPostsSeen' <- readMVar upvoteIfPostsSeen
    postsDownvoted' <- readMVar postsDownvoted
    return [ ("Total posts seen", tshow postsSeen')
           , ("'upvote if' posts seen", tshow upvoteIfPostsSeen')
           , ("Posts downvoted", tshow postsDownvoted')
           ]
  FeatureIfThisGetsUpvotes -> do
    postsSeen' <- readMVar postsSeen
    ifThisGetsUpvotesPostsSeen' <- readMVar ifThisGetsUpvotesPostsSeen
    postsDownvoted' <- readMVar postsDownvoted
    return [ ("Total posts seen", tshow postsSeen')
           , ("'if this gets upvotes' posts seen"
             , tshow ifThisGetsUpvotesPostsSeen')
           , ("Posts downvoted", tshow postsDownvoted')
           ]
  FeatureDAE -> do
    commentsSeen' <- readMVar commentsSeen
    daeCommentsSeen' <- readMVar daeCommentsSeen
    commentsRepliedTo' <- readMVar commentsRepliedTo
    return [ ("Total comments seen", tshow commentsSeen')
           , ("DAE comments seen", tshow daeCommentsSeen')
           , ("Comments replied to", tshow commentsRepliedTo')
           ]
