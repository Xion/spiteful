{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Spiteful.Metrics where

import Control.Concurrent.STM
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


postsSeen :: TVar Int
postsSeen = unsafePerformIO $ newTVarIO 0
{-# NOINLINE postsSeen #-}

postsUpvoted :: TVar Int
postsUpvoted = unsafePerformIO $ newTVarIO 0
{-# NOINLINE postsUpvoted #-}

postsDownvoted :: TVar Int
postsDownvoted = unsafePerformIO $ newTVarIO 0
{-# NOINLINE postsDownvoted #-}

commentsSeen :: TVar Int
commentsSeen = unsafePerformIO $ newTVarIO 0
{-# NOINLINE commentsSeen #-}

commentsRepliedTo :: TVar Int
commentsRepliedTo = unsafePerformIO $ newTVarIO 0
{-# NOINLINE commentsRepliedTo #-}

dontUpvotePostsSeen :: TVar Int
dontUpvotePostsSeen = unsafePerformIO $ newTVarIO 0
{-# NOINLINE dontUpvotePostsSeen #-}

ifThisGetsUpvotesPostsSeen :: TVar Int
ifThisGetsUpvotesPostsSeen = unsafePerformIO $ newTVarIO 0
{-# NOINLINE ifThisGetsUpvotesPostsSeen #-}

daeCommentsSeen :: TVar Int
daeCommentsSeen = unsafePerformIO $ newTVarIO 0
{-# NOINLINE daeCommentsSeen #-}

upvoteIfPostsSeen :: TVar Int
upvoteIfPostsSeen = unsafePerformIO $ newTVarIO 0
{-# NOINLINE upvoteIfPostsSeen #-}


-- | Pipe that counts all passing elements and stores the total in given TVar.
countAs :: (MonadIO m, Num n) => TVar n -> Pipe a a m r
countAs tvar = P.chain $ \_ -> liftIO $ atomically $ modifyTVar' tvar (+1)


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
    lines <- (concat . ([sep]:) . map (map formatKV)) <$>
             atomically (mapM getStatistics fs)
    return $ lines <> [sep]

-- TODO: don't repeat statistics that are used by multiple features
getStatistics :: Feature -> STM [(Text, Text)]
getStatistics = \case
  FeatureDontUpvote -> do
    postsSeen' <- readTVar postsSeen
    dontUpvotePostsSeen' <- readTVar dontUpvotePostsSeen
    postsUpvoted' <- readTVar postsUpvoted
    return [ ("Total posts seen", tshow postsSeen')
           , ("'dont upvote' posts seen", tshow dontUpvotePostsSeen')
           , ("Posts upvoted", tshow postsUpvoted')
           ]
  FeatureUpvoteIf -> do
    postsSeen' <- readTVar postsSeen
    upvoteIfPostsSeen' <- readTVar upvoteIfPostsSeen
    postsDownvoted' <- readTVar postsDownvoted
    return [ ("Total posts seen", tshow postsSeen')
           , ("'upvote if' posts seen", tshow upvoteIfPostsSeen')
           , ("Posts downvoted", tshow postsDownvoted')
           ]
  FeatureIfThisGetsUpvotes -> do
    postsSeen' <- readTVar postsSeen
    ifThisGetsUpvotesPostsSeen' <- readTVar ifThisGetsUpvotesPostsSeen
    postsDownvoted' <- readTVar postsDownvoted
    return [ ("Total posts seen", tshow postsSeen')
           , ("'if this gets upvotes' posts seen"
             , tshow ifThisGetsUpvotesPostsSeen')
           , ("Posts downvoted", tshow postsDownvoted')
           ]
  FeatureDAE -> do
    commentsSeen' <- readTVar commentsSeen
    daeCommentsSeen' <- readTVar daeCommentsSeen
    commentsRepliedTo' <- readTVar commentsRepliedTo
    return [ ("Total comments seen", tshow commentsSeen')
           , ("DAE comments seen", tshow daeCommentsSeen')
           , ("Comments replied to", tshow commentsRepliedTo')
           ]
