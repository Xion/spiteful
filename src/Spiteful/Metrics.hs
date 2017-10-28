module Spiteful.Metrics where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.HashSet as HS
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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

daeCommentsSeen :: MVar Int
daeCommentsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE daeCommentsSeen #-}

upvoteIfPostsSeen :: MVar Int
upvoteIfPostsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE upvoteIfPostsSeen #-}


-- | Pipe that counts all passing elements and stores the total in given MVar.
countAs :: (MonadIO m, Num n) => MVar n -> Pipe a a m r
countAs mvar = P.chain $ \_ -> liftIO $ modifyMVar_ mvar (return . (+1))

printStatistics :: Features -> IO ()
printStatistics features = do
  lines <- (concat <$>) . mapM (((sep:) <$>) . stats) $ HS.toList features
  mapM_ Text.putStrLn $ lines <> [sep]
  where
  stats FeatureDontUpvote = do
    postsSeen' <- readMVar postsSeen
    dontUpvotePostsSeen' <- readMVar dontUpvotePostsSeen
    postsUpvoted' <- readMVar postsUpvoted
    return [ "Total posts seen: " <> tshow postsSeen'
           , "'dont upvote' posts seen: " <> tshow dontUpvotePostsSeen'
           , "Posts upvoted: " <> tshow postsUpvoted'
           ]
  stats FeatureUpvoteIf = do
    postsSeen' <- readMVar postsSeen
    upvoteIfPostsSeen' <- readMVar upvoteIfPostsSeen
    postsDownvoted' <- readMVar postsDownvoted
    return [ "Total posts seen: " <> tshow postsSeen'
           , "'upvote if' posts seen: " <> tshow upvoteIfPostsSeen'
           , "Posts downvoted: " <> tshow postsDownvoted'
           ]
  stats FeatureDAE = do
    commentsSeen' <- readMVar commentsSeen
    daeCommentsSeen' <- readMVar daeCommentsSeen
    commentsRepliedTo' <- readMVar commentsRepliedTo
    return [ "Total comments seen: " <> tshow commentsSeen'
           , "DAE comments seen: " <> tshow daeCommentsSeen'
           , "Comments replied to: " <> tshow commentsRepliedTo'
           ]

  sep = Text.replicate 20 "-"
