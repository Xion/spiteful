module Spiteful.Features.DontUpvote
  ( monitorDontUpvotePosts
  ) where

import Control.Monad
import Data.Either.Combinators (isRight, whenLeft)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Pipes
import qualified Pipes.Prelude as P
import Reddit.Types.Post

import Spiteful.Logging
import Spiteful.Metrics
import Spiteful.Options
import Spiteful.Reddit
import Spiteful.Util (deburr, tshow)


monitorDontUpvotePosts :: Options -> IO ()
monitorDontUpvotePosts opts = do
  result <- runEffect $
    fetchPosts opts >-> countAs postsSeen
    >-> P.filter isDontUpvotePost >-> countAs dontUpvotePostsSeen
    >-> P.filter (not . hasBeenVotedOn)
    >-> P.mapM (upvotePost opts) >-> P.filter isRight >-> countAs postsUpvoted
    >-> P.drain -- drop upvote outcomes but retain a possible fetch error
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching posts: " <> tshow err


isDontUpvotePost :: Post -> Bool
isDontUpvotePost Post{..} = any (`Text.isInfixOf` title') phrases
  where
  title' = deburr title
  phrases = map deburr $ wordProduct ["don't", "dont", "no", "not"]
                                     ["upvote", "upboat"]

  -- | Make a Cartesian product of two word lists.
  wordProduct :: [Text] -> [Text] -> [Text]
  wordProduct = liftM2 $ \a b -> a <> " " <> b


hasBeenVotedOn :: Post -> Bool
hasBeenVotedOn = isJust . liked
