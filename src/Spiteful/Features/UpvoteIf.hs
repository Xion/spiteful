module Spiteful.Features.UpvoteIf
  ( monitorUpvoteIfPosts
  ) where

import Control.Monad
import Data.Either.Combinators (isRight, whenLeft)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Pipes
import qualified Pipes.Prelude as P
import Reddit.Types.Post

import Spiteful.Logging
import Spiteful.Metrics
import Spiteful.Options
import Spiteful.Reddit
import Spiteful.Util (deburr, tshow)


monitorUpvoteIfPosts :: Options -> IO ()
monitorUpvoteIfPosts opts = do
  result <- runEffect $
    fetchPosts opts >-> countAs postsSeen
    >-> P.filter isUpvoteIfPost >-> countAs upvoteIfPostsSeen
    >-> P.filter (isNothing . liked)
    >-> P.mapM (downvotePost opts)
                >-> P.filter isRight >-> countAs postsDownvoted
    >-> P.drain
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching posts: " <> tshow err


isUpvoteIfPost :: Post -> Bool
isUpvoteIfPost Post{..} = any (`Text.isInfixOf` title') phrases
  where
  title' = deburr title
  phrases = map deburr $ liftM2 ($) pleas upvotePhrases
  pleas = [ ("pls " <>), ("plis " <>), ("plz " <>), ("pliz " <>), ("please " <>)
          , (<> " if"), (<> " for")
          ]
  upvotePhrases = [ "upvote", "upboat"
                  , "vote up", "vote this up", "vote this post up"
                  ]
