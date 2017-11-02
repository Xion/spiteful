module Spiteful.Features.IfThisGetsUpvotes
  ( monitorIfThisGetsUpvotesPosts
  ) where

import Data.Either.Combinators (isRight, whenLeft)
import Data.Maybe (isJust, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Pipes
import qualified Pipes.Prelude as P
import Reddit.Types.Post
import Text.Regex

import Spiteful.Logging
import Spiteful.Metrics
import Spiteful.Options
import Spiteful.Reddit
import Spiteful.Util (collapseWhitespace, deburr, tshow)


monitorIfThisGetsUpvotesPosts :: Options -> IO ()
monitorIfThisGetsUpvotesPosts opts = do
  result <- runEffect $
    fetchPosts opts >-> countAs postsSeen
    >-> P.filter isIfThisGetsUpvotesPost >-> countAs ifThisGetsUpvotesPostsSeen
    >-> P.filter (isNothing . liked)
    >-> P.mapM (downvotePost opts)
                >-> P.filter isRight >-> countAs postsDownvoted
    >-> P.drain
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching posts: " <> tshow err


isIfThisGetsUpvotesPost :: Post -> Bool
isIfThisGetsUpvotesPost Post{..} =
  any isJust . map (`matchRegex` titleString) $ phrases
  where
  titleString = Text.unpack $ deburr title
  phrases = map (mkRegex' . Text.unpack . regexizeSpaces . collapseWhitespace)
      [ "if this(\\s+post)? gets? \\d+ upvotes" ]
  regexizeSpaces = Text.replace " " "\\s+"
  mkRegex' r = mkRegexWithOpts r singleLine caseSensitive
    where
    singleLine = True  -- ^ and $ match beginning/end of the line
    caseSensitive = False
