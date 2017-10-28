{-# LANGUAGE TupleSections #-}

module Spiteful.Features.DAE
  ( monitorDAEComments
  ) where

import Control.Monad.IO.Class
import Data.Either.Combinators (isRight, whenLeft)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Pipes
import qualified Pipes.Prelude as P
import Reddit.Actions.Thing (reply)
import Reddit.Types.Comment
import Reddit.Types.Subreddit (SubredditName(..))
import Reddit.Types.User (Username(..))
import Safe (headMay)
import System.Random
import Text.Regex

import Spiteful.Logging
import Spiteful.Metrics
import Spiteful.Options
import Spiteful.Reddit
import Spiteful.Util (deburr, tshow)


monitorDAEComments :: Options -> IO ()
monitorDAEComments opts = do
  result <- runEffect $
    fetchNewComments opts >-> countAs commentsSeen
    >-> P.mapFoldable findDAEPhrase >-> countAs daeCommentsSeen
    >-> P.filterM ((not <$>) . hasCommentBeenRepliedTo opts . fst)
    >-> P.mapM (uncurry $ replyToDAEComment opts)
                >-> P.filter isRight
                >-> countAs commentsRepliedTo
    >-> P.drain
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching comments: " <> tshow err


findDAEPhrase :: Comment -> Maybe (Comment, Text)
findDAEPhrase comment = (comment,) <$> headMay
    [ Text.pack $ phrase
    | Just (_, phrase, _, _) <- map (`matchRegexAll` commentString) phrases
    ]
  where
  commentString = Text.unpack $ deburr $ body comment
  phrases = map (mkRegex' . (sentenceSepRe ++)) $
      [ "does ae" , "is ae" , "has ae"
      , "dae " ++ anyoneElse -- yes, some dimwits write it like this
      , "does " ++ anyoneElse
      , "is " ++ anyoneElse
      , "has " ++ anyoneElse
      ] <> ["dae"]
  sentenceSepRe = "^\\s*|.\\s+"  -- start of text/line or full stop
  anyoneElse = "any\\s?one else[?]*"

  mkRegex' r = mkRegexWithOpts r singleLine caseSensitive
    where
    singleLine = True  -- ^ and $ match beginning/end of the line
    caseSensitive = False


hasCommentBeenRepliedTo :: MonadIO m => Options -> Comment -> m Bool
hasCommentBeenRepliedTo opts _ | isNothing (optCredentials opts) = return False
hasCommentBeenRepliedTo opts comment@Comment{..} = do
  (hasReply, fetchResult) <- any' isOurReply $ fetchCommentReplies opts comment
  case fetchResult of
    Left err -> do
      let CommentID cid = commentID
          R subr = subreddit
      logFmt Warn "Failed to obtain all replies to comment #{} in /r/{}: {}"
        (cid, subr, tshow err)
      return True -- let's be conservative here
    Right _ -> return hasReply
  where
  -- Like Pipes.Prelude.any but preserves the pipe's original return value.
  any' :: Monad m => (a -> Bool) -> Producer a m r -> m (Bool, r)
  any' pred pipe = P.fold' (||) False id $ pipe >-> P.map pred

  isOurReply Comment{..} = let Username commenter = author
                               Just (username, _) = optCredentials opts
                           in commenter == username


replyToDAEComment :: MonadIO m => Options -> Comment -> Text -> m (EitherR ())
replyToDAEComment opts Comment{..} daePhrase = do
  let CommentID cid = commentID
      R subr = subreddit
  replyText <- sample phrases
  let fullReplyText = "> " <> daePhrase <> "\n\n" <> replyText
  logFmt Info "Replying to comment #{} on /r/{} with \"{}\""
              (cid, subr, fullReplyText)
  result <- runReddit opts $ reply commentID fullReplyText
  case result of
    Left err -> return $ Left err
    Right (CommentID replyID) -> do
      logFmt Debug "Successfuly replied with #{} to comment #{} on /r/{}"
                   (replyID, cid, subr)
      return $ Right ()
  where
  phrases = ["No.", "Nope.", "Not really."]

  sample :: MonadIO m => [a] -> m a
  sample xs = do
    idx <- liftIO $ randomRIO (0, length xs - 1)
    return $ xs !! idx
