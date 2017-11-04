{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Spiteful.Features
  ( Feature(..)
  , describe
  , Features
  , defaultFeatures
  ) where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)


-- | Bot feature that can be turned on or off.
data Feature = FeatureDontUpvote
             | FeatureUpvoteIf
             | FeatureIfThisGetsUpvotes
             | FeatureDAE
             deriving (Bounded, Enum, Eq, Generic, Ord, Read)

instance Hashable Feature

instance Show Feature where
  show FeatureDontUpvote = "dont-upvote"
  show FeatureUpvoteIf = "upvote-if"
  show FeatureIfThisGetsUpvotes = "if-this-gets-upvotes"
  show FeatureDAE = "dae"

-- | User-friendly explanation of what a particular feature does.
describe :: Feature -> Text
describe = \case
  FeatureDontUpvote ->
    "Look for Reddit posts that have \"don't upvote\" or similar phrases "
    <> "in their titles, and specifically upvote them."
  FeatureUpvoteIf ->
    "Look for Reddit posts that are upvote pleas or generic polls -- "
    <> "based on whether they contain a phrase like \"upvote if\" in the title "
    <> "-- and downvote them."
  FeatureIfThisGetsUpvotes ->
    "Look for Reddit posts that are upvote baits "
    <> "(containing \"if this gets N upvotes\" or a similar phrase) "
    <> "and downvote them."
  FeatureDAE ->
    "Look for Reddit comments that ask \"does anyone else\" (dae) "
    <> "and reply with some variant of \"No.\"."


type Features = HashSet Feature

-- | Default for when no --feature flag has been passed.
defaultFeatures :: Features
defaultFeatures = HS.fromList [ FeatureDontUpvote
                              , FeatureUpvoteIf
                              , FeatureIfThisGetsUpvotes
                              ]
