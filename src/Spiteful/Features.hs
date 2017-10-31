{-# LANGUAGE DeriveGeneric #-}

module Spiteful.Features
  ( Feature(..)
  , Features
  , defaultFeatures
  ) where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
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


type Features = HashSet Feature

-- | Default for when no --feature flag has been passed.
defaultFeatures :: Features
defaultFeatures = HS.fromList [ FeatureDontUpvote
                              , FeatureUpvoteIf
                              , FeatureIfThisGetsUpvotes
                              ]
