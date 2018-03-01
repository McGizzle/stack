{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.Urls where

import           Data.Aeson.Extended
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           Stack.Prelude

import           Data.Binary

data Urls = Urls
    { urlsLatestSnapshot    :: !Text
    , urlsLtsBuildPlans     :: !Text
    , urlsNightlyBuildPlans :: !Text
    }
    deriving (Show, Generic, Typeable)

instance Binary Urls

-- TODO: Really need this instance?
instance FromJSON (WithJSONWarnings Urls) where
    parseJSON = withObjectWarnings "Urls" $ \o -> do
        Urls
            <$> o ..: "latest-snapshot"
            <*> o ..: "lts-build-plans"
            <*> o ..: "nightly-build-plans"

data UrlsMonoid = UrlsMonoid
    { urlsMonoidLatestSnapshot    :: !(First Text)
    , urlsMonoidLtsBuildPlans     :: !(First Text)
    , urlsMonoidNightlyBuildPlans :: !(First Text)
    }
    deriving (Show, Generic)

instance FromJSON (WithJSONWarnings UrlsMonoid) where
    parseJSON = withObjectWarnings "UrlsMonoid" $ \o -> do
        UrlsMonoid
            <$> o ..: "latest-snapshot"
            <*> o ..: "lts-build-plans"
            <*> o ..: "nightly-build-plans"

instance Monoid UrlsMonoid where
    mempty = memptydefault
    mappend = mappenddefault
