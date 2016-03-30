module Handler.CommonSpec (spec) where

import TestImport

import Utilities

spec :: Spec
spec = withApp $ do
    describe "robots.txt" $ do
        it "gives a 200" $ do
            getJson RobotsR
            statusIs 200
        it "has correct User-agent" $ do
            getJson RobotsR
            bodyContains "User-agent: *"
    describe "favicon.ico" $ do
        it "gives a 200" $ do
            getJson FaviconR
            statusIs 200
