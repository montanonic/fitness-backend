module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    
    describe "getProfileR" $ do
        it "gives a 201" $ pending
