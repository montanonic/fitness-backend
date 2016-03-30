module Handler.ConversationSpec (spec) where

import TestImport

import Model.Conversation

import Utilities

spec :: Spec
spec = withApp $ do

    describe "getConversationsR" $ do
        it "returns an empty list when a user is not part of any conversation" $ do
            (_, user) <- createNewUserAndProfile 1

            authenticateAs user

            getJson ConversationsR

            statusIs 200

            withResponse $ \r ->
                assertEqual "is empty" (Just [])
                    ((decode $ simpleBody r) :: Maybe [ConversationId])
