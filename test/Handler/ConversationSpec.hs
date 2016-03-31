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


{- This is more time-consuming than it's worth right now. So I'll be taking
   a break from writing it.


    describe "Integration test across all Conversation handlers" $ do

        it ([r|
    Integration: creates a conversation between 3 users, and logs into each
    user to post a new message and make sure that all messages are accessible.

    Then log into a fourth user and make sure that they aren't able to access
    the conversation, or add themselves to it. Then log back in as one of the
    conversation's participants and add the fourth user to the conversation.

    The fourth user should be able to see the full conversation history and add
    their own message, though in the future they shouldn't be able to see the
    history without explicit permission.|])
            $ do

            let printResponseToConsole = withResponse (print . simpleBody)

            (yourId, you) <- createNewUserAndProfile 1
            (jacksId, jack) <- createNewUserAndProfile 2
            (jillsId, jill) <- createNewUserAndProfile 3
            (otherId, other) <- createNewUserAndProfile 4

            authenticateAs you
            postBody ConversationsR $ encode (
                ("first", [2,3]) :: (Text, [Int64])
                )

            authenticateAs jack
            getJson ConversationsR
            statusIs 200

            -- get the ConversationId
            cid <- withResponse (\r -> do
                mcids <- decode <$> simpleBody r
                return $ headEx . fromJust $ mcids)

            -- add a new message to the conversation
            postBody (ConversationContentsR cid) $ encode "second"

            authenticateAs jill
            getJson ConversationsR
            statusIs 200
            printResponseToConsole

            postBody (ConversationContentsR cid) $ encode "third"

            authenticateAs other
            getJson ConversationsR
            statusIs 500 -- they shouldn't have access
            printResponseToConsole
-}
