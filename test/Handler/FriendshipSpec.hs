module Handler.FriendshipSpec (spec) where

import TestImport

import Model
import Model.Persistent
import Model.Friendship

import Utilities

spec :: Spec
spec = withApp $ do

    describe "postCreateFriendRequestR" $ do

        let createRequest = do
            e1@(Entity _ you) <- createNewUserAndProfileEnt 1
            e2@(Entity theirId _) <- createNewUserAndProfileEnt 2

            authenticateAs you

            post (CreateFriendRequestR theirId)

            return (e1, e2)

        it "gives a 303 when not authenticated" $ do
            _ <- createNewUserAndProfile 1
            (theirId, _) <- createNewUserAndProfile 2

            post (CreateFriendRequestR theirId)

            statusIs 303

        it "gives a 500 if attempting to friend self" $ do
            (uid, user) <- createNewUserAndProfile 1

            authenticateAs user

            post (CreateFriendRequestR uid)

            statusIs 500

        it "gives a 200 when used on another user" $ do
            createRequest

            statusIs 200

        it "gives a 500 when already friends" $ do
            (Entity _ you, Entity theirId _) <- createRequest

            authenticateAs you

            post (CreateFriendRequestR theirId)

            statusIs 500

        it "properly creates a Friendship entity" $ do
            (Entity yourId you, Entity theirId them) <- createRequest

            (Entity _ Friendship{..}) <- runDB $
                getUniqueFriendship yourId theirId

            assertEqual ("friendship created has the correct references to the"
                ++ "users")
                (friendshipFirstUser, friendshipSecondUser)
                (yourId             , theirId             )

            assertEqual "friendship created is not active and set as a request"
                (friendshipIsActive, friendshipIsRequest) (False, True)

        it "properly creates a log" $ do
            (Entity yourId you, Entity theirId _) <- createRequest

            mlog <- runDB $ do
                fid <- getUniqueFriendshipId yourId theirId
                selectFirst
                    [ FriendshipLogFriendship ==. fid
                    , FriendshipLogAction ==. CreateRequest ]
                    []

            assertEqual "the correct log exists"
                (isJust mlog)
                True

        it "won't let you send more than 3 requests" $ do
            (Entity _ you) <- createNewUserAndProfileEnt 1
            (Entity theirId _) <- createNewUserAndProfileEnt 2

            authenticateAs you

            -- first
            post (CreateFriendRequestR theirId)
            post (CancelFriendRequestR theirId)
            -- second
            post (CreateFriendRequestR theirId)
            post (CancelFriendRequestR theirId)
            -- should be okay here
            statusIs 200

            -- should be able to create a third request
            post (CreateFriendRequestR theirId)
            statusIs 200

            -- should be able to cancel a third time as well
            post (CancelFriendRequestR theirId)

    describe "postAcceptFriendRequestR" $ do
        let createAndAcceptRequest = do
            e1@(Entity yourId you) <- createNewUserAndProfileEnt 1
            e2@(Entity theirId them) <- createNewUserAndProfileEnt 2

            authenticateAs you

            post (CreateFriendRequestR theirId)

            authenticateAs them

            post (AcceptFriendRequestR yourId)

            return (e1, e2)

        it "gives a 303 when receiver is not authenticated" $ do
            (yourId, you) <- createNewUserAndProfile 1
            (theirId, _) <- createNewUserAndProfile 2

            authenticateAs you

            post (CreateFriendRequestR theirId)

            logout

            post (AcceptFriendRequestR yourId)


        it "gives a 500 when no request exists" $ do
            (yourId, you) <- createNewUserAndProfile 1
            (theirId, them) <- createNewUserAndProfile 2

            authenticateAs you

            post (AcceptFriendRequestR theirId)

            statusIs 500

        it "gives a 200 following the creation of a request" $ do
            _ <- createAndAcceptRequest

            statusIs 200

        it "properly modifies the Friendship entity" $ do
            (Entity yourId _, Entity theirId _) <- createAndAcceptRequest

            (Entity _ Friendship{..}) <- runDB $
                getUniqueFriendship yourId theirId

            assertEqual ("friendship entity is flagged as active, and is not"
                ++ "flagged as a request")
                (friendshipIsActive, friendshipIsRequest)
                (True              , False              )

        it "properly creates a log" $ do
            (Entity yourId _, Entity theirId _) <- createAndAcceptRequest

            mlog <- runDB $ do
                fid <- getUniqueFriendshipId yourId theirId
                selectFirst
                    [ FriendshipLogFriendship ==. fid
                    , FriendshipLogAction ==. AcceptRequest ]
                    []

            assertEqual "the correct log exists"
                (isJust mlog)
                True

    describe "postCancelFriendRequestR" $ do
        let createRequest = do
            e1@(Entity _ you) <- createNewUserAndProfileEnt 1
            e2@(Entity theirId _) <- createNewUserAndProfileEnt 2

            authenticateAs you

            post (CreateFriendRequestR theirId)

            return (e1, e2)

        it "gives a 500 when no request exists" $ do
            (yourId, you) <- createNewUserAndProfile 1
            (theirId, them) <- createNewUserAndProfile 2

            authenticateAs you

            post (CancelFriendRequestR theirId)

            statusIs 500

        it "gives a 200 when you cancel a request you sent" $ do
            (Entity yourId you, Entity theirId them) <- createRequest

            post (CancelFriendRequestR theirId)

            statusIs 200
