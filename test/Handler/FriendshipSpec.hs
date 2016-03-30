module Handler.FriendshipSpec (spec) where

import TestImport
import Text.RawString.QQ

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

            postJson (CreateFriendRequestR theirId)

            return (e1, e2)

        it "gives a 401 when not authenticated" $ do
            _ <- createNewUserAndProfile 1
            (theirId, _) <- createNewUserAndProfile 2

            postJson (CreateFriendRequestR theirId)

            statusIs 401

        it "gives a 500 if attempting to friend self" $ do
            (uid, user) <- createNewUserAndProfile 1

            authenticateAs user

            postJson (CreateFriendRequestR uid)

            statusIs 500

        it "gives a 500 when already friends" $ do
            (Entity _ you, Entity theirId _) <- createRequest

            authenticateAs you

            postJson (CreateFriendRequestR theirId)

            statusIs 500

        it ("when used on another user, it properly creates a Friendship entity"
            ++ "and a FriendshipLog with the correct action") $ do
            (Entity yourId _, Entity theirId _) <- createRequest

            (Entity _ Friendship{..}) <- runDB $
                getUniqueFriendship yourId theirId

            assertEqual ("friendship created has the correct references to the"
                ++ "users")
                (friendshipFirstUser, friendshipSecondUser)
                (yourId             , theirId             )

            assertEqual ("friendship created is not active, and is set as a"
                ++ "request")
                (friendshipIsActive, friendshipIsRequest)
                (False             , True               )

            mlog <- runDB $ do
                fid <- getUniqueFriendshipId yourId theirId
                selectFirst
                    [ FriendshipLogFriendship ==. fid
                    , FriendshipLogAction ==. CreateRequest ]
                    []

            assertEqual "a log recording the CreateRequest action was created"
                (isJust mlog)
                True

        it ("gives a 200 when sending another request after cancelling the"
            ++ "previous one") $ do
            (_, Entity theirId _) <- createRequest

            postJson (CancelFriendRequestR theirId)
            statusIs 200

            postJson (CreateFriendRequestR theirId)
            statusIs 200

        it "won't let you send more than 3 requests" $ do
            (Entity _ you) <- createNewUserAndProfileEnt 1
            (Entity theirId _) <- createNewUserAndProfileEnt 2

            authenticateAs you

            -- first
            postJson (CreateFriendRequestR theirId)
            postJson (CancelFriendRequestR theirId)
            -- second
            postJson (CreateFriendRequestR theirId)
            postJson (CancelFriendRequestR theirId)

            -- should be able to create a third request
            postJson (CreateFriendRequestR theirId)
            statusIs 200

            -- should be able to cancel a third time as well
            postJson (CancelFriendRequestR theirId)
            statusIs 200

            -- but creating a fourth request should result in a 500
            postJson (CreateFriendRequestR theirId)
            statusIs 500



    let createAndAcceptRequest = do
        e1@(Entity yourId you) <- createNewUserAndProfileEnt 1
        e2@(Entity theirId them) <- createNewUserAndProfileEnt 2

        authenticateAs you

        postJson (CreateFriendRequestR theirId)

        authenticateAs them

        postJson (AcceptFriendRequestR yourId)

        return (e1, e2)

    describe "postAcceptFriendRequestR" $ do

        it "gives a 303 when receiver is not authenticated" $ do
            (yourId, you) <- createNewUserAndProfile 1
            (theirId, _) <- createNewUserAndProfile 2

            authenticateAs you

            postJson (CreateFriendRequestR theirId)

            logout

            postJson (AcceptFriendRequestR yourId)


        it "gives a 500 when no request exists" $ do
            (_, you) <- createNewUserAndProfile 1
            (theirId, _) <- createNewUserAndProfile 2

            authenticateAs you

            postJson (AcceptFriendRequestR theirId)

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

            postJson (CreateFriendRequestR theirId)

            return (e1, e2)

        it "gives a 500 when no request exists" $ do
            (_, you) <- createNewUserAndProfile 1
            (theirId, _) <- createNewUserAndProfile 2

            authenticateAs you

            postJson (CancelFriendRequestR theirId)

            statusIs 500

        it ("correctly cancels a request you sent, including logging that it"
            ++ "did so") $ do
            (Entity yourId _, Entity theirId _) <- createRequest

            postJson (CancelFriendRequestR theirId)
            statusIs 200

            (Friendship{..}, logs) <- runDB $ do
                (Entity fid fe) <-
                    getUniqueFriendship yourId theirId

                logs <- map (friendshipLogAction . entityVal) <$> selectList
                    [ FriendshipLogFriendship ==. fid
                    , FriendshipLogAction ==. CancelRequest ] []

                return (fe, logs)

            assertEqual "cancel request was logged"
                logs
                [CancelRequest]

            assertEqual "cancel request properly modified the friendship entity"
                (friendshipIsActive, friendshipIsRequest)
                (False             , False              )

    describe "postDefriendR" $ do

{-
        ("Integration: creates a friendship, then defriends, followed by "
            ++ " refriending, and defriending again, but as the second user."
            ++ "\n\n If all of these return 200s, then check the logs to make"
            ++ " sure"
            ++ " that there is one log for each of these actions, six in total."
            ++ "\n\n Finally, ensure that the current friendship entity"
            ++ " indicates that they are no longer friends." )

            -}

        it ([r|
    Integration: creates a friendship, then defriends, followed by refriending,
    and defriending again, but as the second user.

    If all of these return 200s, then check the logs to make sure that there is
    one log for each of these actions, six in total.

    Finally, ensure that the current friendship entity indicates that they are
    no longer friends.|]) $ do
            (Entity yourId you, Entity theirId them) <- createAndAcceptRequest

            authenticateAs you

            postJson (DefriendR theirId)
            statusIs 200

            -- become friends again
            postJson (CreateFriendRequestR theirId)
            authenticateAs them
            postJson (AcceptFriendRequestR yourId)
            statusIs 200

            -- defriend again, but this time as User2
            postJson (DefriendR yourId)
            statusIs 200

            -- check logs to make sure two create, two accept, and two defriend
            -- requests were created

            (Friendship{..}, logs) <- runDB $ do
                (Entity fid fe) <-
                    getUniqueFriendship yourId theirId

                logs <- selectList
                    ([ FriendshipLogFriendship ==. fid ]
                    ++ ([ FriendshipLogAction ==. Defriend ]
                    ||. [ FriendshipLogAction ==. CreateRequest ]
                    ||. [ FriendshipLogAction ==. AcceptRequest ])) []

                return (fe, logs)

            assertEqual "there are six log entities"
                (length logs)
                6

            assertEqual ("the latest friendship entity correctly indicates that"
                ++ "these users are no longer friends, and that there is no"
                ++ "active request")
                (friendshipIsActive, friendshipIsRequest)
                (False             , False              )
