module Model.Conversation
    ( module Model.Conversation
    ) where

import Import

import EsqueletoLocal
import Model.Persistent (ConversationAction(..))


data ConversationException
    = NotMemberOfConversation
    | AlreadyMemberOfConversation
instance Show ConversationException where
    show NotMemberOfConversation = "You are not currently a member of this\
        \ conversation. This can occur if you either have never been a member\
        \ of it, or if you were once a member, but have since left the\
        \ conversation."
    show AlreadyMemberOfConversation = "The user is already a member of the\
        \ conversation you are trying to add them to."
instance Exception ConversationException

--
-- ## Conversations
--

-- | This function is used to initiate a new conversation, tying participants
-- and messages to a new Conversation entity.
createNewConversation :: UTCTime -> UserId -> [UserId] -> Text
    -> DB ConversationId
createNewConversation now you them msgContent = do
    cid <- insert $ Conversation now
    -- log that a new conversation was created and who created it
    insert_ $ ConversationLog cid you CreateNewConversation Nothing now
    -- add the initial participants of the conversation to the conversation:
    --
    -- NOTE: each ConversationUser must be unique in terms of cid and uid. if
    -- not, a 500 will be thrown. anticipating this, I will sort and filter the
    -- input list of users to ensure that there are no duplicates, which should
    -- not have much a performance penalty since conversations should rarely be
    -- between large #'s of users.
    let noDups = map headEx . group . sort $ you:them
    insertMany_ $ (\u -> ConversationUser cid u True now) <$> noDups
    -- because all the users are inserted into the conversation at the same
    -- time, we do not need to create logs to query who the initial conversation
    -- participants were.
    addMessage now cid you msgContent
    return cid

-- Updates the Conversation entity to reflect that there is a new message or
-- user. This information is useful for client-side code, as it allows the
-- sorting of conversations by when they were last active, rather than when they
-- were created.
updateConversation :: UTCTime -> ConversationId -> DB ()
updateConversation now cid = update cid [ ConversationLastUpdated =. now ]

-- | #TODO: Update to allow for pagination, to reduce network usage.
--
-- Get all conversations that the given User is currently active in, in order
-- from when the conversation last had activity.
getConversations :: UserId -> DB [ConversationId]
getConversations you = map (unValue <$>) $
    select $ from $ \(cu `InnerJoin` c) -> do
        on_ $ cu ^. ConversationUserConversation .==. c ^. ConversationId
        where_ $ cu ^. ConversationUserUser .==. val you
            &&. cu ^. ConversationUserIsActive .==. val True
        orderBy [desc $ c ^. ConversationLastUpdated]
        return $ c ^. ConversationId

--
-- ## ConversationMessages
--

-- | Add a message to an existing conversation if you are an active member of
-- that conversation.
addMessage :: UTCTime -> ConversationId -> UserId -> Text -> DB ()
addMessage now cid you content = do
    res <- isParticipantOfConversation cid you
    if res
        then do
            insert_ $ ConversationMessage cid content you now
            updateConversation now cid
        else throwM NotMemberOfConversation

getConversationMessages :: UserId -> ConversationId
    -> DB [ConversationMessage]
getConversationMessages you cid = do
    unlessM (isParticipantOfConversation cid you)
        $ throwM NotMemberOfConversation
    map entityVal <$> selectList [ ConversationMessageConversation ==. cid ]
                                 [ Desc ConversationMessageCreatedAt ]

--
-- ## ConversationUsers
--

-- | Does not include past participants of the conversation (those that are
-- currently flagged as inactive for a particular conversation).
queryConversationUsers :: ConversationId -> DB [Entity ConversationUser]
queryConversationUsers cid =
    selectList [ ConversationUserConversation ==. cid
               , ConversationUserIsActive ==. True ] []

conversationUsersToIds :: [Entity ConversationUser] -> [UserId]
conversationUsersToIds userEnts = conversationUserUser . entityVal <$> userEnts

queryConversationUserIds :: ConversationId -> DB [UserId]
queryConversationUserIds cid = queryConversationUsers cid
    >>= return . conversationUsersToIds

-- | Tells us if the given user is currently a part of the given conversation,
-- which means that they must have `isActive` flagged as `True`, in addition
-- to having an ConversationUser entity referencing the given conversation.
--
-- This function is inefficient if used more than once in any single database
-- action. If attempting to see if more than one user is part of a conversation
-- based upon their UserId, just use `queryConversationUserIds` instead, and
-- don't throw away the list of UserIds.
isParticipantOfConversation :: ConversationId -> UserId -> DB Bool
isParticipantOfConversation cid uid = do
    participants <- queryConversationUserIds cid
    return $ any (==uid) participants

-- | Adds a new user to an existing conversation, or reinvite them if they've
-- gone inactive. The first UserId, the inviter, must already be a member of the
-- conversation for this action to carry out.
addUser :: UTCTime -> ConversationId -> UserId -> UserId -> DB ()
addUser now cid you them = do
    participants <- queryConversationUserIds cid
    -- check if the user you are trying to add is in fact already a part of
    -- the conversation
    when (any (==them) participants) $ throwM AlreadyMemberOfConversation
    if any (==you) participants
        then do
            -- add them to the conversation and log this event
            eValOrKey <- insertBy $ ConversationUser cid them True now
            case eValOrKey of
                Right _ -> insert_ $
                    ConversationLog cid you AddNewConversationUser (Just them)
                        now
                -- if they were previously a member of the conversation, but
                -- since went inactive, we update their entity instead
                Left (Entity cuid _) -> do
                    update cuid [ ConversationUserIsActive =. True ]
                    -- and log the event as a reinvitation instead of a new
                    -- addition
                    insert_ $
                        ConversationLog cid you ReinviteConversationUser
                            (Just them) now
                    updateConversation now cid
        else throwM NotMemberOfConversation
