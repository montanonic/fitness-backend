# Conversations

Implementation should be similar to how IRC chat works, except with an
invitation-only scheme. So, for an example: I message (open up a conversation
with) a friend, then they message me back, and we chat for a while. Then, I
choose to invite a third friend (whether or not we allow arbitrary adding of
people in conversations without the consent of the other users is something to
consider, though it isn't priority now). This third friend *should not* see what
we had talked about before, but we get to see it, and anything after it. If I
leave the conversation, then the conversation will be marked as inactive, and I
no longer receive notifications for it, though I retain the history of it.

From this point, I may request to be a part of the conversation again, and upon
rejoining (invitation accepted), I will have the previous history, and whatever
is added from this point, but not what happened in between. I wasn't present,
and therefore, could not "hear" what happened.


## Permissions with adding new users to a conversation

The current implementation makes it so that any active member of a conversation
can invite other people to it (we might want to make this friends-only?). This
is a very simplistic system, and we can expand upon it in the future when we
decide it is too simple.

## You can create multiple conversations that include the same users.

So, currently, you're able create multiple identical conversations with an
identical set of recipients. We may want to create the more traditional behavior
where any new conversation that includes the same set of users will get rolled
into the existing conversation instead of creating a new one.

That said, this free-er model does allow for some potentially desirable
creations, and if we just imitated what texting already does perhaps we'd be
missing an opportunity here?

Until I get a word from Denis or Kevin on how we want conversations to behave,
I'm going to leave it as it is right now.
