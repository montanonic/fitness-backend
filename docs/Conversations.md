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
