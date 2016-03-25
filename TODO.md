Update Friendships and FriendRequests to encompass more actions, and beef up
testing.

Rewrite Model code to rely upon DB (Either ModelException Result) types, and
move away from code that doesn't force error handling. (On second thought, maybe
stick to how things are currently being done until it looks like we can do
better?)

Notification system:

* getLastCheckedNotificationsR userId = ...

Rewrite any hefty queries to use more efficient data types and potentially the
Conduit library as well.
