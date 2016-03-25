# Friendship

## Friend Requests

Facebook has a pretty good model for how friend requests work, and that's the
implementation I'll be sticking with. The sender of a request can cancel it at
any time, at which point either party may resend a request.

Friend Requests can be ignored by the receiver, but doing so doesn't cancel
them, as ignoring is primarily a client-side UI operation, informed by flags on
the server-side (likely in the Notification entity itself). Instead, all
requests have a timeout, at which point a request is cancelled, and can be sent
again. The sender of a request is never notified when their request is ignored,
and it appears as if their request is still pending, at least until the timeout
is triggered.
