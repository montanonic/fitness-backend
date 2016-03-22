Friendrequest functions don't need to have the first UserId (YourId) given,
because requireAuthId/maybeAuthId can handle that logic. That said, doing it
that way puts an implementation burden upon how authentication works, which is
an unknown, given the lack of integration with mobile clients currently.

Therefore, it would cost very little to defer that key-lookup to the mobile
client, instead of leaving that logic here in the server. At least, unless/until
this becomes a website as well.

We'll see how things go; it shouldn't be too costly a change to make either way.
