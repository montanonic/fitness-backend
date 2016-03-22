CONSIDER switching uses of UserId over to ProfileId. Both can be used to
uniquely identify a user, the only current difference has to do with how we
handle the privacy. The User entity is obviously necessary for logging-in, and
matters such as that. But Profile is the public-facing entity, and it may simply
make more sense to use a ProfileId as the foreign key for public-facing entities
such as Friendships.

Clearly Profile and Friendships will have privacy settings, but that is
fundamentally different from information that *should never be shared*, such as
those in the User entity, save for their email (only at their discretion).

All of this said, the only real loss to sticking with UserIds as foreign keys
for all entities is the need for another lookup to get entities based upon
ProfileId, but a user's ProfileId value can easily be cached on the client and
retrieved at no greater cost than their UserId. So honestly, none of the above
seems necessary, and it is conceptually easier to understand a User as the link
to other entities than Profile as being a PublicUser entity.
