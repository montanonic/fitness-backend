# NOTIFICATIONS

What are notifications in a database? Well, without Push Notifications, a
server+database can only service *requests*, and cannot perform requests on
behalf of a client. This means that if a client does not ask the server to do
something, the server won't do *anything*.

So, if we can only service requests, we need to rely upon the client to know
when to ask us. From the perspective of the client, they'll be creating a friend
request by sending a POST request to the `/friendRequest/#UserId/create` route.
That route in turn will get the Id of the current user (the one logged-in to the
client), and create a Friendship entity referencing the Id of the user from the
url, and flagged as FriendshipIsJustRequest.

Now, a friendship request can then be accepted by the second user, which just
flags the entity as active, and gets rid of the FriendshipIsJustRequest flag.
The problem to solve is to ensure that they know that user1 sent them a request,
when all that user1 *actually* did was create a new data entity (and a log of
them sending a request). How do we get the awareness of that Friendship entity's
creation sent to user2?

## Proposed system

The Notification Entity will have foreign key reference to a user that the
notification belongs to. Furthermore, it will state what the notification was
for (through a new ADT in Model.Persistent), if it has been read, and when the
notification was created (of course).

In this case, our User2's newest Notification entity would state that it was a
FriendRequest, and contain the UserId of user who sent it (which the client
would probably use to pull in the profile picture of that user). So, once the
client checked User2's notifications, they'd know they had one. So now the final
question is to consider when the client can know to check for notifications, or
if it just has to rely on long-polling, querying for new entries in the
Notification table ever set number of seconds.

The answer: Push Notifications, or long polling. Redis may be a possible
alternative as well, but if AWS's Push Notification system is easy to use, then
that will likely be the easiest route.

## One Notification table, or many?

So, one caveat with Persistent is that any custom ADTs that you want to use with
it have to be created before Persistent generates the models, meaning that
custom ADTs can't have references to things like UserId in them. This is a fair
constraint, given that this could easily create a Foreign Key headache.

Because of this, if we have Notification types that involve different FK ents,
such as the difference between a FriendRequest, which relies upon UserId, and
a ConversationUpdate, which relies upon a ConversationId (variable number of
users, and can be changed at any point, dynamically, so we need to rely upon its
junction table to find the participants), then we'll need to either add those
possible FKs as nullable fields in the Notification entity, or have a separate
Notification entity for each type.

The issue with having a Notification entity for each type of notification comes
up when we want to update a notification through long polling, as now we must
either create yet another table, a junction between users and the different
notification entities, where we can keep track of the existence of any and all
updates, no matter the type, or we must poll multiple separate entities at once
for changes. Alternatively: a single entity, with multiple fields, each of which
is nullable, and only one of which should not be null for any given entity.

A single entity with nullables vs. a Junction table + specific notification
entities is the question then, and because we'll have to filter for notification
types in any case (wall updates should display differently than friendship
requests, which should display differently than message updates), then we might
as well go for a Junction table + multiple entities, as whatever cost we accrue
during the storage stage, we make up for when it comes to querying power, and
all the while retaining the ability to long-poll cheaply.

Furthermore, with this scheme, anything that may replace long-polling in favor
of a server-sent event (push notification) would eliminate the need for a
junction table, though not for individualized notification entities. So it's
abundantly clear that this is the correct way to do things.
